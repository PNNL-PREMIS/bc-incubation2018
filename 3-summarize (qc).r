# Process Picarro data for 3soils lab experiment
# This workhorse script summarizes individual (raw) Picarro observations to 
# summaries of "samples" (groups of consecutive observations made from a given 
# core at a point in time). It computes gas concentration changes, performs 
# some QC, merges the Picarro data with valve map and other ancillary data,
# and writes SUMMARYDATA_FILE.
# 
# Ben Bond-Lamberty November 2017

source("0-functions.R")

SCRIPTNAME  	<- "3-summarize.R"
PROBLEM       <- FALSE


# ==============================================================================
# Main 

openlog(file.path(outputdir(), paste0(SCRIPTNAME, ".log.txt")), sink = TRUE) # open log

printlog("Welcome to", SCRIPTNAME)

printlog("Reading in raw data...")
read_csv(RAWDATA_FILE, col_types = "cccidddd") %>%
  # Convert date/time to POSIXct
  mutate(DATETIME = ymd_hms(paste(DATE, TIME))) %>%
  select(-DATE, -TIME) %>%
  arrange(DATETIME) %>%
  print_dims("rawdata") ->
  rawdata
print(summary(rawdata))
printlog("First timestamp:")
print(min(rawdata$DATETIME))
printlog("Last timestamp:")
print(max(rawdata$DATETIME))

# ------------ Prep work -------------------
# Data cleaning, dates, sample numbers, elapsed time

# Assign a different sample number to each sample group 
# (we know we're on a new sample when MPVPosition changes)
printlog("Assigning sample numbers and computing elapsed time...")
rawdata %>%
  mutate(newsample = MPVPosition != lag(MPVPosition)) %>%
  replace_na(list(newsample = FALSE)) %>% 
  mutate(samplenum = cumsum(newsample)) %>%
  select(-newsample) %>%
  group_by(samplenum) %>%
  mutate(elapsed_seconds = as.double(difftime(DATETIME, min(DATETIME), units = "secs"))) ->
  rawdata_samples

printlog("Removing ambient and bad (valve 2) samples...")
AMBIENT_VALVE <- c(0, 2, 16)
rawdata_samples %>%
  filter(!MPVPosition %in% AMBIENT_VALVE) %>% 
  group_by(samplenum) %>%
  #  filter(max(elapsed_seconds) <= MAX_MEASUREMENT_TIME) %>%
  print_dims("rawdata_samples") ->
  rawdata_samples

printlog("Visualizing...")
p <- ggplot(rawdata_samples, aes(x = elapsed_seconds, color = yday(DATETIME), group = samplenum)) +
  facet_wrap(~MPVPosition, scales = "free_y") + 
  ggtitle("Concentration by date and day of year") +
  xlab("Elapsed time (s)") + scale_color_continuous("Day")
print(p + geom_line(aes(y = CO2_dry)) + xlim(c(0, 60))) + ylab("CO2 (ppm)")
save_plot("co2_by_valve", ptype = ".png")
print(p + geom_line(aes(y = CH4_dry)) + xlim(c(0, 60))) + ylab("CH4 (ppb)")
save_plot("ch4_by_valve", ptype = ".png")


# ------------ Ancillary data -------------------
# Read valvemap and wetweight files

# The 'valvemap' data maps Picarro valve numbers to sample IDs
printlog(SEPARATOR)
printlog("Reading valve and core mapping data...")
read_csv(VALVEMAP_FILE) %>% 
  mutate(rownum = row_number()) %>% 
  filter(!is.na(Core)) %>%
  mutate(Time = gsub("\\.", ":", Time)) %>% 
  separate(Time, into = c("Start_time", "Stop_time"), sep = "-", remove = FALSE) %>% 
  # ADITI SENGUPTA YOU ARE KILLING ME HERE
  # Completely inconsistent recording of times. Arrrgh!
  # Periods vs. colons, AM/PM versus nothing, ".-", ...
  mutate(Start_time = gsub("(A|P)M", "", Start_time)) %>% 
  separate(Start_time, into = c("hr", "min"), sep =":", remove = FALSE, convert = TRUE) %>% 
  mutate(Start_time = paste(Start_time, if_else(hr < 7 | hr > 11, "PM", "AM"))) %>% 
  
  mutate(Stop_time = gsub("(A|P)M", "", Stop_time)) %>% 
  separate(Stop_time, into = c("hr", "min"), sep =":", remove = FALSE, convert = TRUE) %>% 
  mutate(Stop_time = paste(Stop_time, if_else(hr < 7 | hr > 11, "PM", "AM"))) %>% 
  
  mutate(Picarro_start = mdy_hm(paste(Date, Start_time), tz = "America/Los_Angeles"),
         Picarro_stop = mdy_hm(paste(Date, Stop_time), tz = "America/Los_Angeles")) %>% 
  
  # Oh ffs what's going on with the `Core` column here?
  #  separate(Core, into = c("Core", "something_else"), sep = "-") %>% 
  select(Core, Picarro_start, Picarro_stop, Headspace, Valve, rownum) %>% 
  arrange(Picarro_start) ->
  valvemap

# valvemap diagnostic
ggplot(valvemap, aes(Picarro_start, Valve, color = Core)) + 
  geom_point()
save_plot("valvemap", height = 4, width = 8)

# The 'wetweight' data records sample wet weights
read_csv("data/wet_weights.csv", na = "#N/A") %>% 
  mutate(Date = mdy(Date)) ->
  wetweights


# ------------ Matching -------------------
# Match Picarro data to valvemap data

printlog(SEPARATOR)
printlog("Matching valvemap to Picarro data (slow)...")
rawdata_matched <- list()
valvemap$Matches <- NA_real_
for(i in seq_len(nrow(valvemap))) {
  if(!i %% 10) print(i)
  x <- filter(rawdata_samples, 
              MPVPosition == valvemap$Valve[i],
              DATETIME >= valvemap$Picarro_start[i],
              DATETIME <= valvemap$Picarro_stop[i])
  valvemap$Matches[i] <- nrow(x)
  if(valvemap$Matches[i] > 0) {
    x$valvemap_rownum <- valvemap$rownum[i]
    x$Core <- valvemap$Core[i]
    x$Headspace <- valvemap$Headspace[i]
    rawdata_matched[[i]] <- x
  }
}
rawdata_matched <- bind_rows(rawdata_matched)

# Look for overlapping entries
rawdata_matched %>% 
  group_by(samplenum, Core) %>% 
  summarise(len = length(unique(Headspace)), 
            rownums = paste(unique(valvemap_rownum), collapse = ", ")) %>% 
  filter(len > 1) -> 
  overlaps

if(nrow(overlaps)) {
  warning("Overlapping valvemap entries!")
  print(overlaps)
}

no_matches <- filter(valvemap, Matches == 0) %>% arrange(rownum)
printlog(nrow(no_matches), "valvemap entries had no matches")

# ------------ Fluxes -------------------
# Compute fluxes

library(picarro.data)

# The instrument tubing is 455 cm long by ID 1/16"
V_tubing <- (1/16 * 2.54 / 2 ) ^ 2 * pi * 455
# Internal volume of Picarro? 
V_picarro <- 9 # Assume same as PP-Systems

rawdata_matched %>% 
  group_by(samplenum, Core) %>%
  mutate(n = n(),
         V = V_picarro + V_tubing + (7.5 / 2) ^ 2 * pi * Headspace) %>% 
  filter(n > 2, elapsed_seconds < 60) %>% 
  group_by(samplenum, Core) %>% 
  summarise(DATETIME = mean(DATETIME),
            CO2_flux = compute_flux(elapsed_seconds, CO2_dry, 
                                    volume_cm3 = unique(V), 
                                    tair_C = 20, 
                                    pressure_kPa = 101),
            CH4_flux = 1e3 * compute_flux(elapsed_seconds, CH4_dry / 1e3, 
                                          volume_cm3 = unique(V), 
                                          tair_C = 20, 
                                          pressure_kPa = 101)) ->
  summarydata
