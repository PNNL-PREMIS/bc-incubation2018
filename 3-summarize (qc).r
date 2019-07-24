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
  ungroup %>% 
  filter(!MPVPosition %in% AMBIENT_VALVE) %>% 
  # group_by(samplenum) %>%
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
  separate(Time, into = c("Start_time", "Stop_time"), sep = "-") %>% 
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
  distinct() %>% 
  mutate(Date = mdy(Date)) ->
  wetweights


# ------------ Matching -------------------
# Match Picarro data to valvemap data

printlog(SEPARATOR)
printlog("Matching valvemap to Picarro data (slow)...")
rawdata_matched <- list()
valvemap$Matches <- NA_real_
rawdata_samples$Matches <- 0
for(i in seq_len(nrow(valvemap))) {
  if(!i %% 100) print(i)
  
  wch <- which(rawdata_samples$MPVPosition == valvemap$Valve[i] &
                 rawdata_samples$DATETIME >= valvemap$Picarro_start[i] &
                 rawdata_samples$DATETIME <= valvemap$Picarro_stop[i])
  x <- rawdata_samples[wch,]
  valvemap$Matches[i] <- nrow(x)
  rawdata_samples$Matches[wch] <- rawdata_samples$Matches[wch] + 1
  
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

no_data_matches <- filter(valvemap, Matches == 0) %>% arrange(rownum)
printlog(nrow(no_data_matches), "valvemap entries had no data matches")
save_data(no_data_matches)

no_valvemap_matches <- filter(rawdata_samples, Matches == 0)
printlog(nrow(no_valvemap_matches), "data entries had no valvemap matches")
save_data(no_valvemap_matches)

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
  fluxdata

# Merge with wet weight data (for now) and normalize fluxes by that

fluxdata %>% 
  mutate(Date = as.Date(DATETIME)) %>% 
  left_join(wetweights, by = c("Core", "Date")) %>% 
  mutate(CO2_flux_norm = CO2_flux / `Soil wet weight (g)`,
         CH4_flux_norm = CH4_flux / `Soil wet weight (g)`) ->
  fluxdata

p <- ggplot(fluxdata, aes(DATETIME, CO2_flux_norm, color = Core)) + 
  geom_line() +
  ylab("CO2 flux (µmol/g soil/s")
print(p)
save_plot("CO2")
p <- ggplot(fluxdata, aes(DATETIME, CH4_flux_norm, color = Core)) + 
  geom_line() +
  ylab("CH4 flux (nmol/g soil/s")
print(p)
save_plot("CO2")

# Some cores have 0 or 1 valid flux values; remove these

fluxdata %>%
  group_by(Core) %>%
  mutate(goodvals = sum(!is.na(CO2_flux_norm))) -> 
  fluxdata

fewdata <- filter(fluxdata, goodvals < 2)
if(any(nrow(fewdata))) {
  printlog(length(unique(fewdata$Core)), "cores have fewer than 2 good flux values")
  save_data(fewdata)
}

# Cumulative fluxes over time

fluxdata %>% 
  arrange(Core, DATETIME) %>% 
  filter(goodvals > 1) %>% 
  select(Core, DATETIME, CO2_flux_norm, CH4_flux_norm) %>% 
  group_by(Core) %>% 
  mutate(inctime_hrs = as.numeric(difftime(DATETIME, first(DATETIME), units = "hours")),
         inctime_hrs = round(inctime_hrs, 2),
         deltatime = c(0, as.numeric(difftime(tail(DATETIME, -1), head(DATETIME, -1), units = "sec"))),
         CO2_flux_interp = approx(deltatime, CO2_flux_norm, xout = deltatime, rule = 2)[['y']],
         CH4_flux_interp = approx(deltatime, CH4_flux_norm, xout = deltatime, rule = 2)[['y']]) %>%
  group_by(Core) %>%
  # Go from µmol/s to mgC
  mutate(cumCO2_mgC = c(0, cumsum(CO2_flux_interp[-1] / 1e6 * 12 * deltatime[-1])),
         cumCH4_mgC = c(0, cumsum(CH4_flux_interp[-1]) / 1e6 * 12 * deltatime[-1])) ->
  fluxdata_cum

p <- ggplot(fluxdata_cum, aes(inctime_hrs, cumCO2_mgC, color = Core)) + 
  geom_line() +
  xlab("Incubation time (hrs)") + ylab("Cumulative CO2 flux (mgC)")
print(p)
save_plot("CO2_cum", width = 10, height = 4)
p <- ggplot(fluxdata_cum, aes(inctime_hrs, cumCH4_mgC, color = Core)) + 
  geom_line() +
  xlab("Incubation time (hrs)") + ylab("Cumulative CH4 flux (mgC)")
print(p)
save_plot("CH4_cum", width = 10, height = 4)

save_data(fluxdata_cum)

fluxdata_cum %>% 
  group_by(Core) %>% 
  filter(DATETIME == max(DATETIME)) %>% 
  select(Core, inctime_hrs, cumCO2_mgC, cumCH4_mgC) ->
  fluxdata_cum_final

save_data(fluxdata_cum_final)

closelog()
cat("All done.\n")
