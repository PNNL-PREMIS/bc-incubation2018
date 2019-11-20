# 1-read_files.R

read_core_key <- function(filename) {
  readr::read_csv(filename, col_types = "cccccc") %>%
    dplyr::select(CoreID, Treatment, Site, `Salinity Transect`, LanscapePosition)
}

read_core_wetweights <- function(filename) {
  readr::read_csv(filename, col_types = "cccd", na = "#N/A") %>% 
    dplyr::select(Core, Wet_weight_g = `Soil wet weight (g)`)
}

read_valve_key <- function(filename) {
  readr::read_csv(filename, col_types = "cccdddc") %>%
    dplyr::select(Core, Date, Time, Run, Headspace, Valve) %>% 
    separate(Time, into = c("start","stop"), sep = "-") %>% 
    mutate(TIMESTAMP_START = mdy_hm(paste(Date, start), tz = "America/Los_Angeles"),
           TIMESTAMP_STOP = mdy_hm(paste(Date, stop), tz = "America/Los_Angeles")) %>% 
    select(-start, -stop)
}
