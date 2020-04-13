# 1-read_files.R

read_core_key <- function(filename) {
  readr::read_csv(filename, col_types = "cccccc") %>%
    dplyr::select(CoreID, Treatment, Site, `Salinity Transect`, LanscapePosition)
}

read_core_wetweights <- function(filename) {
  readr::read_csv(filename, col_types = "cccd", na = "#N/A") %>% 
    dplyr::select(Core, Wet_weight_g = `Soil wet weight (g)`)
}

read_core_dryweights <- function(filename) {
  readr::read_csv(filename, col_types = "cddd") %>% 
    dplyr::select(Core, Dry_weight_g = `Soil_drywright(g)`)
}

read_valve_key <- function(filename) {
  readr::read_csv(filename, col_types = "cccdddc") %>%
    dplyr::select(Core, Date, Time, Run, Headspace, Valve) %>% 
    separate(Time, into = c("start","stop"), sep = "-") %>% 
    mutate(TIMESTAMP_START = mdy_hm(paste(Date, start), tz = "America/Los_Angeles"),
           TIMESTAMP_STOP = mdy_hm(paste(Date, stop), tz = "America/Los_Angeles")) %>% 
    select(-start, -stop)
}

read_inundation_data <- function(filename) {
  readr::read_csv(filename, col_types = "ccccccccc") %>% 
    mutate(Harvested = mdy(Harvested),
           Inundation1 = mdy_hm(paste(Date_Inund1, Time_Inund1), tz = "America/Los_Angeles"),
           Inundation2 = mdy_hm(paste(Date_Inund2, Time_Inund2), tz = "America/Los_Angeles"),
           Inundation3 = mdy_hm(paste(Date_Inund3, Time_Inund3), tz = "America/Los_Angeles"),
           Treatment = factor(Treatment, levels = c("ControlCore", "Pre-inundationControl", "InundatedCore"))) %>% 
    select(Core, Treatment, Harvested, Inundation1, Inundation2, Inundation3)
}

read_site_categories <- function(filename) {
  readr::read_csv(filename, col_types = "ccc") %>% 
    mutate(Proximity_to_creek = factor(Proximity_to_creek, levels = c("Close", "Medium", "Far", "Very far")))
}
