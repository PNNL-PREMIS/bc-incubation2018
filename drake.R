
## packages ####
library(readxl)
library(ggplot2)       # 2.1.0
library(readr)         # 1.0.0
library(lubridate)     # 1.6.0
library(tidyr)
library(readr)
library(ggplot2)
library(dplyr)         
library(drake)
pkgconfig::set_config("drake::strings_in_dots" = "literals")
library(readxl)

# My 'picarro.data' package isn't on CRAN (yet) so need to install it via:
# devtools::install_github("PNNL-TES/picarro.data")
library(picarro.data)

source("1-read_files.R")
source("3-picarro_data.R")

library(drake)

plan <- drake_plan(
  # Metadata
  core_key = read_core_key(file_in("data/Core_key.xlsx")),
  core_dry_weights = read_core_dryweights(file_in("data/Core_weights.xlsx"), sheet = "initial"),
  core_masses = read_core_masses(file_in("data/Core_weights.xlsx"),
                                 sheet = "Mass_tracking", core_key, core_dry_weights),
  valve_key = filter(core_masses, Seq.Program == "CPCRW_SFDec2018.seq"),
  
  # Picarro data
  # Using the 'trigger' argument below means we only re-read the Picarro raw
  # data when necessary, i.e. when the files change
  picarro_raw = target(process_directory("data/picarro/"),
                       trigger = trigger(change = list.files("data/picarro/", pattern = "dat$", recursive = TRUE))),
  picarro_clean = clean_picarro_data(picarro_raw),
  
  # Match Picarro data with the valve key data
  pcm = match_picarro_data(picarro_clean, valve_key),
  picarro_clean_matched = pcm$pd,
  picarro_match_count = pcm$pmc,
  valve_key_match_count = pcm$vkmc,
  
  qc1 = qc_match(picarro_clean, picarro_clean_matched, valve_key, picarro_match_count, valve_key_match_count),
  qc2 = qc_concentrations(picarro_clean_matched, valve_key),
  
  ghg_fluxes = compute_ghg_fluxes(picarro_clean_matched, valve_key),
  qc3 = qc_fluxes(ghg_fluxes, valve_key)
)
message("Now type make(plan)")
