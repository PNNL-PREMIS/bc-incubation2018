# 3-clean_picarro_data.R

# This is just a thin wrapper around two calls to picarro.data package functions
clean_picarro_data <- function(prd) {
  message("Welcome to clean_picarro_data")
  MAX_TIME <- 180
  message("Filter to Elapsed_seconds < ", MAX_TIME)
  
  prd %>% 
    clean_data(tz = "UTC") %>% 
    assign_sample_numbers() %>% 
    filter(Elapsed_seconds < MAX_TIME)
}

# Match the Picarro data (pd) with associated entries in the valve_key file.
# We do this by finding rows in pd that have the same valve number and whose timestamps
# fall within the range specified in the valve_key
match_picarro_data <- function(pd, valve_key) {
  message("Welcome to match_picarro_data")
  if(is.null(pd)) return(NULL)
  
  pd_match_count <- rep(0L, nrow(pd))
  valve_key_match_count <- rep(0L, nrow(valve_key))
  
  results <- list()
  for(i in seq_len(nrow(valve_key))) {
    # find matches based on timestamp
    matches <- with(pd, DATETIME >= valve_key$TIMESTAMP_START[i] & 
                      DATETIME <= valve_key$TIMESTAMP_STOP[i] &
                      MPVPosition == valve_key$Valve[i])
    
    # update match count for each record in each dataset
    pd_match_count[matches] <- pd_match_count[matches] + 1L
    valve_key_match_count[i] <- sum(matches)
    
    # take those records from the Picarro data, record core, save
    pd[matches,] %>% 
      mutate(Core = valve_key$Core[i]) ->
      results[[i]]
  }
  
  # Return the Picarro data with new 'Core' column, and counts of how many times each
  # data row was matched (should be 1) and how many rows each valve_key entry matched
  list(pd = bind_rows(results), pmc = pd_match_count, vkmc = valve_key_match_count)
}

# Check for problems in the match process
qc_match <- function(p_clean, p_clean_matched, valve_key, p_match_count, valve_key_match_count) {
  vkmc <- sum(valve_key_match_count > 0)
  message(vkmc, " of ", length(valve_key_match_count), " valve key entries were matched")
  if(any(valve_key_match_count == 0)) {
    warning("Some valve key entries were not matched")
  }
  message(sum(p_match_count > 0), " of ", length(p_match_count), " Picarro data entries were matched")
  pmc1 <- sum(p_match_count > 1)
  if(pmc1) {
    warning(pmc1, " Picarro data entries were matched more than once")
  }
  
  p <- ggplot(p_clean, aes(DATETIME, MPVPosition, color = p_match_count)) + geom_point()
  ggsave("outputs/qc_match.png", plot = p)
}

# Plot concentrations
qc_plot_concentrations <- function(p_clean_matched, valve_key, ELAPSED_SECONDS_MAX) {
  p_co2 <- ggplot(p_clean_matched, aes(Elapsed_seconds, CO2_dry, group = Sample_number)) + 
    geom_line(alpha = 0.5) + 
    facet_wrap(~Core, scales = "free_y") +
    geom_vline(xintercept = ELAPSED_SECONDS_MAX, color = "red") +
    theme(axis.text.y = element_blank(), strip.text = element_text(size = 6))
  ggsave("outputs/qc_co2.pdf", plot = p_co2, height = 8, width = 8)
  
  p_ch4 <- ggplot(p_clean_matched, aes(Elapsed_seconds, CH4_dry, group = Sample_number)) + 
    geom_line(alpha = 0.5) + 
    facet_wrap(~Core, scales = "free_y") +
    geom_vline(xintercept = ELAPSED_SECONDS_MAX, color = "red") +
    theme(axis.text.y = element_blank(), strip.text = element_text(size = 6))
  ggsave("outputs/qc_ch4.pdf", plot = p_ch4)
}

compute_ghg_fluxes <- function(p_clean_matched, valve_key, core_dryweights) {
  message("Welcome to compute_fluxes")
  
  # The instrument tubing is 455 cm long by ID 1/16"
  V_tubing <- (1/16 * 2.54 / 2 ) ^ 2 * pi * 455
  # Internal volume of Picarro? 
  V_picarro <- 9 # Assume same as PP-Systems
  
  Tair = 21  # per Kaizad
  
  p_clean_matched %>% 
    group_by(Sample_number) %>% 
    filter(n() > 1) %>% 
    group_by(Core, Sample_number) %>% 
    summarise(DATETIME = mean(DATETIME),
              n = n(),
              flux_co2_umol_s = compute_flux(Elapsed_seconds, 
                                             CO2_dry, 
                                             volume_cm3 = V_tubing + V_picarro, 
                                             tair_C = Tair),
              flux_ch4_nmol_s = compute_flux(Elapsed_seconds, 
                                             CH4_dry / 1000,  # in ppb not ppm 
                                             volume_cm3 = V_tubing + V_picarro, 
                                             tair_C = Tair) * 1000) %>% 
    # join with valve_key data to get dry weights
    left_join(core_dryweights, by = "Core") %>% 
    group_by(Core, Sample_number, DATETIME) %>% 
    summarise(flux_co2_umol_s = flux_co2_umol_s,
              flux_ch4_nmol_s = flux_ch4_nmol_s,
              flux_co2_umol_g_s = flux_co2_umol_s / Dry_weight_g,
              flux_ch4_nmol_g_s = flux_ch4_nmol_s / Dry_weight_g) %>% 
    ungroup()
}

qc_fluxes <- function(ghg_fluxes, valve_key) {
  ghg_fluxes %>%
    separate(Core, into = c("grp","cor"), sep = "-", fill = "right") %>%
    filter(grepl("^BC", grp)) ->
    gf
  
  p_co2 <- ggplot(gf, aes(DATETIME, flux_co2_umol_g_s, group = cor, color = cor)) + 
    geom_point() + geom_line() +
    facet_wrap(~grp, scale = "free_y")
  ggsave("outputs/fluxes_co2.pdf", plot = p_co2, width = 8, height = 6)
  p_ch4 <- ggplot(gf, aes(DATETIME, flux_ch4_nmol_g_s, group = cor, color = cor)) + 
    geom_point() + geom_line() +
    facet_wrap(~grp, scale = "free_y")
  ggsave("outputs/fluxes_ch4.pdf", plot = p_ch4, width = 8, height = 6)
}

do_flux_summary <- function(ghg_fluxes, inundations, site_categories) {
  # Average/median and summary statistics of flux per site temporally or at the end of the experiment
  ghg_fluxes %>% 
    separate(Core, into = c("Site", "Site_core"), remove = FALSE, sep = "-") %>% 
    left_join(inundations, by = "Core") %>% 
    left_join(site_categories, by = "Site") %>% 
    filter(!is.na(flux_co2_umol_g_s)) ->
    ghgf
  
  ghgf %>% 
    filter(!is.na(Inundation1)) %>% 
    mutate(Inundation = 1, Inundation_dttm = Inundation1) ->
    ghgf_i1
  ghgf %>% 
    filter(!is.na(Inundation2)) %>% 
    mutate(Inundation = 2, Inundation_dttm = Inundation2) ->
    ghgf_i2
  ghgf %>% 
    filter(!is.na(Inundation3)) %>% 
    mutate(Inundation = 3, Inundation_dttm = Inundation3) %>% 
    bind_rows(ghgf_i1, ghgf_i2) %>% 
    mutate(Inundation = as.factor(Inundation)) %>% 
    select(-Inundation1, -Inundation2, -Inundation3) ->
    ghgf_inundations
  
  p <- ggplot(ghgf_inundations, aes(DATETIME, flux_co2_umol_g_s)) + 
    geom_point() + 
    geom_vline(aes(xintercept = Inundation_dttm, color = Inundation)) + 
    geom_vline(aes(xintercept = Inundation_dttm + 60 * 60 * 24, color = Inundation), linetype = 2) +
    facet_wrap(~Core, scales = "free_y")
  ggsave("outputs/inundations_qc.png", plot = p)
  
  ghgf %>% 
    group_by(Proximity_to_creek, Site, Treatment, Core) %>% 
    summarise(co2_mean = mean(flux_co2_umol_g_s),
              co2_median = median(flux_co2_umol_g_s),
              ch4_mean = mean(flux_ch4_nmol_g_s),
              ch4_median = median(flux_ch4_nmol_g_s)) %>% 
    tidyr::gather(statistic, value, co2_mean, co2_median, ch4_mean, ch4_median) ->
    ghgf_means
  
  p <- ggplot(filter(ghgf_means, 
                     statistic %in% c("co2_mean", "co2_median"),
                     Treatment != "Pre-inundationControl"),
              aes(Treatment, value)) + 
    geom_boxplot(color = "lightgrey") +
    geom_point() + 
    ylab(expression(CO[2]~(µmol~g^{-1}~s^{-1}))) +
    facet_grid(statistic ~ Proximity_to_creek + Site) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave("outputs/inundations_meanmedian_co2.png", plot = p)
  
  p <- ggplot(filter(ghgf_means, 
                     Treatment != "Pre-inundationControl",
                     statistic %in% c("ch4_mean", "ch4_median")),
              aes(Treatment, value)) + 
    geom_boxplot(color = "lightgrey") +
    geom_point() + 
    ylab(expression(CH[4]~(nmol~g^{-1}~s^{-1}))) +
    facet_grid(statistic ~ Proximity_to_creek + Site) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave("outputs/inundations_meanmedian_ch4.png", plot = p)
  
  write_csv(ghgf_means, "outputs/ghgf_means.csv")
  
  #  browser()
}
