# 4-figures.R

aditi_figure1 <- function() {
  
}

aditi_figure2 <- function(ghgf_inundations, ghg_si) {
  ghg_si %>% 
    ungroup() %>%
    filter(Treatment %in% c("ControlCore", "InundatedCore")) %>% 
    mutate(Treatment = if_else(Treatment == "ControlCore", "CC", "IC")) %>% 
    select(Site, Treatment, CO2 = flux_co2_umol_g_s, CH4 = flux_ch4_nmol_g_s) %>% 
    tidyr::gather(Gas, value, CO2, CH4) %>% 
    group_by(Treatment, Gas, Site) %>% 
    summarise(Flux = mean(value), flux_sd = sd(value)) %>% 
    ggplot(aes(Treatment, Flux)) + 
    geom_errorbar(aes(ymin = Flux - flux_sd, ymax = Flux + flux_sd)) +
    facet_grid(Gas ~ Site, scales = "free_y", labeller = label_parsed) + 
    geom_point() +
    theme(strip.background = element_blank(),
          strip.text.y = element_text(angle = 00)) ->
    p
  
  ggsave("outputs/aditi-fig2.png", plot = p)
  
}

# Distribution of change
aditi_figure3 <- function(ghgf_inundations) {
  
  # Compute median control flux
  ghgf_inundations %>% 
    filter(Treatment == "ControlCore") %>% 
    group_by(Site, Inundation) %>% 
    summarise(control_co2 = mean(flux_co2_umol_g_s),
              control_ch4 = mean(flux_ch4_nmol_g_s)) ->
    control_fluxes
  
  # Take just the inundation fluxes in 24 hours and compare to controls
  ghgf_inundations %>% 
    filter(Treatment == "InundatedCore",
           inundation_hrs <= 24) %>% 
    group_by(Site, Core, Inundation) %>% 
    summarise(flux_co2_umol_g_s = mean(flux_co2_umol_g_s),
              flux_ch4_nmol_g_s = mean(flux_ch4_nmol_g_s)) %>% 
    # join with control means and compute difference
    left_join(control_fluxes, by = c("Site", "Inundation")) %>% 
    mutate(delta_co2 = flux_co2_umol_g_s - control_co2,
           delta_ch4 = flux_ch4_nmol_g_s - control_ch4) %>% 
    tidyr::gather(Gas, value, delta_co2, delta_ch4) %>% 
    select(Site, Core, Inundation, Gas, value) ->
    flux_change
  
  ggplot(flux_change, aes(x = value, color = Inundation)) + 
    geom_density() + 
    geom_vline(xintercept = 0, linetype = 2) + 
    facet_wrap(~Gas, scales = "free") ->
    p
  print(p)
  ggsave("outputs/aditi-fig3.png", plot = p)
  
  # Summary
  flux_change %>% 
    group_by(Inundation, Gas) %>% 
    summarise(value_sd = sd(value), change = mean(value)) %>% 
    ggplot(aes(Inundation, change, ymin = change - value_sd, ymax = change + value_sd)) + 
    geom_point() + geom_errorbar() +
    geom_hline(yintercept = 0, linetype = 2) +
    facet_wrap(~Gas, scales = "free") ->
    p
  print(p)
  ggsave("outputs/aditi-fig3-smry.png", plot = p, width = 6, height = 4)
}
