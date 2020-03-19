# 4-figures.R

aditi_figure1 <- function() {
  
}

aditi_figure2 <- function(flux_summary) {
  
  flux_summary %>% 
    ungroup() %>%
    filter(Treatment %in% c("ControlCore", "InundatedCore")) %>% 
    mutate(Treatment = if_else(Treatment == "ControlCore", "CC", "IC")) %>% 
    filter(statistic %in% c("co2_mean", "ch4_mean")) %>% 
    mutate(statistic = if_else(statistic == "co2_mean", "CO[2]", "CH[4]")) %>% 
    group_by(Treatment, statistic, Site) %>% 
    summarise(Flux = mean(value), flux_sd = sd(value)) %>% 
    ggplot(aes(Treatment, Flux)) + 
    geom_errorbar(aes(ymin = Flux - flux_sd, ymax = Flux + flux_sd)) +
    facet_grid(statistic ~ Site, scales = "free_y", labeller = label_parsed) + 
    geom_point() +
    theme(strip.background = element_blank(),
          strip.text.y = element_text(angle = 00)) %>% 
    print()
  
  ggsave("outputs/aditi-fig2.png")
  
}
