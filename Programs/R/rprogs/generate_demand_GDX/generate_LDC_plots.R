###############################################
### Title: Generate LDC plots                ##
### Desription: Generate LDC plots           ##
### of load by POC.                          ##
### Date: 6 August 2018                      ##
###############################################

# Check if plot output folder exists for current demand year. If not create it.
if(!dir.exists(paste0("Programs/R/output/plots_", demand_year))){
  dir.create(paste0("Programs/R/output/plots_", demand_year))
}

# Create new LDC plot directory if it doesn't exist
if(!dir.exists(paste0("Programs/R/output/plots_", demand_year, "/ldc_plots"))){
  dir.create(paste0("Programs/R/output/plots_", demand_year, "/ldc_plots"))
}

# Plot LDCs with coloured load blocks
generate_LDC_plots <- function(ldc_data){
  
  pblapply(unique(ldc_data$poc), function(x) {
    
    p <- ldc_data %>%
      filter(poc == x) %>%
      ggplot(aes(load_ranked, ymax = MWh, ymin = 0, fill = lb)) +
      geom_ribbon(alpha = 0.6) +
      theme_bw() +
      labs(x = "", y = "MWh", title = paste("LDCs for", x, "with load blocks")) +
      theme(plot.title = element_text(size = 15, hjust = 0.5)) +
      facet_wrap(~month)
    
    # Output plots (suppress the geom_smooth message)
    suppressMessages(
      ggsave(paste0("Programs/R/output/plots_", demand_year, "/ldc_plots/", x, ".png"), dpi = 450)
    )
    
  })
  
}