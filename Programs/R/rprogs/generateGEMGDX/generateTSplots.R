###############################################
### Title: Generate time series plots        ##
### Description: Generate time series plots  ##
### of load by POC.                          ##
### Date: 6 August 2018                      ##
###############################################

# Check if plot output folder exists for current demand year. If not create it.
if(!dir.exists(paste0("Programs/R/output/plots_", demand_year))){
  dir.create(paste0("Programs/R/output/plots_", demand_year))
}

# Create directory for ts plots if it doesn't exist
if(!dir.exists(paste0("Programs/R/output/plots_", demand_year, "/ts"))){
  dir.create(paste0("Programs/R/output/plots_", demand_year, "/ts"))
}

# Create time series plots of load by POC
generate_ts_plots <- function(demand_data){
  
  pblapply(unique(demand_data$poc), function(x) {
    
    p <- demand_data %>%
      filter(poc == x) %>%
      ggplot(aes(dttm, MWh)) +
      geom_line(alpha = 0.3) +
      geom_smooth(se = FALSE, colour = "steelblue") +
      theme_bw() +
      labs(x = "Date", title = x) +
      theme(plot.title = element_text(size = 20, hjust = 0.5))
    
    # Output plots (suppress the geom_smooth message)
    suppressMessages(
      ggsave(paste0("Programs/R/output/plots_", demand_year, "/ts/", x, ".png"))
    )
    
  })
  
}
