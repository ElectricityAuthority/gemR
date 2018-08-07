###############################################
### Title: Apportion forecast load to POCs   ##
### Desription: This code is used to         ##
### apportion national annual load forecast  ##
### to POCs based on load share and block    ##
### weights.                                 ##
### Date: 7 August 2018                      ##
###############################################

# Read in annual energy forecast dataset and convert to long dataset
read_forecast_demand <- function(){
  
  annual_energy_forecast <- read_csv(demand_forecast_file, comment = "*")
  
  annual_energy_forecast_long <- annual_energy_forecast %>%
    gather(poc, energy_GWh, -CalendarYear)
  
  return(annual_energy_forecast_long)
  
}

# Join annual forecast to load share (note the implicit use of cartesian joining
# to assign the annual amount to each row of each poc by year)
forecast_by_load_share <- function(load_share_dataset, forecast_dataset){
  
  tmp <- load_share_dataset %>% 
    left_join(
      forecast_dataset
      , by = "poc"
    ) %>%
    # Calculate monthly load by POC by year
    mutate(
      energy_month_GWh = energy_GWh * load_share
    ) %>%
    # Drop annual energy amount and load share weights
    select(-c(energy_GWh, load_share)) %>% 
    arrange(poc, CalendarYear, mn) %>% 
    filter(!is.na(energy_month_GWh))
  
  warn(
    logger
    , "Current demand forecast file used in development only has 180 POCs. 
    Temporarily dropping those records that don't have forecast values."
  )
  
  return(tmp)
  
} 

# Join on block weights to apportion further (note the implicit use of cartesian joining
# to assign the monthly amount to each row of each poc by load block)
forecast_by_load_share_blockwt <- function(block_weight_dataset, load_share_forecast_dataset){
  
  tmp <- load_share_and_block_wt_with_energy_forecast <- block_weight_dataset %>%
    select(-count_TPs)  %>%
    left_join(
      load_share_forecast_dataset
      , by = c("poc", "mn")
    ) %>%
    # Calculate load per POC, month and load block
    mutate(
      energy_month_block_GWh = energy_month_GWh * block_weight
    ) %>%
    # Drop monthly energy amount and block weights
    select(-c(energy_month_GWh, block_weight)) %>% 
    filter(!is.na(energy_month_block_GWh))
  
  warn(
    logger
    , "Current demand forecast file used in development only has 180 POCs. 
  Temporarily dropping those records that don't have forecast values."
  )
  
  return(tmp)
  
}


