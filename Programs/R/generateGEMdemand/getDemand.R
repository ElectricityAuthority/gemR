###############################################
### Title: Generate demand                   ##
### Description: This file either creates    ##
### demand file from the SQL database or     ##
### loads the demand file if it already      ##
### exists.                                  ##
### Date: 6 August 2018                      ##
###############################################

# Check to see if a demand CSV for the current 'demand_year' already exists.
# If it does, load it. If it doesn't, pull from SQL (takes approx. 5mins).

getDemand <- function(demand_year){
  
  if(!file.exists(paste0("Data/Demand/demand_", demand_year, ".csv"))){
    
    # Query demand data from SQL DataWarehouse and write to CSV
    demand <- dbGetQuery(
      conn = channel,
      statement = paste0(
        " 
        select
        DIM_DTTM_ID as dttm
        ,TradingPeriod
        ,POC as poc
        ,sum(Val) as kW
        from 
        [Wholesale_RM].[cm].[ReconciliationConsumption]
        where 
        left(DIM_DTTM_ID, 4) = ",
        
        demand_year,
        
        " 
        group by
        POC
        , TradingPeriod
        , DIM_DTTM_ID
        "
      )
      ) %>% 
      as_tibble() %>%
      mutate(
        dttm = ymd_hm(dttm)
        , y = year(dttm)
        , mn = month(dttm)
        , d = day(dttm)
        , MWh = kW / 2000
      ) %>%
      rename(tp = TradingPeriod) %>%
      select(-kW) %>% 
      arrange(poc, y, mn, d, tp) %>% 
      ungroup
    
    # Write to CSV (for later use)
    write_csv(demand, paste0("Data/Demand/demand_", demand_year, ".csv"))
    
  } else {
    
    demand <- read_csv(paste0("Data/Demand/demand_", demand_year, ".csv"))
    
  }
  
  return(demand)
  
}
