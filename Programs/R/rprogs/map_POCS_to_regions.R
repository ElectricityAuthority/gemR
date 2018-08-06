###############################################
### Title: Map POCs to regions               ##
### Desription: This code is used to         ##
### map POCs to regions                      ##
### Date: 7 August 2018                      ##
###############################################

# Read concordance of POCS to region from SQL
POC_region_concordance <- function(){
  
  dbGetQuery(
    
    conn = channel,
    
    statement = 
      
  "
  select 
  PointOfConnectionCode as poc
  , IslandCode
  , GridZoneDescription
  from 
  [DataWarehouse].[common].[DimPointOfConnection]
  "
    
  ) %>%
    as_tibble() %>%
    mutate(
      poc = as.character(poc)
    ) %>% 
    arrange(poc)
  
}


# Join region with the energy forecast table and generate quarters from months
forecast_by_region_qtr <- function(forecast_dataset, region_concordance_dataset){
  
  forecast_dataset %>%
    left_join(
      region_concordance_dataset,
      by = "poc"
    ) %>%
    mutate(
      IslandCode = tolower(IslandCode)
      ,quarter = 
        case_when(
          mn %in% 1:3    ~ "q1"  
          ,mn %in% 4:6    ~ "q2"
          ,mn %in% 7:9    ~ "q3"
          ,mn %in% 10:12  ~ "q4"
        )
    ) %>%
    # Group load by year, region, quarter and load block
    group_by(
      CalendarYear
      , IslandCode
      , quarter
      , lb
    ) %>%
    # Summarise energy by region
    summarise(
      energy_month_block_GWh = sum(
        energy_month_block_GWh
        , na.rm = TRUE
      )
    ) %>% 
    ungroup()
  
}
