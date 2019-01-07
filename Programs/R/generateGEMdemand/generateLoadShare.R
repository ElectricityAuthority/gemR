###############################################
### Title: Generate load share               ##
### Description: This code is used to        ##
### generate load share - the share of       ##
### annual load by POC and month.            ##
### Date: 7 August 2018                      ##
###############################################

generate_load_share <- function(ldc_dataset){
  
  ldc_dataset %>%
    group_by(poc, mn) %>%
    summarise(
      MWh = sum(MWh)
    ) %>%
    ungroup() %>%
    group_by(poc) %>%
    mutate(
      load_share = MWh / sum(MWh)
    ) %>%
    select(-MWh) %>%
    ungroup() %>%
    mutate(
      poc = as.character(poc)
    )
  
} 