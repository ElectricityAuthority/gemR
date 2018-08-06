###############################################
### Title: Generate block weights            ##
### Desription: This code is used to         ##
### generate block weights - the proportion  ##
### of energy by POC, load block and month.  ##
### Date: 7 August 2018                      ##
###############################################

generate_block_weights <- function(ldc_dataset){
  
  ldc_dataset %>%
    group_by(poc, mn) %>%
    mutate(
      block_weight = MWh / sum(MWh)
    ) %>%
    select(-MWh) %>% 
    ungroup() %>% 
    mutate(
      poc = as.character(poc)
    )
  
} 