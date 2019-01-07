###############################################
### Title: Generate final demand CSV file    ##
### Description: This function is used to    ##
### create a demand CSV file                 ##
### Date: 19 April 2018                      ##
###############################################

# Create final dataframe with required names
create_final_CSV <- function(input_dataset, CSV_output_dir, CSV_output_filename){
  
  # Create final dataframe with required names
  input_dataset %>% 
    rename(
      r = IslandCode
      , y = CalendarYear
      , t = quarter
      , i_NrgDemand = energy_month_block_GWh
    ) %>% 
    # Reorder columns
    select(r, y, t, lb, i_NrgDemand) %>% 
    # Write CSV
    write_csv(paste0(CSV_output_dir, "Archive_", time_suffix, "/", CSV_output_filename))
  
}