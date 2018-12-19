library(tidyverse)
library(gdxtools)
library(rlist)
library(stringr)

generateGEMGDX <- function(use_default_demand = FALSE, demand_location, demand_name, output_dir, output_name){
  
  # CSV vectors
  sets_csv_list <- list.files("Data/Sets", recursive = TRUE, pattern = ".csv", full.names = TRUE)
  subsets_csv_list <- list.files("Data/Subsets", recursive = TRUE, pattern = ".csv", full.names = TRUE)
  params_csv_list <- list.files("Data/Parameters", recursive = TRUE, pattern = ".csv", full.names = TRUE)
  
  # If the default demand is set to FALSE, replace the default 'energyDemand' parameter file with the new one
  if(!use_default_demand){
    
    params_csv_list <- str_replace(
      params_csv_list
      , "Data/Parameters/energyDemand.csv"
      , paste0(demand_location, demand_name, ".csv")
    )
    
  } 
  
  # Sets
  
  ## Initialise sets list
  df_sets_list <- list()
  
  ## Loop through CSVs and extract sets
  for(csv in sets_csv_list){
    
    print(paste0("*****", csv, "*****"))
    
    x <- suppressMessages(read_csv(csv))
    
    x_cols <- colnames(x)
    
    print(paste0("*", x_cols, "*"))
    
    df_sets_list[[x_cols]] <- x
    
  }
  
  # Subsets
  
  ## Initialise subsets list
  df_subsets_list <- list()
  
  ## Loop through CSVs and extract subsets
  for(csv in subsets_csv_list){
    
    # Extract subset name from full path
    subset_name <- str_extract(csv, "[A-Z a-z _]+.csv") %>%
      str_replace_all(".csv", "")
    
    print(paste0("*****", csv, "*****"))
    
    x <- suppressMessages(read_csv(csv))
    
    df_subsets_list[[subset_name]] <- x
    
  }
  
  # Parameters
  
  ## Initialise parameters list
  df_params_list <- list()
  
  ## Loop through CSVs and extract parameters
  for(csv in params_csv_list){
    
    print(paste0("*****", csv, "*****"))
    
    x <- suppressMessages(read_csv(csv))
    
    # Treat scalars file differently 
    if(str_detect(csv, "scalars.csv")){
      
      for(i in 1:nrow(x)){
        
        scalar_tmp <- x[i,]
        
        df_params_list[[scalar_tmp$name]] <- data_frame(value = as.numeric(scalar_tmp$value))
        
      }
      
    } else {
      
      x_cols <- colnames(x)
      
      x_sets <- x_cols[!(x_cols %>% str_detect("i_"))]
      x_parameters <- x_cols[x_cols %>% str_detect("i_")]
      
      for(param in 1:length(x_parameters)){
        
        print(paste0("*", x_parameters[param], "*"))
        
        df_tmp <- x %>% 
          select(
            x_sets
            , x_parameters[param]
          ) %>% 
          rename(
            value = x_parameters[param]
          ) %>% 
          mutate(
            value = as.numeric(value)
          )
        
        df_params_list[[x_parameters[param]]] <- df_tmp
        
      }
      
    }
    
  }
  
  write2.gdx(
    paste0(output_dir, output_name)
    , params = df_params_list
    , sets = list.merge(
      df_sets_list
      , df_subsets_list
    )
  )
  
}
