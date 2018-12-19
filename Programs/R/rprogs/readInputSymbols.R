###############################################
### Title: Read in all input symbols         ##
### Description: Read in all input symbols   ##
### from CSVs.                               ##
### Date: 19 December 2018                   ##
###############################################

###############################################
### Read in global and scalar variables #######
###############################################

globalVars <- read_csv("Data/Setup/globalVariables.csv") %>% 
  spread(variable, value) %>% 
  as.list()

scalarVars <- read_csv("Data/Setup/scalarVariables.csv") %>% 
  spread(variable, value) %>% 
  as.list()

###############################################
### Get all input CSV paths ###################
###############################################

get_CSV_paths <- function(fp){
  list.files(fp, recursive = TRUE, pattern = ".csv", full.names = TRUE)
}

sets_csv_list <- get_CSV_paths(fp = "Data/Sets")
subsets_csv_list <- get_CSV_paths(fp = "Data/Subsets")
params_csv_list <- get_CSV_paths(fp = "Data/Parameters")

###############################################
### Read in all sets ##########################
###############################################

# Initialise sets list
sets <- list()

# Loop through CSVs and extract sets
for(csv in sets_csv_list){
  
  # print(paste0("*****", csv, "*****"))
  
  x <- suppressMessages(read_csv(csv))
  
  x_cols <- colnames(x)
  
  # print(paste0("*", x_cols, "*"))
  
  sets[[x_cols]] <- x
  
}

###############################################
### Read in all subsets #######################
###############################################

# Initialise subsets list
subsets <- list()

# Loop through CSVs and extract subsets
for(csv in subsets_csv_list){
  
  # Extract subset name from full path
  subset_name <- str_extract(csv, "[A-Z a-z _]+.csv") %>%
    str_replace_all(".csv", "")
  
  # print(paste0("*****", csv, "*****"))
  
  x <- suppressMessages(read_csv(csv))
  
  subsets[[subset_name]] <- x
  
}

###############################################
### Read in all parameters ####################
###############################################

# Initialise parameters list
params <- list()

# Loop through CSVs and extract parameters
for(csv in params_csv_list){
  
  # print(paste0("*****", csv, "*****"))
  
  x <- suppressMessages(read_csv(csv))
  
  # Treat scalars file differently 
  if(str_detect(csv, "scalars.csv")){
    
    for(i in 1:nrow(x)){
      
      scalar_tmp <- x[i,]
      
      params[[scalar_tmp$name]] <- data_frame(value = as.numeric(scalar_tmp$value))
      
    }
    
  } else {
    
    x_cols <- colnames(x)
    
    x_sets <- x_cols[!(x_cols %>% str_detect("i_"))]
    x_parameters <- x_cols[x_cols %>% str_detect("i_")]
    
    for(param in 1:length(x_parameters)){
      
      # print(paste0("*", x_parameters[param], "*"))
      
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
      
      params[[x_parameters[param]]] <- df_tmp
      
    }
    
  }
  
}

###############################################
### Assign a couple of (hard-coded) sets ######
### that were previously assigned in     ###### 
### GEMdeclarations                      ######
###############################################

# ild - Islands
sets$ild <- data_frame(ild = c("ni", "si"))

# lvl - Levels of non-free reserves
sets$lvl <- data_frame(lvl = paste0("lvl", 1:5))

# aggR - Aggregate regional entities
sets$aggR <- data_frame(aggR = c("ni", "si", "nz"))

###############################################
### Assign a couple of (hard-coded) scalars ###
### that were previously assigned in     ###### 
### GEMdeclarations                      ######
###############################################

# largestNIplant
params$largestNIplant <- data_frame(value = 385)

# largestSIplant
params$largestSIplant <- data_frame(value = 125)

###############################################
### Assign a couple of (hard-coded) sets ######
### that were previously assigned in     ###### 
### GEMdata                              ######
###############################################

# n
sets$n <- data_frame(n = paste0("n", 1:as.numeric(globalVars$NumVertices)))

## numT - number of tranches
params$numT <- data_frame(value = as.numeric(globalVars$NumVertices) - 1)

###############################################
### Tidy up (i.e. remove unwanted objects) ####
###############################################
to_keep <- c("sets", "subsets", "params", "globalVars", "scalarVars")

rm(list = setdiff(ls(), to_keep))

