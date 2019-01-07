###############################################
### Title: generateGEMsetup()                ##
### Description: Function for generating the ##
### GEM setup include file from input CSVs   ##
###############################################
generateGEMsetup <- function(){
  
  # Read in global variables CSV
  globalVars <- read_csv("Data/Setup/globalVariables.csv") %>% 
    mutate(
      value = ifelse(is.na(value), "", value)
    )
  
  # Read in scalar variables CSV
  scalarVars <- read_csv("Data/Setup/scalarVariables.csv")
  
  # Create list of global variables
  globalVars_list <- paste0(
    '$setglobal ' 
    , globalVars$variable
    , ' "'
    , globalVars$value
    , '"'
    , collapse = "\n"
  ) 
  
  # Create list of scalar variables
  scalarVars_list <- paste(
    "Scalar"
    , scalarVars$variable
    , "/"
    , scalarVars$value
    , "/ ;"
    , collapse = "\n"
  )
  
  write_lines(
    c(
      globalVars_list
      , scalarVars_list
    )
    , "Programs/GAMS/GEMsetup.inc"
  )
  
}