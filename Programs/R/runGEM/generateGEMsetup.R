###############################################
### Title: generateGEMsetup()                ##
### Description: Function for generating the ##
### GEM setup include file from input CSVs   ##
###############################################
generateGEMsetup <- function(runName, runVersionName, runNameDesc,
                             firstYear, lastYear){
  
  # Read in global variables CSV
  globalVars <- read_csv("Data/Setup/globalVariables.csv") %>% 
    mutate(
      value = ifelse(is.na(value), "", value)
    ) %>% 
    # Add rows for runName, runVersionName and runNameDesc
    add_row(
      variable = c("runName", "runVersionName", "runNameDesc",
                   "firstYear", "lastYear")
      , value = c(runName, runVersionName, runNameDesc,
                  firstYear, lastYear)
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