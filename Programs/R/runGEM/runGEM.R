###############################################
### Title: Run GEM                           ##
### Description: Run GEM from end-to-end     ##
### Date: 7 January 2019                     ##
###############################################

###############################################
### 1. Load libraries                        ##
###############################################
library(tidyverse)
library(gdxtools)
library(gdxrrw)
library(readtext)
library(rlist)

###############################################
### 2. Additional setup                      ##
###############################################

runName <- c(
  "standard_run"
  , "demand_plus_10"
)

runVersionName <- c(
  "v1"
  , "v1"
)

runNameDesc <- c(
  "Standard run"
  , "Demand plus 10 percent"
)

demand_location <- c(
  "Data/Demand/Archive_20190108100602/"
  , "Data/Demand/Archive_20190108100602/"
)

demand_name <- c(
  "energyDemand_2Region9LB_Standard"
  , "energyDemand_2Region9LB_plus10percent"
)

runSheet <- tibble(runName, runVersionName, runNameDesc, demand_location, demand_name)

GEMdeclarationsFlag <- FALSE

###############################################
### 3. Load function for executing GAMS code ##
###############################################
source("Programs/R/shared/executeGAMS.R")

###############################################
### 4. Execute GEMdeclarations               ##
###############################################
if(GEMdeclarationsFlag){
  
  executeGAMS(
    GAMS_filepath = "Programs/GAMS"
    , GAMS_filename = "GEMdeclarations"
    , GAMS_opts = "s = GEMdeclarations"
  )
  
}

###############################################
### 5. Read input symbols from CSV           ##
###############################################
source("Programs/R/runGEM/readInputSymbols.R")

# Loop through "run sheet"
for(i in 1:nrow(runSheet)){
  
  ###############################################
  ### 6. Set up folders                        ##
  ###############################################
  source("Programs/R/runGEM/setupFolders.R")
  
  setupFolders(runName = runSheet$runName[i])
  
  ###############################################
  ### 7. Generate GEMsetup include file from   ##
  ### CSV                                      ##
  ###############################################
  source("Programs/R/runGEM/generateGEMsetup.R")
  
  generateGEMsetup(
    runName = runSheet$runName[i]
    , runVersionName = runSheet$runVersionName[i]
    , runNameDesc = runSheet$runNameDesc[i]
  )
  
  ###############################################
  ### 8. Generate GEMdata GDX file             ##
  ###############################################
  source("Programs/R/runGEM/generateGEMdata.R")
  
  generateGEMdata(
    use_default_demand = FALSE
    , demand_location = runSheet$demand_location[i]
    , demand_name = runSheet$demand_name[i]
  )
  
  ###############################################
  ### 9. Execute GEMsolve                      ##
  ###############################################
  executeGAMS(
    GAMS_filepath = "Programs/GAMS"
    , GAMS_filename = "GEMsolve"
    , GAMS_opts = "r = GEMdeclarations gdx = GEMsolve"
  )
  
}
