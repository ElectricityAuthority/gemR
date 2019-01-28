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

##############################################
### 2. Additional setup                      ##
###############################################

runName <- "standard_run"

runNameDesc <- "Standard run"

firstYear <- "2018"

lastYear <- "2028"

demand_path <- "Data/Demand/i_NrgDemand_2Region9LB_Standard.csv"

GEMdeclarationsFlag <- FALSE

# Turn off messages from readr
options(readr.num_columns = 0)

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

###############################################
### 6. Set up folders                        ##
###############################################
source("Programs/R/runGEM/setupFolders.R")

setupFolders(runName = runName)

###############################################
### 7. Generate GEMsetup include file from   ##
### CSV                                      ##
###############################################
source("Programs/R/runGEM/generateGEMsetup.R")

generateGEMsetup(
  runName = runName
  , runNameDesc = runNameDesc
  , firstYear = firstYear
  , lastYear = lastYear
)

###############################################
### 8. Generate GEMdata GDX file             ##
###############################################
source("Programs/R/runGEM/generateGEMdata.R")

generateGEMdata(
  use_default_demand = FALSE
  , demand_path = demand_path
  , firstYear = firstYear
  , lastYear = lastYear
)

###############################################
### 9. Archive input GDX for current run     ##
###############################################
source("Programs/R/runGEM/archiveGEMdataGDX.R")

archiveGEMdataGDX(runName = runName)

###############################################
### 10. Execute GEMsolve                     ##
###############################################
executeGAMS(
  GAMS_filepath = "Programs/GAMS"
  , GAMS_filename = "GEMsolve"
  , GAMS_opts = paste0("r = GEMdeclarations gdx = ", "../../Output/", runName, "/GEMsolve_", runName, ".gdx")
)

###############################################
### 11. Create reports                       ##
###############################################
source("Programs/R/generateGEMreports/GEMreporting.R")

createGEMreports(runName = runName)
