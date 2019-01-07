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

demand_location <- "Data/Demand/Archive_20190108100602/"
demand_name <- "energyDemand_2Region9LB_Standard"

###############################################
### 3. Set up folders                        ##
###############################################
source("Programs/R/runGEM/setupFolders.R")

setupFolders()

###############################################
### 4. Load function for executing GAMS code ##
###############################################
source("Programs/R/shared/executeGAMS.R")

###############################################
### 5. Execute GEMdeclarations               ##
###############################################
executeGAMS(
  GAMS_filepath = "Programs/GAMS"
  , GAMS_filename = "GEMdeclarations"
  , GAMS_opts = "s = GEMdeclarations"
)

###############################################
### 6. Generate GEMsetup include file from   ##
### CSV                                      ##
###############################################
source("Programs/R/runGEM/generateGEMsetup.R")

generateGEMsetup()

###############################################
### 7. Read input symbols from CSV           ##
###############################################
source("Programs/R/runGEM/readInputSymbols.R")

###############################################
### 8. Generate GEMdata GDX file             ##
###############################################
source("Programs/R/runGEM/generateGEMdata.R")

generateGEMdata(
  use_default_demand = FALSE
  , demand_location = demand_location
  , demand_name = demand_name
)

###############################################
### 9. Execute GEMsolve                      ##
###############################################
executeGAMS(
  GAMS_filepath = "Programs/GAMS"
  , GAMS_filename = "GEMsolve"
  , GAMS_opts = "r = GEMdeclarations gdx = GEMsolve"
)

###############################################
### 10. Move report to Archive               ##
###############################################
source("Programs/R/runGEM/relocateReport.R")

relocateReport()

