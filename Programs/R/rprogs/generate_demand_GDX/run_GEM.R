###############################################
### Title: Run the GEM model                 ##
### Desription: This file is used to         ##
### run the GEM model                        ##
### Date: 7 August 2018                      ##
###############################################

###############################################
### 1. Set up libraries and parameters ########
###############################################

library(tidyverse)
library(gdxrrw)

# Function to run GAMS (TODO: put in separate script)
run_GAMS <- function(GMS_filepath, GMS_filename){
  
  # Switch working directory temporarily to where GMS file is
  old_wd <- getwd()
  
  setwd(GMS_filepath)
  
  # Run gms file with gams
  gams(GMS_filename)
  
  # Set working directory back
  setwd(old_wd)
  
}

###############################################
### 2. Run the GEM setup program ##############
###############################################

run_GAMS(
  GMS_filepath = "P:/Market Analytics/EMI/EMI tools/gemR/Programs/GAMS"
  , GMS_filename = "runGEMsetup.gms" 
)

###############################################
### 3. Run the GEM Data and Solve program #####
###############################################

run_GAMS(
  GMS_filepath = "P:/Market Analytics/EMI/EMI tools/gemR/Programs/GAMS"
  , GMS_filename = "runGEMDataAndSolve.gms" 
)

# TODO:
## Create ability to edit GEMpathsAndFiles.inc. Or maybe turn this in to a CSV input? 
## Create output folders etc using R instead of using a batch file from GAMS?
