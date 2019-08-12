###############################################
### Title: Generate GEM demand file          ##
### Description: This file is used to        ##
### create a demand CSV file for use in      ##
### the GEM model.                           ## 
### Date: 18 April 2018                      ##
###############################################

###############################################
### 1. Set up libraries and parameters       ##
###############################################

# Libraries
# library(tidyverse)
library(lubridate)
library(ggthemes)
library(foreach)
library(pbapply)
library(log4r)
library(stringr)

# Parameters
demand_year <- 2017
demand_forecast_file <- "Data/Demand/Forecast/annual_energy_forecasts_by_GXP_2012_2050.csv"
set.seed(12345)

###############################################
### 2. Additional setup                      ##
###############################################

# Relative path to location of GDX creation code
codePath <- "Programs/R/generateGEMdemand/"

# Create time suffix for current run
time_suffix <- str_replace_all(as.character(now()), "[:punct:]|\\s", "")

# Set scenario suffix
scenario_suffix <- "2Region9LB_Standard"

# Plot 'switches' (i.e. turn on/off plotting)
plot_ts <- FALSE # Takes approx. 5 minutes to plot all POCs
plot_LDCs <- FALSE # Takes approx. 13 minutes to plot all POCs

# Create archive folder for current run if it doesn't exist
if(!dir.exists(paste0("Data/Demand/Archive_", time_suffix))){
  dir.create(paste0("Data/Demand/Archive_", time_suffix))
}

# Create a new logger object
logger <- create.logger()

# Set logfile location
logfile(logger) <- paste0("Data/Demand/Archive_", time_suffix, "/log_", time_suffix, ".log")

# Set the current level of the logger.
level(logger) <- 'INFO' 

###############################################
### 3. Read in demand dataset                ##
###############################################

# Load demand by POC
demand_by_POC <- read_csv(paste0("Data/Demand/demand_", demand_year, ".csv"))

###############################################
### 4. Plot time series                      ##
###############################################

# Plot time series if plotting turned on.
if(plot_ts == TRUE){
  
  source(paste0(codePath, "generateTSplots.R"))
  
  # Run generate_ts_plots() function
  generate_ts_plots(demand_data = demand_by_POC)
  
}

###############################################
### 5. Generate load duration curves (LDCs)  ## 
############################################### 

source(paste0(codePath, "generateLDCs.R"))

# Generate LDCs by POC and assign them to load block
ldc_by_block <- generate_LDCs(demand_data = demand_by_POC, byPOC = TRUE)

ldc_by_block_sum <- generate_LDCs_sum(ldc_data = ldc_by_block, byPOC = TRUE)

###############################################
### 6. Plot LDCs                             ##
###############################################

# Plot LDCs if plotting turned on.
if(plot_LDCs == TRUE){
  
  source(paste0(codePath, "generateLDCplots.R"))
  
  # Run generate_LDC_plots() function
  generate_LDC_plots(ldc_data = ldc_by_block)
  
}

###############################################
### 7. Compute block weights                 ##
###############################################

source(paste0(codePath, "generateBlockWeights.R"))

# Generate block weights
block_weights <- generate_block_weights(ldc_by_block_sum)

###############################################
### 8. Compute load share                    ##
###############################################

source(paste0(codePath, "generateLoadShare.R"))

# Generate load share 
load_share <- generate_load_share(ldc_by_block_sum)

###############################################
### 9. Apportion annual energy forecast      ##
###############################################

source(paste0(codePath, "apportionForecastLoad.R"))

# Read forecast demand dataset
forecast_demand <- read_forecast_demand(forecast_file = demand_forecast_file)

# Join annual forecast with load share and apportion national demand by POC and month
forecast_by_load_share <- forecast_by_load_share(
  load_share_dataset = load_share
  , forecast_dataset = forecast_demand
)

# Join above dataset with block weights data and apportion demand within each year, 
# POC and month using block weights
forecast_by_load_share_blockwt <- forecast_by_load_share_blockwt(
  block_weight_dataset = block_weights
  , load_share_forecast_dataset = forecast_by_load_share
)

###############################################
### 10. Map POCs to regions                  ##
###############################################

source(paste0(codePath, "mapPOCStoRegions.R"))

# Read concordance of POCS to region from SQL
# POC_region_concordance <- POC_region_concordance()
POC_region_concordance <- read_csv("Data/Geography/mapPOCsToRegions.csv")

# Create mapping of POCs to regions
forecast_by_region_qtr <- forecast_by_region_qtr(
  forecast_dataset = forecast_by_load_share_blockwt
  , region_concordance_dataset = POC_region_concordance
)

# TODO: Regions - use GridZones from [DataWarehouse].[common].[DimPointOfConnection] but split Tiwai out from Southland

###############################################
### 11. Output to CSV                        ##
###############################################

source(paste0(codePath, "createFinalCSV.R"))

final_file_name <- paste0("energyDemand_", scenario_suffix)

# Create final dataframe with required names. Output to CSV.
create_final_CSV(
  input_dataset = forecast_by_region_qtr
  , CSV_output_dir = "Data/Demand/"
  , CSV_output_filename = paste0(final_file_name, ".csv")
)

