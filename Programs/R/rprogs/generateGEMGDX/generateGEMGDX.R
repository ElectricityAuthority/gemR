###############################################
### Title: Generate GEM GDX data file        ##
### Description: This file is used to        ##
### create a GDX file for use in the GEM     ##
### model.                                   ##
### Date: 18 April 2018                      ##
###############################################

###############################################
### 1. Set up libraries and parameters ########
###############################################

# Libraries
library(tidyverse)
library(lubridate)
library(ggthemes)
library(foreach)
library(pbapply)
library(gdxrrw)
library(DBI)
library(log4r)
library(stringr)

# Parameters
demand_year <- 2017
demand_forecast_file <- "Data/Forecast/annual_energy_forecasts_by_GXP_2012_2050.csv"
set.seed(12345)

###############################################
### 2. Additional setup #######################
###############################################

# Relative path to location of GDX creation code
codePath <- "Programs/R/rprogs/generateGEMGDX/"

# Read in configuration file
config <- config::get()

# Set path to GDX API (using path set in configuration file)
igdx(config$gams_dir)

# Create time suffix for current run
time_suffix <- str_replace_all(as.character(now()), "[:punct:]|\\s", "")

# Set scenario suffix
scenario_suffix <- "2Region9LB_Standard"

# Plot 'switches' (i.e. turn on/off plotting)
plot_ts <- FALSE # Takes approx. 5 minutes to plot all POCs
plot_LDCs <- FALSE # Takes approx. 13 minutes to plot all POCs

# Create new output directory if it doesn't exist
if(!dir.exists(paste0("Programs/R/output"))){
  dir.create(paste0("Programs/R/output"))
}

# Create archive folder for current run if it doesn't exist
if(!dir.exists(paste0("Programs/R/output/Archive_", time_suffix))){
  dir.create(paste0("Programs/R/output/Archive_", time_suffix))
}

# Create a new logger object
logger <- create.logger()

# Set logfile location
logfile(logger) <- paste0("Programs/R/output/Archive_", time_suffix, "/log_", time_suffix, ".log")

# Set the current level of the logger.
level(logger) <- 'INFO' 

# Connect to SQL (using credentials set in configuration file)
channel <- dbConnect(
  odbc::odbc(),
  .connection_string = paste0(
    "DRIVER=", config$masqlvmprd$driver, ";",
    "SERVER=", config$masqlvmprd$server, ";",
    "UID="   , config$masqlvmprd$user, ";",
    "PWD="   , config$masqlvmprd$password, ";"
  )
)

###############################################
### 3. Read in demand dataset #################
###############################################

source(paste0(codePath, "generateDemand.R"))

# Demand by POC
## This function looks to see if a demand file already exists for the given demand year.
## If it doesn't, it generates a demand file using data from reconciled consumption.
demand_by_POC <- generate_demand(demand_year = demand_year)

# National demand
# demand_NZ <- demand %>% 
#   group_by(dttm, tp, y, mn, d) %>% 
#   summarise(
#     MWh = sum(MWh)
#   ) %>% 
#   ungroup()

###############################################
### 4. Plot time series #######################
###############################################

# Plot time series if plotting turned on.
if(plot_ts == TRUE){
  
  source(paste0(codePath, "generateTSplots.R"))
  
  # Run generate_ts_plots() function
  generate_ts_plots(demand_data = demand_by_POC)
  
}

###############################################
### 5. Generate load duration curves (LDCs) ### 
############################################### 

source(paste0(codePath, "generateLDCs.R"))

# Generate LDCs by POC and assign them to load block
ldc_by_block <- generate_LDCs(demand_data = demand_by_POC, byPOC = TRUE)

ldc_by_block_sum <- generate_LDCs_sum(ldc_data = ldc_by_block, byPOC = TRUE)

# # Generate LDCs nationally and assign them to load block
# ldc_by_block_NZ <- generate_LDCs(demand_data = demand_NZ, byPOC = FALSE)
# 
# ldc_by_block_sum_NZ <- generate_LDCs_sum(ldc_data = ldc_by_block_NZ, byPOC = FALSE)

###############################################
### 6. Plot LDCs ##############################
###############################################

# Plot LDCs if plotting turned on.
if(plot_LDCs == TRUE){
  
  source(paste0(codePath, "generateLDCplots.R"))
  
  # Run generate_LDC_plots() function
  generate_LDC_plots(ldc_data = ldc_by_block)
  
}

###############################################
### 7. Compute block weights ##################
###############################################

source(paste0(codePath, "generateBlockWeights.R"))

# Generate block weights
block_weights <- generate_block_weights(ldc_by_block_sum)

###############################################
### 8. Compute load share #####################
###############################################

source(paste0(codePath, "generateLoadShare.R"))

# Generate load share 
load_share <- generate_load_share(ldc_by_block_sum)

###############################################
### 9. Apportion annual energy forecast #######
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
### 10. Map POCs to regions ###################
###############################################

source(paste0(codePath, "mapPOCStoRegions.R"))

# Read concordance of POCS to region from SQL
POC_region_concordance <- POC_region_concordance()

# Create mapping of POCs to regions
forecast_by_region_qtr <- forecast_by_region_qtr(
  forecast_dataset = forecast_by_load_share_blockwt
  , region_concordance_dataset = POC_region_concordance
)

# TODO: Regions - use GridZones from [DataWarehouse].[common].[DimPointOfConnection] but split Tiwai out from Southland

###############################################
### 11. Output to CSV #########################
###############################################

source(paste0(codePath, "createFinalCSV.R"))

final_file_name <- paste0("energyDemand_", scenario_suffix)

# Create final dataframe with required names. Output to CSV.
create_final_CSV(
  input_dataset = forecast_by_region_qtr
  , CSV_output_dir = "Programs/R/output/"
  , CSV_output_filename = paste0(final_file_name, ".csv")
)

# # Generate and execute the gms file to convert the CSV to a GDX
# convert_CSV_to_GDX(
#   CSV_filename = paste0(final_file_name, ".csv"),
#   GMS_filepath = paste0("P:/Market Analytics/EMI/EMI tools/gemR/Programs/R/output/Archive_", time_suffix),
#   GMS_filename = "generate_GEM_GDX.gms",
#   GDX_output_filename = paste0("../../../../Data/", final_file_name, ".gdx")
# )

###############################################
### 11. Generate GDX from CSVs ################
###############################################

# source(paste0(codePath, "GDXfromCSVs.R"))
# 
# generateGEMGDX(
#   use_default_demand = FALSE
#   , demand_location = paste0("Programs/R/output/Archive_", time_suffix, "/")
#   , demand_name = paste0("energyDemand_", scenario_suffix)
#   , output_dir = paste0("Programs/R/output/Archive_", time_suffix, "/")
#   , output_name = paste0("GEMData_", scenario_suffix, ".gdx")
# )

