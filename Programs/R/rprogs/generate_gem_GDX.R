###############################################
### Title: Generate GEM GDX file             ##
### Desription: This file is used to         ##
### create a demand GDX file for use in      ##
### the GEM model.                           ##
### Date: 18 April 2018                      ##
###############################################

###############################################
### 1. Set up libraries and parameters ########
###############################################

# Libraries
library(tidyverse)
library(lubridate)
# library(RODBC)
library(ggthemes)
library(foreach)
library(pbapply)
library(gdxrrw)
library(DBI)

# Parameters
demand_year <- 2015
demand_forecast_file <- "Data/Annual energy forecasts for 180 GXPs, 2012-50, GWh (Dec 2009).csv"
set.seed(12345)

###############################################
### 2. Other setup ############################
###############################################

# Set path to GDX API
igdx("C:/GAMS/win64/25.1")

# Read in config file
config <- config::get()

# Source functions for converting CSV to GDX
source("Programs/R/rprogs/convert_CSV_to_GDX.R")

# Create new directories if they don't exist
if(!dir.exists(paste0("Programs/R/output"))){
  dir.create(paste0("Programs/R/output"))
}

if(!dir.exists(paste0("Programs/R/output/plots_", demand_year))){
  dir.create(paste0("Programs/R/output/plots_", demand_year))
}

# Connect to SQL (using credentials set in config file)

channel <- dbConnect(
  odbc::odbc(),
  .connection_string = paste0(
    "DRIVER=", config$madwprod$driver, ";",
    "SERVER=", config$madwprod$server, ";",
    "Trusted_Connection=Yes;"
  )
)

# conn_string <- paste0(
#   "DRIVER=ODBC Driver 13 for SQL Server;",
#   "Trusted_Connection=Yes;",
#   "SERVER=", sql_server_ip_addr 
# )
# 
# channel <- odbcDriverConnect(connection = conn_string)

###############################################
### 3. Read in demand dataset #################
###############################################

# Check to see if a demand CSV for the current 'demand_year' already exists.
# If it does, load it. If it doesn't, pull from SQL (takes approx. 5mins).

if(!file.exists(paste0("Programs/R/output/demand_", demand_year, ".csv"))){
  
  # Query demand data from SQL DataWarehouse and write to CSV
  # demand <- sqlQuery(
  demand <- dbGetQuery(
    conn = channel,
    statement = paste0(
      " 
      select
        DIM_DTTM_ID as dttm
        ,TradingPeriod
        ,POC as poc
        ,sum(Val) as kW
      from 
        [Wholesale_RM].[cm].[ReconciliationConsumption]
      where 
        left(DIM_DTTM_ID, 4) = ",
      
      demand_year,
      
      " 
      group by
        POC
        , TradingPeriod
        , DIM_DTTM_ID
      "
    )
  ) %>% 
    as_tibble() %>%
    mutate(
      dttm = ymd_hm(dttm)
      , y = year(dttm)
      , mn = month(dttm)
      , d = day(dttm)
      , MWh = kW / 2000
    ) %>%
    rename(tp = TradingPeriod) %>%
    select(-kW) %>% 
    arrange(poc, y, mn, d, tp)
  
  # Write to CSV (for later use)
  write_csv(demand, paste0("Programs/R/output/demand_", demand_year, ".csv"))
  
} else {
  
  demand <- read_csv(paste0("Programs/R/output/demand_", demand_year, ".csv"))
  
}

# Check if there are 17,520 records for each POC 
# (i.e. the number of TPs per year)
count_by_POC <- demand %>% 
  group_by(poc) %>%
  count()

if(nrow(count_by_POC %>% filter(n != 17520)) > 0) { 
  warning(paste(
    "The following POCS have less than 17,520 records:",
    paste((count_by_POC %>% filter(n != 17520))$poc, collapse = ", ")
  ))
}

###############################################
### 4. Plot time series #######################
###############################################

# # Create directory for ts plots if it doesn't exist
# if(!dir.exists(paste0("Programs/R/output/plots_", demand_year, "/ts"))){
#   dir.create(paste0("Programs/R/output/plots_", demand_year, "/ts"))
# }
# 
# # Create time series plots of load by POC
# pblapply(unique(demand$poc), function(x) {
#   
#   p <- demand %>%
#     filter(poc == x) %>%
#     ggplot(aes(dttm, MWh)) +
#     geom_line(alpha = 0.3) +
#     geom_smooth(se = FALSE, colour = "steelblue") +
#     theme_bw() +
#     labs(x = "Date", title = x) +
#     theme(plot.title = element_text(size = 20, hjust = 0.5))
#   
#   # Turn off the geom_smooth message
#   suppressMessages(
#     ggsave(paste0("Programs/R/output/plots_", demand_year, "/ts/", x, ".png"))
#   )
#   
# })

###############################################
### 5. Generate load duration curves (LDCs) ### 
############################################### 

# Define a 9-block LDC
b1l <- 1 ## Low wind top block
b1w <- 3 ## Windy top block
b2l <- 12 ## Low wind second block
b2w <- 36 ## Windy second block
b3l <- 40 ## Low wind third block
b3w <- 120 ## Windy third block
# b4 <- ## Fourth block (not assigned here. Gets the residual TPs after assigning all others)
b5 <- 400 ## Fifth block (i.e. N - 560 to N - 160)
b6 <- 160 ## Sixth/last block (i.e. the last 160 TPs)

# Function for assigning load to blocks (based on definition above)
assign_blocks <- function(x){
  
  tmp <- cumsum(c(b1l, b1w, b2l, b2w, b3l, b3w)) 
  tmp2 <- rev(cumsum(c(length(x), -b6, -b5))) #Note: this goes backwards from the end and the vector is then reversed.
  
  tmp3 <- c(tmp, tmp2)
  
  case_when(
    x %in% 0:tmp3[1] ~ "b1l"
    ,x %in% (tmp3[1] + 1):tmp3[2] ~ "b1w"
    ,x %in% (tmp3[2] + 1):tmp3[3] ~ "b2l"
    ,x %in% (tmp3[3] + 1):tmp3[4] ~ "b2w"
    ,x %in% (tmp3[4] + 1):tmp3[5] ~ "b3l"
    ,x %in% (tmp3[5] + 1):tmp3[6] ~ "b3w"
    ,x %in% (tmp3[6] + 1):tmp3[7] ~ "b4"
    ,x %in% (tmp3[7] + 1):tmp3[8] ~ "b5"
    ,x %in% (tmp3[8] + 1):tmp3[9] ~ "b6"
  )
  
}

# Create LDCs by POC by month
ldc <- demand %>%
  select(-dttm) %>%
  group_by(poc, mn) %>%
  mutate(
    load_ranked = rank(desc(MWh), ties.method = "first")
  ) %>%
  arrange(poc, mn, load_ranked)

# Create national LDC by month
ldc_national <- demand %>%
  group_by(y, mn, d, tp) %>%
  summarise(
    MWh = sum(MWh)
  ) %>%
  ungroup() %>%
  group_by(mn) %>%
  mutate(
    load_ranked = rank(desc(MWh), ties.method = "first")
  ) %>%
  arrange(mn, load_ranked)

# Assign load to blocks by POC and month
ldc_with_load_blocks <- ldc %>%
  group_by(poc, mn) %>%
  mutate(
    lb = assign_blocks(load_ranked)
    ,month = month(mn, label = TRUE, abbr = FALSE)
  )

# Assign load to blocks nationally by month
ldc_with_load_blocks_national <- ldc_national %>%
  group_by(mn) %>%
  mutate(
    lb = assign_blocks(load_ranked)
    ,month = month(mn, label = TRUE, abbr = FALSE)
  )

###############################################
### 6. Plot LDCs ##############################
###############################################

# # Create new LDC plot directory if it doesn't exist
# if(!dir.exists(paste0("Programs/R/output/plots_", demand_year, "/ldc_with_load_block"))){
#   dir.create(paste0("Programs/R/output/plots_", demand_year, "/ldc_with_load_block"))
# }
# 
# # Plot LDCs with coloured load blocks
# pblapply(unique(demand$poc), function(x) {
#   
#   p <- ldc_with_load_blocks %>%
#     filter(poc == x) %>%
#     ggplot(aes(load_ranked, ymax = MWh, ymin = 0, fill = lb)) +
#     geom_ribbon(alpha = 0.6) +
#     theme_bw() +
#     labs(x = "", y = "MWh", title = paste("LDCs for", x, "with load blocks")) +
#     theme(plot.title = element_text(size = 15, hjust = 0.5)) +
#     facet_wrap(~month)
#   
#   # Turn off the geom_smooth message
#   suppressMessages(
#     ggsave(paste0("Programs/R/output/plots_", demand_year, "/ldc_with_load_block/", x, ".png"), dpi = 450)
#   )
#   
# })

###############################################
### 7. Aggregate load to load blocks ##########
###############################################

# Sum up load by POC, month and load block
ldc_with_load_blocks_sum <- ldc_with_load_blocks %>%
  group_by(poc, mn, lb) %>%
  summarise(
    MWh = sum(MWh)
    ,count_TPs = n()
  )

# Sum up load nationally by month and load block
ldc_with_load_blocks_national_sum <- ldc_with_load_blocks_national %>%
  group_by(mn, lb) %>%
  summarise(
    MWh = sum(MWh)
    ,count_TPs = n()
  )

###############################################
### 8. Compute block weights and load share ###
###############################################

# BlockWeights - weight of energy by POC, load block and month
block_weights <- ldc_with_load_blocks_sum %>%
  group_by(poc, mn) %>%
  mutate(
    block_weight = MWh / sum(MWh)
  ) %>%
  select(-MWh) %>% 
  ungroup() %>% 
  mutate(
    poc = as.character(poc)
  )

# LoadShareByMonth - share of annual load by POC and month
load_share <- ldc_with_load_blocks_sum %>%
  group_by(poc, mn) %>%
  summarise(
    MWh = sum(MWh)
  ) %>%
  ungroup() %>%
  group_by(poc) %>%
  mutate(
    load_share = MWh / sum(MWh)
  ) %>%
  select(-MWh) %>%
  ungroup() %>%
  mutate(
    poc = as.character(poc)
  )

###############################################
### 9. Apportion annual energy forecast #######
###############################################

# Read in annual energy forecast dataset (note the use of 'comment = "*"' to skip commented rows)
annual_energy_forecast <- read_csv(demand_forecast_file, comment = "*")

# Transpose into long dataset
annual_energy_forecast_long <- annual_energy_forecast %>%
  gather(poc, energy_GWh, -CalendarYear)

# Join annual forecast to load share (note the implicit use of cartesian joining
# to assign the annual amount to each row of each poc by year)
load_share_with_energy_forecast <- load_share %>% 
  left_join(
    annual_energy_forecast_long
    , by = "poc"
  ) %>%
  #Calculate monthly load by POC by year
  mutate(
    energy_month_GWh = energy_GWh * load_share
  ) %>%
  #Drop annual energy amount and load share weights
  select(-c(energy_GWh, load_share)) %>% 
  arrange(poc, CalendarYear, mn) %>% 
  filter(!is.na(energy_month_GWh))

warning(
  "Current demand forecast file used in development only has 180 POCs. 
Temporarily dropping those records that don't have forecast values."
)

# Join on block weights to apportion further (note the implicit use of cartesian joining
# to assign the monthly amount to each row of each poc by load block)
load_share_and_block_wt_with_energy_forecast <- block_weights %>%
  select(-count_TPs)  %>%
  left_join(
    load_share_with_energy_forecast
    , by = c("poc", "mn")
  ) %>%
  #Calculate load per POC, month and load block
  mutate(
    energy_month_block_GWh = energy_month_GWh * block_weight
  ) %>%
  #Drop monthly energy amount and block weights
  select(-c(energy_month_GWh, block_weight)) %>% 
  filter(!is.na(energy_month_block_GWh))

warning(
  "Current demand forecast file used in development only has 180 POCs. 
  Temporarily dropping those records that don't have forecast values."
)

###############################################
### 10. Map regions to POCs ###################
###############################################

# Create mapping of POCs to regions
POC_region_info <- sqlQuery(
  channel = channel,
  "
  select 
    PointOfConnectionCode as poc
    , IslandCode
    , GridZoneDescription
  from 
    [DataWarehouse].[common].[DimPointOfConnection]
  "
) %>%
  as_tibble() %>%
  mutate(
    poc = as.character(poc)
  ) %>% 
  arrange(poc)

# Join region on to energy forecast table and generate quarters from months
load_by_year_timeperiod_lb_region <- load_share_and_block_wt_with_energy_forecast %>%
  left_join(
    POC_region_info,
    by = "poc"
  ) %>%
  mutate(
    IslandCode = tolower(IslandCode)
    ,quarter = 
      case_when(
        mn %in% 1:3    ~ "q1"  
        ,mn %in% 4:6    ~ "q2"
        ,mn %in% 7:9    ~ "q3"
        ,mn %in% 10:12  ~ "q4"
      )
  ) %>%
  #Group load by year, region, quarter and load block
  group_by(
    CalendarYear
    , IslandCode
    , quarter
    , lb
  ) %>%
  #Summarise energy by region
  summarise(
    energy_month_block_GWh = sum(
      energy_month_block_GWh
      , na.rm = TRUE
    )
  ) %>% 
  ungroup()

###############################################
### 11. Output to GDX #########################
###############################################

# Create final dataframe with required names
i_NrgDemand_df <- load_by_year_timeperiod_lb_region %>% 
  rename(
    r = IslandCode
    , y = CalendarYear
    , t = quarter
    , Value = energy_month_block_GWh
  ) %>% 
  #Reorder
  select(r, y, t, lb, Value)

# Write CSV
write_csv(i_NrgDemand_df, "Programs/R/output/i_NrgDemand_df.csv", col_names = FALSE)

# Generate and execute a gms file to convert the CSV to a GDX
convert_CSV_to_GDX(
  CSV_filename = "Programs/R/output/i_NrgDemand_df.csv",
  GMS_filename = "Programs/R/output/generate_GEM_GDX.gms",
  GDX_output_filename = "Data/i_NrgDemand.gdx"
)
