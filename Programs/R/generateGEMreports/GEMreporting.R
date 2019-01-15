###############################################
### Title: GEM reporting                     ##
### Description: Create reports for current  ##
### GEM run.                                 ##
### Date: 7 January 2019                     ##
###############################################

###############################################
### 1. Load libraries                        ##
###############################################
library(tidyverse)
library(gdxrrw)

###############################################
### 2. Additional setup                      ##
###############################################

# Turn off messages from readr
options(readr.num_columns = 0)

# ggplot shared settings
theme_set(
  theme_bw() +
    theme(
      axis.text = element_text(size = 5)
      , axis.title = element_text(size = 7)
      , legend.text = element_text(size = 5)
      , legend.title = element_text(size = 7)
    )
)

###############################################
### 3. Functions                             ##
###############################################

# Function for getting all results for single variable
getResults <- function(runName, runVersionName, variableName, colNames){
  
  reportPath <- paste0(
    "Output/"
    , runName
    , "/GDX/allExperimentsReportOutput - "
    , runName
    ,"_"
    , runVersionName
    , ".gdx")
  
  reportItem <- rgdx.param(reportPath, variableName) %>% 
    mutate_if(is.factor, as.character) %>% 
    as_tibble()
  
  colnames(reportItem) <- colNames
  
  return(reportItem)
  
}

createGEMreports <- function(runName, runVersionName){
  
  ###############################################
  ### 4. Get external vars                     ##
  ###############################################
  
  # Read in GEMsolve output GDX to use some subsets etc.
  GEMsolveGDX_path <- paste0("Output/", runName, "/GEMsolve_", runName, ".gdx")
  
  ## Map generation to technology
  mapg_k <- rgdx.set(GEMsolveGDX_path, "mapg_k") %>% 
    mutate_if(is.factor, as.character) %>% 
    rename(
      Plant = g
      , Technology = k
    ) %>% 
    as_tibble()
  
  ## Map generation to fuel
  mapg_f <- rgdx.set(GEMsolveGDX_path, "mapg_f") %>% 
    mutate_if(is.factor, as.character) %>% 
    rename(
      Plant = g
      , Fuel = f
    ) %>% 
    as_tibble()
  
  ## Map generation to island
  mapg_ild <- rgdx.set(GEMsolveGDX_path, "mapg_ild") %>% 
    mutate_if(is.factor, as.character) %>% 
    rename(
      Plant = g
      , Island = ild
    ) %>% 
    as_tibble()
  
  ## s_GEN - Generation by generating plant and block, GWh
  s_GEN <- rgdx.param(GEMsolveGDX_path, "s_GEN") %>% 
    mutate_if(is.factor, as.character) %>% 
    rename(
      Plant = g
      , Year = y
      , Period = t
      , LoadBlock = lb
    ) %>% 
    as_tibble()
  
  ## i_heatrate - heat rate (GJ/GWh)
  i_heatrate <- rgdx.param(GEMsolveGDX_path, "i_heatrate") %>% 
    mutate_if(is.factor, as.character) %>% 
    rename(
      Plant = g
    ) %>% 
    as_tibble()
  
  ## i_emissionFactors - CO2e emissions, toness CO2/PJ
  i_emissionFactors <- rgdx.param(GEMsolveGDX_path, "i_emissionFactors") %>% 
    mutate_if(is.factor, as.character) %>% 
    rename(
      Fuel = f
    ) %>% 
    as_tibble()
  
  ###############################################
  ### 5. Plots etc.                            ##
  ###############################################
  
  # Create Report output folder
  if(!file.exists(paste0("Output/", runName, "/Report"))){
    
    dir.create(paste0("Output/", runName, "/Report"))
    
  }
  
  # Solve report
  solveReport <- getResults(
    runName = runName
    , runVersionName  = runVersionName
    , variableName = "solveReport"
    , colNames = c("Experiments","Experiments2", "Steps", "ScenarioSets", "Variable", "Value")
  ) %>% 
    select(-Experiments2)
  
  write_csv(solveReport, paste0("Output/", runName, "/Report/solveReport.csv"))
  
  # Total cost
  totalCost <- getResults(
    runName = runName
    , runVersionName  = runVersionName
    , variableName = "s_TOTALCOST"
    , colNames = c("Experiments", "Steps", "ScenarioSets", "s_TOTALCOST")
  )
  
  png(file = paste0("Output/", runName, "/Report/totalCost.png")
      , width = 18.5, height = 10.5, units = "cm", res = 1000)
  
  print(
    totalCost %>% 
      mutate(Steps = str_to_title(Steps)) %>% 
      ggplot(aes(reorder(ScenarioSets, s_TOTALCOST), s_TOTALCOST, fill = Steps)) +
      geom_bar(stat = "identity", alpha = 0.6, width = 0.7) +
      scale_y_continuous(labels = scales::dollar_format()) +
      ggthemes::scale_fill_economist() +
      labs(x = "Scenario sets", y = "Total cost ($million)") + 
      facet_wrap(~Experiments)
  )
  
  dev.off()
  
  # Build schedule (new capacity)
  
  ## Build schedule by plant and year
  buildScheduleByPlantYr <- getResults(
    runName = runName
    , runVersionName  = runVersionName
    , variableName = "s_BUILD"
    , colNames = c("Experiments", "Steps", "ScenarioSets", "Plant", "Year", "s_BUILD")
  ) %>% 
    arrange(Experiments, Steps, ScenarioSets, Plant, Year)
  
  ## Build schedule total by year
  buildScheduleTotalYr <- buildScheduleByPlantYr %>%  
    group_by_at(vars(-c(Plant, s_BUILD))) %>% 
    summarise(
      s_BUILD = sum(s_BUILD)
    ) %>% 
    ungroup() %>% 
    group_by_at(vars(-c(Year, s_BUILD))) %>% 
    mutate(
      s_BUILD_cumsum = cumsum(s_BUILD)
    ) %>% 
    ungroup()
  
  png(file = paste0("Output/", runName, "/Report/buildScheduleTotalYr.png")
      , width = 18.5, height = 10.5, units = "cm", res = 1000)
  
  print(
    buildScheduleTotalYr %>% 
      ggplot(aes(Year, s_BUILD_cumsum, colour = ScenarioSets, group = ScenarioSets)) +
      geom_step(alpha = 0.3, lwd = 1) +
      scale_y_continuous(labels = scales::comma_format()) +
      ggthemes::scale_fill_economist() +
      labs(x = "Scenario sets", y = "Cumulative capacity built (MW)")
  )
  
  dev.off()
  
  ## Build schedule by technology and year
  buildScheduleByTechYr <- buildScheduleByPlantYr %>% 
    inner_join(
      mapg_k
      , by = "Plant"
    ) %>% 
    group_by_at(vars(-c(Plant, s_BUILD))) %>% 
    summarise(
      s_BUILD = sum(s_BUILD)
    ) %>% 
    arrange(Experiments, Steps, ScenarioSets, Technology, Year) %>% 
    ungroup() %>% 
    group_by_at(vars(-c(Year, s_BUILD))) %>% 
    mutate(
      s_BUILD_cumsum = cumsum(s_BUILD)
    ) %>% 
    ungroup()
  
  png(file = paste0("Output/", runName, "/Report/buildScheduleByTechYr.png")
      , width = 18.5, height = 10.5, units = "cm", res = 1000)
  
  print(
    buildScheduleByTechYr %>% 
      ggplot(aes(Year, s_BUILD_cumsum, colour = Technology, group = Technology)) +
      geom_step(alpha = 0.6, lwd = 1) +
      scale_y_continuous(labels = scales::comma_format()) +
      ggthemes::scale_colour_economist() +
      labs(x = "Scenario sets", y = "Cumulative capacity built (MW)") + 
      facet_wrap(~ScenarioSets)
  )
  
  dev.off()
  
  ## Build schedule by island and year
  buildScheduleByIslandYr <- buildScheduleByPlantYr %>% 
    inner_join(
      mapg_ild
      , by = "Plant"
    ) %>% 
    group_by_at(vars(-c(Plant, s_BUILD))) %>% 
    summarise(
      s_BUILD = sum(s_BUILD)
    ) %>% 
    arrange(Experiments, Steps, ScenarioSets, Island, Year) %>% 
    ungroup() %>% 
    group_by_at(vars(-c(Year, s_BUILD))) %>% 
    mutate(
      s_BUILD_cumsum = cumsum(s_BUILD)
    ) %>% 
    ungroup() 
  
  png(file = paste0("Output/", runName, "/Report/buildScheduleByIslandYr.png")
      , width = 18.5, height = 10.5, units = "cm", res = 1000)
  
  print(
    buildScheduleByIslandYr %>% 
      mutate(
        Island = str_to_upper(Island)
      ) %>% 
      ggplot(aes(Year, s_BUILD_cumsum, colour = Island, group = Island)) +
      geom_step(alpha = 0.6, lwd = 1) +
      scale_y_continuous(labels = scales::comma_format()) +
      ggthemes::scale_colour_economist() +
      labs(x = "Scenario sets", y = "Cumulative capacity built (MW)") + 
      facet_wrap(~ScenarioSets)
  )
  
  dev.off()
  
  # Installed capacity 
  
  ## Cumulative capacity by plant and year
  installedCapacityByPlantYr <- getResults(
    runName = runName
    , runVersionName  = runVersionName
    , variableName = "s_CAPACITY"
    , colNames = c("Experiments", "Steps", "ScenarioSets", "Plant", "Year", "s_CAPACITY")
  ) %>% 
    arrange(Experiments, Steps, ScenarioSets, Plant, Year)
  
  ## Total cumulative capacity by year
  installedCapacityTotalYr <- installedCapacityByPlantYr %>%  
    group_by_at(vars(-c(Plant, s_CAPACITY))) %>% 
    summarise(
      s_CAPACITY_cumsum = sum(s_CAPACITY)
    ) %>% 
    ungroup()
  
  png(file = paste0("Output/", runName, "/Report/installedCapacityTotalYr.png")
      , width = 18.5, height = 10.5, units = "cm", res = 1000)
  
  print(
    installedCapacityTotalYr %>% 
      ggplot(aes(Year, s_CAPACITY_cumsum, colour = ScenarioSets, group = ScenarioSets)) +
      geom_step(alpha = 0.3, lwd = 1) +
      scale_y_continuous(labels = scales::comma_format()) +
      ggthemes::scale_fill_economist() +
      labs(x = "Scenario sets", y = "Installed capacity (MW)")
  )
  
  dev.off()
  
  ## Installed capacity by technology and year
  installedCapacityByTechYr <- installedCapacityByPlantYr %>% 
    inner_join(
      mapg_k
      , by = "Plant"
    ) %>% 
    group_by_at(vars(-c(Plant, s_CAPACITY))) %>% 
    summarise(
      s_CAPACITY_cumsum = sum(s_CAPACITY)
    ) %>% 
    arrange(Experiments, Steps, ScenarioSets, Technology, Year) %>% 
    ungroup() 
  
  png(file = paste0("Output/", runName, "/Report/installedCapacityByTechYr.png")
      , width = 18.5, height = 10.5, units = "cm", res = 1000)
  
  print(
    installedCapacityByTechYr %>%  
      ggplot(aes(Year, s_CAPACITY_cumsum, fill = Technology, group = Technology)) +
      geom_area(alpha = 0.6) +
      scale_y_continuous(labels = scales::comma_format()) +
      # ggthemes::scale_fill_economist() +
      scale_fill_brewer(palette = "Set3") +
      labs(x = "Scenario sets", y = "Installed capacity (MW)") + 
      facet_wrap(~ScenarioSets)
  )
  
  dev.off()
  
  ## Installed capacity by fuel and year
  installedCapacityByFuelYr <- installedCapacityByPlantYr %>% 
    inner_join(
      mapg_f
      , by = "Plant"
    ) %>% 
    group_by_at(vars(-c(Plant, s_CAPACITY))) %>% 
    summarise(
      s_CAPACITY_cumsum = sum(s_CAPACITY)
    ) %>% 
    arrange(Experiments, Steps, ScenarioSets, Fuel, Year) %>% 
    ungroup() 
  
  png(file = paste0("Output/", runName, "/Report/installedCapacityByFuelYr.png")
      , width = 18.5, height = 10.5, units = "cm", res = 1000)
  
  print(
    installedCapacityByFuelYr %>%  
      ggplot(aes(Year, s_CAPACITY_cumsum, fill = Fuel, group = Fuel)) +
      geom_area(alpha = 0.6) +
      scale_y_continuous(labels = scales::comma_format()) +
      ggthemes::scale_fill_economist() +
      labs(x = "Scenario sets", y = "Installed capacity (MW)") + 
      facet_wrap(~ScenarioSets)
  )
  
  dev.off()
  
  ## Installed capacity by island and year
  installedCapacityByIslandYr <- installedCapacityByPlantYr %>% 
    inner_join(
      mapg_ild
      , by = "Plant"
    ) %>% 
    group_by_at(vars(-c(Plant, s_CAPACITY))) %>% 
    summarise(
      s_CAPACITY_cumsum = sum(s_CAPACITY)
    ) %>% 
    arrange(Experiments, Steps, ScenarioSets, Island, Year) %>% 
    ungroup() 
  
  png(file = paste0("Output/", runName, "/Report/installedCapacityByIslandYr.png")
      , width = 18.5, height = 10.5, units = "cm", res = 1000)
  
  print(
    installedCapacityByIslandYr %>% 
      mutate(
        Island = str_to_upper(Island)
      ) %>% 
      ggplot(aes(Year, s_CAPACITY_cumsum, fill = Island, group = Island)) +
      geom_area(alpha = 0.6) +
      scale_y_continuous(labels = scales::comma_format()) +
      ggthemes::scale_fill_economist() +
      labs(x = "Scenario sets", y = "Installed capacity (MW)") + 
      facet_wrap(~ScenarioSets)
  )
  
  dev.off()
  
  # Emissions
  
  ## CO2e emissions by plant and year
  emissionsByPlantYear <- s_GEN %>% 
    group_by_at(vars(-c(Period, LoadBlock, s_GEN))) %>% 
    summarise(s_GEN = sum(s_GEN)) %>% 
    ungroup() %>% 
    inner_join(
      i_heatrate
      , by = "Plant"
    ) %>% 
    inner_join(
      mapg_f
      , by = "Plant"
    ) %>% 
    inner_join(
      i_emissionFactors
      , by = "Fuel"
    ) %>% 
    mutate(
      CO2e_tonnes = 1e-6 * i_heatrate * s_GEN * i_emissionFactors
      , CO2e_mill_tonnes = 1e-6 * CO2e_tonnes
    ) %>% 
    select(-c(s_GEN, i_heatrate, i_emissionFactors))
  
  ## Total CO2e emissions by year
  emissionsTotalByYear <- emissionsByPlantYear %>% 
    group_by(steps, scenarioSets, scenarios, Year) %>% 
    summarise(CO2e_mill_tonnes = sum(CO2e_mill_tonnes))
  
  png(file = paste0("Output/", runName, "/Report/emissionsTotalByYear.png")
      , width = 18.5, height = 10.5, units = "cm", res = 1000)
  
  print(
    emissionsTotalByYear %>% 
      ggplot(aes(Year, CO2e_mill_tonnes, colour = scenarioSets, group = scenarioSets)) +
      geom_line(lwd = 1, alpha = 0.6) +
      scale_y_continuous(labels = scales::comma_format()) +
      ggthemes::scale_colour_economist() +
      labs(x = "Year", y = "CO2e emissions (million tonnes)") 
  )
  
  dev.off()
  
  ## CO2e emissions by fuel and year
  emissionsByFuelYear <- emissionsByPlantYear %>% 
    group_by(steps, scenarioSets, scenarios, Fuel, Year) %>% 
    summarise(CO2e_mill_tonnes = sum(CO2e_mill_tonnes))
  
  png(file = paste0("Output/", runName, "/Report/emissionsByFuelYear.png")
      , width = 18.5, height = 10.5, units = "cm", res = 1000)
  
  print(
    emissionsByFuelYear %>% 
      ggplot(aes(Year, CO2e_mill_tonnes, fill = Fuel, group = Fuel)) +
      geom_area(alpha = 0.6) +
      scale_y_continuous(labels = scales::comma_format()) +
      ggthemes::scale_fill_economist() +
      labs(x = "Year", y = "CO2e emissions (million tonnes)") + 
      facet_wrap(~scenarioSets)
  )
  
  dev.off()
  
}
