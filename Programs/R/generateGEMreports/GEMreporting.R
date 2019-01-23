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
### 3. Get results                           ##
###############################################

# Function for getting all results for single variable
getResults <- function(runName, variableName, colNames){
  
  reportPath <- paste0(
    "Output/"
    , runName
    , "/GDX/allExperimentsReportOutput - "
    , runName
    , ".gdx")
  
  reportItem <- rgdx.param(reportPath, variableName) %>% 
    mutate_if(is.factor, as.character) %>% 
    as_tibble()
  
  colnames(reportItem) <- colNames
  
  return(reportItem)
  
}

# Function for getting all results for single variable from multiple runs
getResultsAll <- function(runName, variableName, colNames){
  
  resultsAll <- tibble()
  
  for(i in 1:length(runName)){
    
    reportPath <- paste0(
      "Output/"
      , runName[i]
      , "/GDX/allExperimentsReportOutput - "
      , runName[i]
      , ".gdx")
    
    reportItem <- rgdx.param(reportPath, variableName) %>% 
      mutate_if(is.factor, as.character) %>% 
      as_tibble()
    
    colnames(reportItem) <- colNames
    
    reportItem$runName <- runName[i]
    
    resultsAll <- bind_rows(resultsAll, reportItem)
    
  }
  
  return(resultsAll)
  
}

###############################################
### 4. Get external vars                     ##
###############################################

getExternalVars <- function(runName){
  
  # Read in GEMsolve output GDX to use some subsets etc.
  GEMsolveGDX_path <- paste0("Output/", runName, "/GEMsolve_", runName, ".gdx")
  
  tmp <- list()
  
  ## Map generation to technology
  tmp$mapg_k <- rgdx.set(GEMsolveGDX_path, "mapg_k") %>% 
    mutate_if(is.factor, as.character) %>% 
    rename(
      Plant = g
      , Technology = k
    ) %>% 
    as_tibble()
  
  ## Map generation to fuel
  tmp$mapg_f <- rgdx.set(GEMsolveGDX_path, "mapg_f") %>% 
    mutate_if(is.factor, as.character) %>% 
    rename(
      Plant = g
      , Fuel = f
    ) %>% 
    as_tibble()
  
  ## Map generation to island
  tmp$mapg_ild <- rgdx.set(GEMsolveGDX_path, "mapg_ild") %>% 
    mutate_if(is.factor, as.character) %>% 
    rename(
      Plant = g
      , Island = ild
    ) %>% 
    as_tibble()
  
  # ## s_GEN - Generation by generating plant and block, GWh
  # tmp$s_GEN <- rgdx.param(GEMsolveGDX_path, "s_GEN") %>% 
  #   mutate_if(is.factor, as.character) %>% 
  #   rename(
  #     Plant = g
  #     , Year = y
  #     , Period = t
  #     , LoadBlock = lb
  #   ) %>% 
  #   as_tibble()
  
  ## i_heatrate - heat rate (GJ/GWh)
  tmp$i_heatrate <- rgdx.param(GEMsolveGDX_path, "i_heatrate") %>% 
    mutate_if(is.factor, as.character) %>% 
    rename(
      Plant = g
    ) %>% 
    as_tibble()
  
  ## i_emissionFactors - CO2e emissions, toness CO2/PJ
  tmp$i_emissionFactors <- rgdx.param(GEMsolveGDX_path, "i_emissionFactors") %>% 
    mutate_if(is.factor, as.character) %>% 
    rename(
      Fuel = f
    ) %>% 
    as_tibble()
  
  return(tmp)
  
}

createGEMreports <- function(runName){
  
  # Set output path
  outPath <- paste0("Output/", runName)
  
  # Load external variables
  extVars <- getExternalVars(runName = runName)
  
  ###############################################
  ### 5. Plots/reports                         ##
  ###############################################
  
  # Solve report
  solveReport <- getResults(
    runName = runName
    , variableName = "solveReport"
    , colNames = c("Experiments","Experiments2", "Steps", "ScenarioSets", "Variable", "Value")
  ) %>% 
    select(-Experiments2)
  
  write_csv(solveReport, paste0(outPath, "/Report/solveReport.csv"))
  
  # Total cost
  totalCost <- getResults(
    runName = runName
    , variableName = "s_TOTALCOST"
    , colNames = c("Experiments", "Steps", "ScenarioSets", "s_TOTALCOST")
  )
  
  totalCostAvgDisp <-  totalCost %>% 
    filter(Steps == "dispatch") %>% 
    group_by(Experiments) %>% 
    summarise(s_TOTALCOST = mean(s_TOTALCOST)) %>% 
    mutate(
      ScenarioSets = "AvgDispatch"
      , grp = "average"
    )
  
  png(file = paste0(outPath, "/Report/totalCost.png")
      , width = 18.5, height = 10.5, units = "cm", res = 1000)
  
  print(
    totalCost %>% 
      filter(Steps == "dispatch") %>% 
      mutate(grp = "disp") %>% 
      union_all(
        totalCostAvgDisp
      ) %>% 
      ggplot(aes(reorder(ScenarioSets, s_TOTALCOST), s_TOTALCOST, fill = grp)) +
      geom_bar(stat = "identity", alpha = 0.6, width = 0.7) +
      scale_y_continuous(labels = scales::dollar_format()) +
      scale_fill_manual(
        values = c("disp" = "lightsteelblue", "average" = "steelblue")
        , guide = FALSE) +
      labs(x = "Scenario sets", y = "Total cost ($million)") + 
      facet_wrap(~Experiments) +
      geom_text(
        data = totalCostAvgDisp
        , aes(label = paste0(scales::dollar(s_TOTALCOST, accuracy = 1), " mill"), vjust = 1.25)
        , colour = "white"
        , size = 2
      )
  )
  
  dev.off()
  
  # Build schedule (new capacity)
  
  ## Build schedule by plant and year
  buildScheduleByPlantYr <- getResults(
    runName = runName
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
  
  buildScheduleTotalYrAvgDisp <-  buildScheduleTotalYr %>% 
    filter(Steps == "dispatch") %>% 
    group_by(Experiments, Year) %>% 
    summarise(s_BUILD = mean(s_BUILD)) %>% 
    mutate(
      s_BUILD_cumsum = cumsum(s_BUILD)
      , ScenarioSets = "AvgDispatch"
      , grp = "average"
    )
  
  png(file = paste0(outPath, "/Report/buildScheduleTotalYr.png")
      , width = 18.5, height = 10.5, units = "cm", res = 1000)
  
  print(
    buildScheduleTotalYr %>% 
      filter(Steps == "dispatch") %>% 
      mutate(grp = "disp") %>% 
      union_all(
        buildScheduleTotalYrAvgDisp
      ) %>% 
      ggplot(aes(Year, s_BUILD_cumsum, colour = grp, group = grp)) +
      geom_step(alpha = 0.6, lwd = 1) +
      scale_y_continuous(labels = scales::comma_format()) +
      ggthemes::scale_fill_economist() +
      labs(x = "Year", y = "Cumulative capacity built (MW)") +
      scale_colour_manual(
        values = c("disp" = "lightsteelblue", "average" = "steelblue")
        , guide = FALSE
      ) + 
      facet_wrap(~Experiments) +
      expand_limits(y = 0)
  )
  
  dev.off()
  
  ## Build schedule by technology and year
  buildScheduleByTechYr <- buildScheduleByPlantYr %>% 
    inner_join(
      extVars$mapg_k
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
  
  buildScheduleByTechYrAvgDisp <-  buildScheduleByTechYr %>% 
    filter(Steps == "dispatch") %>% 
    group_by(Experiments, Year, Technology) %>% 
    summarise(s_BUILD = mean(s_BUILD)) %>% 
    ungroup() %>% 
    group_by(Experiments, Technology) %>% 
    mutate(
      s_BUILD_cumsum = cumsum(s_BUILD)
      , ScenarioSets = "AvgDispatch"
      , grp = "average"
    )
  
  png(file = paste0(outPath, "/Report/buildScheduleByTechYr.png")
      , width = 18.5, height = 10.5, units = "cm", res = 1000)
  
  print(
    buildScheduleByTechYr %>% 
      filter(Steps == "dispatch") %>% 
      mutate(grp = "disp") %>% 
      union_all(
        buildScheduleByTechYrAvgDisp
      ) %>% 
      ggplot(aes(Year, s_BUILD_cumsum, colour = grp, group = grp)) +
      geom_step(alpha = 0.6, lwd = 1) +
      scale_y_continuous(labels = scales::comma_format()) +
      ggthemes::scale_fill_economist() +
      labs(x = "Year", y = "Cumulative capacity built (MW)") +
      scale_colour_manual(
        values = c("disp" = "lightsteelblue", "average" = "steelblue")
        , guide = FALSE
      ) + 
      facet_wrap(~Experiments + Technology) +
      expand_limits(y = 0)
  )
  
  dev.off()
  
  ## Build schedule by island and year
  
  buildScheduleByIslandYr <- buildScheduleByPlantYr %>% 
    inner_join(
      extVars$mapg_ild
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
  
  buildScheduleByIslandYrAvgDisp <-  buildScheduleByIslandYr %>% 
    filter(Steps == "dispatch") %>% 
    group_by(Experiments, Year, Island) %>% 
    summarise(s_BUILD = mean(s_BUILD)) %>% 
    ungroup() %>% 
    group_by(Experiments, Island) %>% 
    mutate(
      s_BUILD_cumsum = cumsum(s_BUILD)
      , ScenarioSets = "AvgDispatch"
      , grp = "average"
    )
  
  png(file = paste0(outPath, "/Report/buildScheduleByIslandYr.png")
      , width = 18.5, height = 10.5, units = "cm", res = 1000)
  
  print(
    buildScheduleByIslandYr %>% 
      filter(Steps == "dispatch") %>% 
      mutate(grp = "disp") %>% 
      union_all(
        buildScheduleByIslandYrAvgDisp
      ) %>% 
      mutate(
        Island = str_to_upper(Island)
      ) %>% 
      ggplot(aes(Year, s_BUILD_cumsum, colour = grp, group = grp)) +
      geom_step(alpha = 0.6, lwd = 1) +
      scale_y_continuous(labels = scales::comma_format()) +
      ggthemes::scale_fill_economist() +
      labs(x = "Year", y = "Cumulative capacity built (MW)") +
      scale_colour_manual(
        values = c("disp" = "lightsteelblue", "average" = "steelblue")
        , guide = FALSE
      ) + 
      facet_wrap(~Experiments + Island) +
      expand_limits(y = 0)
  )
  
  dev.off()
  
  # Installed capacity 
  
  ## Cumulative capacity by plant and year
  installedCapacityByPlantYr <- getResults(
    runName = runName
    , variableName = "s_CAPACITY"
    , colNames = c("Experiments", "Steps", "ScenarioSets", "Plant", "Year", "s_CAPACITY")
  ) %>% 
    arrange(Experiments, Steps, ScenarioSets, Plant, Year)
  
  ## Total cumulative capacity by year
  installedCapacityTotalYr <- installedCapacityByPlantYr %>%  
    group_by_at(vars(-c(Plant, s_CAPACITY))) %>% 
    summarise(
      s_CAPACITY = sum(s_CAPACITY)
    ) %>% 
    ungroup() 
  
  installedCapacityTotalYrAvgDisp <-  installedCapacityTotalYr %>% 
    filter(Steps == "dispatch") %>% 
    group_by(Experiments, Year) %>% 
    summarise(s_CAPACITY = mean(s_CAPACITY)) %>% 
    mutate(
      ScenarioSets = "AvgDispatch"
      , grp = "average"
    )
  
  png(file = paste0(outPath, "/Report/installedCapacityTotalYr.png")
      , width = 18.5, height = 10.5, units = "cm", res = 1000)
  
  print(
    installedCapacityTotalYr %>% 
      filter(Steps == "dispatch") %>% 
      mutate(grp = "disp") %>% 
      union_all(
        installedCapacityTotalYrAvgDisp
      ) %>% 
      ggplot(aes(Year, s_CAPACITY, colour = grp, group = grp)) +
      geom_step(alpha = 0.6, lwd = 1) +
      scale_y_continuous(labels = scales::comma_format()) +
      ggthemes::scale_fill_economist() +
      labs(x = "Year", y = "Installed capacity (MW)") +
      scale_colour_manual(
        values = c("disp" = "lightsteelblue", "average" = "steelblue")
        , guide = FALSE
      ) + 
      facet_wrap(~Experiments)
  )
  
  dev.off()
  
  # ## Installed capacity by technology and year
  # installedCapacityByTechYr <- installedCapacityByPlantYr %>% 
  #   inner_join(
  #     extVars$mapg_k
  #     , by = "Plant"
  #   ) %>% 
  #   group_by_at(vars(-c(Plant, s_CAPACITY))) %>% 
  #   summarise(
  #     s_CAPACITY = sum(s_CAPACITY)
  #   ) %>% 
  #   arrange(Experiments, Steps, ScenarioSets, Technology, Year) %>% 
  #   ungroup()
  # 
  # installedCapacityByTechYrAvgDisp <-  installedCapacityByTechYr %>% 
  #   filter(Steps == "dispatch") %>% 
  #   group_by(Experiments, Year, Technology) %>% 
  #   summarise(s_CAPACITY = mean(s_CAPACITY)) %>% 
  #   arrange(Experiments, Technology, Year) %>% 
  #   ungroup() %>% 
  #   mutate(
  #     ScenarioSets = "AvgDispatch"
  #     , grp = "average"
  #   )
  # 
  # png(file = paste0(outPath, "/Report/installedCapacityByTechYr.png")
  #     , width = 18.5, height = 10.5, units = "cm", res = 1000)
  # 
  # print(
  #   installedCapacityByTechYrAvgDisp %>% 
  #     ggplot(aes(Year, s_CAPACITY, fill = Technology, group = Technology)) +
  #     geom_area(alpha = 0.6) +
  #     scale_y_continuous(labels = scales::comma_format()) +
  #     scale_fill_brewer(palette = "Set3") +
  #     labs(x = "Year", y = "Installed capacity (MW)") + 
  #     facet_wrap(~Experiments)
  # )
  # 
  # dev.off()
  
  ## Installed capacity by fuel and year
  installedCapacityByFuelYr <- installedCapacityByPlantYr %>% 
    inner_join(
      extVars$mapg_f
      , by = "Plant"
    ) %>% 
    group_by_at(vars(-c(Plant, s_CAPACITY))) %>% 
    summarise(
      s_CAPACITY = sum(s_CAPACITY)
    ) %>% 
    arrange(Experiments, Steps, ScenarioSets, Fuel, Year) %>% 
    ungroup()
  
  installedCapacityByFuelYrAvgDisp <-  installedCapacityByFuelYr %>% 
    filter(Steps == "dispatch") %>% 
    group_by(Experiments, Year, Fuel) %>% 
    summarise(s_CAPACITY = mean(s_CAPACITY)) %>% 
    arrange(Experiments, Fuel, Year) %>% 
    ungroup() %>% 
    mutate(
      ScenarioSets = "AvgDispatch"
      , grp = "average"
    )
  
  png(file = paste0(outPath, "/Report/installedCapacityByFuelYr.png")
      , width = 18.5, height = 10.5, units = "cm", res = 1000)
  
  print(
    installedCapacityByFuelYrAvgDisp %>% 
      ggplot(aes(Year, s_CAPACITY, fill = Fuel, group = Fuel)) +
      geom_area(alpha = 0.6) +
      scale_y_continuous(labels = scales::comma_format()) +
      ggthemes::scale_fill_economist() +
      labs(x = "Year", y = "Installed capacity (MW)") + 
      facet_wrap(~Experiments)
  )
  
  dev.off()
  
  ## Installed capacity by island and year
  installedCapacityByIslandYr <- installedCapacityByPlantYr %>% 
    inner_join(
      extVars$mapg_ild
      , by = "Plant"
    ) %>% 
    group_by_at(vars(-c(Plant, s_CAPACITY))) %>% 
    summarise(
      s_CAPACITY = sum(s_CAPACITY)
    ) %>% 
    arrange(Experiments, Steps, ScenarioSets, Island, Year) %>% 
    ungroup()
  
  installedCapacityByIslandYrAvgDisp <-  installedCapacityByIslandYr %>% 
    filter(Steps == "dispatch") %>% 
    group_by(Experiments, Year, Island) %>% 
    summarise(s_CAPACITY = mean(s_CAPACITY)) %>% 
    arrange(Experiments, Island, Year) %>% 
    ungroup() %>% 
    mutate(
      ScenarioSets = "AvgDispatch"
      , grp = "average"
    )
  
  png(file = paste0(outPath, "/Report/installedCapacityByIslandYr.png")
      , width = 18.5, height = 10.5, units = "cm", res = 1000)
  
  print(
    installedCapacityByIslandYrAvgDisp %>% 
      mutate(Island = str_to_upper(Island)) %>% 
      ggplot(aes(Year, s_CAPACITY, fill = Island, group = Island)) +
      geom_area(alpha = 0.6) +
      scale_y_continuous(labels = scales::comma_format()) +
      ggthemes::scale_fill_economist() +
      labs(x = "Year", y = "Installed capacity (MW)") + 
      facet_wrap(~Experiments)
  )
  
  dev.off()
  
  # Emissions
  
  s_GEN <- getResults(
    runName = runName
    , variableName = "s_GEN"
    , colNames = c(
      "Experiments", "Steps", "ScenarioSets", 
      "Plant", "Year", "Period", "LoadBlock",
      "scenarios", "s_GEN"
    )
  ) 
  
  ## CO2e emissions by plant and year
  emissionsByPlantYear <- s_GEN %>% 
    group_by_at(vars(-c(Period, LoadBlock, s_GEN))) %>% 
    summarise(s_GEN = sum(s_GEN)) %>% 
    ungroup() %>% 
    inner_join(
      extVars$i_heatrate
      , by = "Plant"
    ) %>% 
    inner_join(
      extVars$mapg_f
      , by = "Plant"
    ) %>% 
    inner_join(
      extVars$i_emissionFactors
      , by = "Fuel"
    ) %>% 
    mutate(
      CO2e_tonnes = 1e-6 * i_heatrate * s_GEN * i_emissionFactors
      , CO2e_mill_tonnes = 1e-6 * CO2e_tonnes
    ) %>% 
    select(-c(s_GEN, i_heatrate, i_emissionFactors))
  
  ## Total CO2e emissions by year
  emissionsTotalByYear <- emissionsByPlantYear %>% 
    group_by(Experiments, Steps, ScenarioSets, scenarios, Year) %>% 
    summarise(CO2e_mill_tonnes = sum(CO2e_mill_tonnes))
  
  emissionsTotalByYearAvgDisp <-  emissionsTotalByYear %>% 
    filter(Steps == "dispatch") %>% 
    group_by(Experiments, Year) %>% 
    summarise(CO2e_mill_tonnes = mean(CO2e_mill_tonnes)) %>% 
    mutate(
      ScenarioSets = "AvgDispatch"
      , grp = "average"
    )
  
  png(file = paste0(outPath, "/Report/emissionsTotalByYear.png")
      , width = 18.5, height = 10.5, units = "cm", res = 1000)
  
  print(
    emissionsTotalByYear %>% 
      filter(Steps == "dispatch") %>% 
      mutate(grp = "disp") %>% 
      union_all(
        emissionsTotalByYearAvgDisp
      ) %>% 
      ggplot(aes(Year, CO2e_mill_tonnes, colour = grp, group = scenarios)) +
      geom_line(lwd = 1, alpha = 0.6) +
      scale_y_continuous(labels = scales::comma_format()) +
      labs(x = "Year", y = "CO2e emissions (million tonnes)") +
      scale_colour_manual(
        values = c("disp" = "lightsteelblue", "average" = "steelblue")
        , guide = FALSE
      ) +
      expand_limits(y = 0)
  )
  
  dev.off()
  
  ## CO2e emissions by fuel and year
  emissionsByFuelYear <- emissionsByPlantYear %>% 
    group_by(Experiments, Steps, ScenarioSets, scenarios, Fuel, Year) %>% 
    summarise(CO2e_mill_tonnes = sum(CO2e_mill_tonnes))
  
  emissionsByFuelYearAvgDisp <-  emissionsByFuelYear %>% 
    filter(Steps == "dispatch") %>% 
    group_by(Experiments, Fuel, Year) %>% 
    summarise(CO2e_mill_tonnes = mean(CO2e_mill_tonnes)) %>% 
    mutate(
      ScenarioSets = "AvgDispatch"
      , grp = "average"
    )
  
  png(file = paste0(outPath, "/Report/emissionsByFuelYear.png")
      , width = 18.5, height = 10.5, units = "cm", res = 1000)
  
  print(
    emissionsByFuelYearAvgDisp %>% 
      ggplot(aes(Year, CO2e_mill_tonnes, fill = Fuel, group = Fuel)) +
      geom_area(alpha = 0.6) +
      scale_y_continuous(labels = scales::comma_format()) +
      ggthemes::scale_fill_economist() +
      labs(x = "Year", y = "CO2e emissions (million tonnes)") + 
      facet_wrap(~Experiments)
  )
  
  dev.off()
  
}
