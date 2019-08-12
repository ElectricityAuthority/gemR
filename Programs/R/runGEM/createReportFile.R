### Create reporting dataset for run (seperate from plotting!)
# library(tidyverse)
library(gdxtools)
library(rlist)

# runName <- "standard_run"

# Function for reading variables from GEMsolve GDX
getGEMsolveVar <- function(x){
  
  x %>% 
    mutate_if(is.factor, as.character) %>% 
    as_tibble()
  
}

# Generate report output for run
createReportFile <- function(runName){
  
  # Path to output folder
  outPath <- paste0("Output/", runName)
  
  # Path to report output
  reportPath <- paste0("Output/", runName, "/GDX/temp/RepOut/")
  
  # Path to GEMsolve GDX
  GEMsolveGDXPath <- paste0("Output/", runName, "/GEMsolve_", runName, ".gdx")
  
  # Read GEMsolve GDX
  solveGDX <- gdx(GEMsolveGDXPath)
  
  # Get all experiment file names from inside "temp/RepOut" folder
  experiments <- list.files(reportPath) %>% 
    str_replace(".gdx", "")
  
  # Loop through and output data for all experiments
  for(ex in 1:length(experiments)){
    
    # Read report GDX
    repGDX <- gdx(paste0(reportPath, experiments[ex], ".gdx"))
    
    # Vector of report parameters to return
    reportParams <- c("solveReport", "s_TOTALCOST", "s_BUILD", "s_CAPACITY", "s_GEN")
    
    # Initialise list for storing items from report GDX
    repList <- lst()
    
    # Loop through and extract items
    for(i in 1:length(reportParams)){
      
      repList[[reportParams[i]]] <- repGDX[reportParams[i]] %>% 
        rename_at(vars(value), function(x) reportParams[i])
      
    }
    
    # Parameters to return from GEMsolve
    GEMsolveParams <- c("mapg_k", "mapg_f", "mapg_ild", 
                        "i_heatrate", "i_emissionFactors")
    
    # Initialise list for mapping and other external variables
    externalVars <- lst()
    
    for(i in 1:length(GEMsolveParams)){
      
      externalVars[[GEMsolveParams[i]]] <- getGEMsolveVar(x = solveGDX[GEMsolveParams[i]])
      
    }
    
    # Initialise output list
    outputList <- lst()
    
    # Output solve report
    outputList[["solveReport"]][[experiments[ex]]] <- repList[["solveReport"]] %>% 
      select(-experiments) %>% 
      rename(Variable = V4) %>% 
      as_tibble()
    
    # Output total cost
    outputList[["totalCost"]][[experiments[ex]]] <- repList[["s_TOTALCOST"]] %>%
      as_tibble()
    
    # Output total cost (dispatch only)
    outputList[["totalCostDisp"]][[experiments[ex]]] <- outputList[["totalCost"]][[experiments[ex]]] %>% 
      filter(steps == "dispatch") %>% 
      mutate(grp = "disp") %>% 
      # Include an average dispatch row
      union_all(
        outputList[["totalCost"]][[experiments[ex]]] %>%
          filter(steps == "dispatch") %>% 
          summarise(s_TOTALCOST = mean(s_TOTALCOST)) %>% 
          mutate(
            scenarioSets = "AvgDispatch"
            , steps = "dispatch"
            , grp = "average"
          )
      )
    
    # Output build (by plant and year)
    outputList[["buildByPlantYr"]][[experiments[ex]]] <- repList[["s_BUILD"]] %>% 
      # Join external variables to 'master' table
      left_join(externalVars$mapg_k, by = "g") %>% 
      left_join(externalVars$mapg_f, by = "g") %>% 
      left_join(externalVars$mapg_ild, by = "g") %>% 
      as_tibble()
    
    # Output build (total by year)
    outputList[["buildTotalYr"]][[experiments[ex]]] <- outputList[["buildByPlantYr"]][[experiments[ex]]] %>% 
      group_by(steps, scenarioSets, y) %>% 
      summarise(s_BUILD = sum(s_BUILD)) %>% 
      ungroup() %>% 
      group_by(steps, scenarioSets) %>% 
      arrange(steps, scenarioSets, y) %>% 
      mutate(s_BUILD_cumsum = cumsum(s_BUILD)) %>% 
      ungroup()
    
    # Output build (by technology and year - dispatch only)
    outputList[["buildTotalYrDisp"]][[experiments[ex]]] <- outputList[["buildTotalYr"]][[experiments[ex]]] %>% 
      filter(steps == "dispatch") %>%  
      mutate(grp = "disp") %>% 
      # Include an average dispatch 'scenario'
      union_all(
        outputList[["buildTotalYr"]][[experiments[ex]]] %>%
          filter(steps == "dispatch") %>% 
          group_by(y) %>% 
          summarise(
            s_BUILD = mean(s_BUILD)
            , s_BUILD_cumsum = mean(s_BUILD_cumsum)
          ) %>% 
          ungroup() %>% 
          mutate(
            scenarioSets = "AvgDispatch"
            , steps = "dispatch"
            , grp = "average"
          )
      )
    
    # Output build (by technology and year)
    outputList[["buildByTechYr"]][[experiments[ex]]] <- outputList[["buildByPlantYr"]][[experiments[ex]]] %>% 
      group_by(steps, scenarioSets, k, y) %>% 
      summarise(s_BUILD = sum(s_BUILD)) %>% 
      ungroup() %>% 
      group_by(steps, scenarioSets, k) %>% 
      arrange(steps, scenarioSets, k, y) %>% 
      mutate(s_BUILD_cumsum = cumsum(s_BUILD)) %>% 
      ungroup()
    
    # Output build (by technology and year - dispatch only)
    outputList[["buildByTechYrDisp"]][[experiments[ex]]] <- outputList[["buildByTechYr"]][[experiments[ex]]] %>% 
      filter(steps == "dispatch") %>%  
      mutate(grp = "disp") %>% 
      # Include an average dispatch 'scenario'
      union_all(
        outputList[["buildByTechYr"]][[experiments[ex]]] %>%
          filter(steps == "dispatch") %>% 
          group_by(k, y) %>% 
          summarise(
            s_BUILD = mean(s_BUILD)
            , s_BUILD_cumsum = mean(s_BUILD_cumsum)
          ) %>% 
          ungroup() %>% 
          mutate(
            scenarioSets = "AvgDispatch"
            , steps = "dispatch"
            , grp = "average"
          )
      )
    
    # Output build (by fuel and year)
    outputList[["buildByFuelYr"]][[experiments[ex]]] <- outputList[["buildByPlantYr"]][[experiments[ex]]] %>% 
      group_by(steps, scenarioSets, f, y) %>% 
      summarise(s_BUILD = sum(s_BUILD)) %>% 
      ungroup() %>% 
      group_by(steps, scenarioSets, f) %>% 
      arrange(steps, scenarioSets, f, y) %>% 
      mutate(s_BUILD_cumsum = cumsum(s_BUILD)) %>% 
      ungroup()
    
    # Output build (by fuel and year - dispatch only)
    outputList[["buildByFuelYrDisp"]][[experiments[ex]]] <- outputList[["buildByFuelYr"]][[experiments[ex]]] %>%  
      filter(steps == "dispatch") %>% 
      mutate(grp = "disp") %>% 
      # Include an average dispatch 'scenario'
      union_all(
        outputList[["buildByFuelYr"]][[experiments[ex]]] %>%
          filter(steps == "dispatch") %>% 
          group_by(f, y) %>% 
          summarise(
            s_BUILD = mean(s_BUILD)
            , s_BUILD_cumsum = mean(s_BUILD_cumsum)
          ) %>% 
          ungroup() %>% 
          mutate(
            scenarioSets = "AvgDispatch"
            , steps = "dispatch"
            , grp = "average"
          )
      )
    
    
    # Output capacity (by plant and year)
    outputList[["capacityByPlantYr"]][[experiments[ex]]] <- repList[["s_CAPACITY"]] %>% 
      # Join external variables to 'master' table
      left_join(externalVars$mapg_k, by = "g") %>% 
      left_join(externalVars$mapg_f, by = "g") %>% 
      left_join(externalVars$mapg_ild, by = "g") %>% 
      as_tibble()
    
    # Output capacity (total by year)
    outputList[["capacityTotalYr"]][[experiments[ex]]] <- outputList[["capacityByPlantYr"]][[experiments[ex]]] %>% 
      group_by(steps, scenarioSets, y) %>% 
      summarise(s_CAPACITY = sum(s_CAPACITY)) %>% 
      ungroup()
    
    # Output capacity (by technology and year - dispatch only)
    outputList[["capacityTotalYrDisp"]][[experiments[ex]]] <- outputList[["capacityTotalYr"]][[experiments[ex]]] %>%  
      filter(steps == "dispatch") %>% 
      mutate(grp = "disp") %>% 
      # Include an average dispatch 'scenario'
      union_all(
        outputList[["capacityTotalYr"]][[experiments[ex]]] %>%
          filter(steps == "dispatch") %>% 
          group_by(y) %>% 
          summarise(s_CAPACITY = mean(s_CAPACITY)) %>% 
          ungroup() %>% 
          mutate(
            scenarioSets = "AvgDispatch"
            , steps = "dispatch"
            , grp = "average"
          )
      )
    
    # Output capacity (by technology and year)
    outputList[["capacityByTechYr"]][[experiments[ex]]] <- outputList[["capacityByPlantYr"]][[experiments[ex]]] %>% 
      group_by(steps, scenarioSets, k, y) %>% 
      summarise(s_CAPACITY = sum(s_CAPACITY)) %>% 
      ungroup()
    
    # Output capacity (by technology and year - dispatch only)
    outputList[["capacityByTechYrDisp"]][[experiments[ex]]] <- outputList[["capacityByTechYr"]][[experiments[ex]]] %>%  
      filter(steps == "dispatch") %>% 
      mutate(grp = "disp") %>% 
      # Include an average dispatch 'scenario'
      union_all(
        outputList[["capacityByTechYr"]][[experiments[ex]]] %>%
          filter(steps == "dispatch") %>% 
          group_by(k, y) %>% 
          summarise(s_CAPACITY = mean(s_CAPACITY)) %>% 
          ungroup() %>% 
          mutate(
            scenarioSets = "AvgDispatch"
            , steps = "dispatch"
            , grp = "average"
          )
      )
    
    # Output capacity (by fuel and year)
    outputList[["capacityByFuelYr"]][[experiments[ex]]] <- outputList[["capacityByPlantYr"]][[experiments[ex]]] %>% 
      group_by(steps, scenarioSets, f, y) %>% 
      summarise(s_CAPACITY = sum(s_CAPACITY)) %>% 
      ungroup()
    
    # Output capacity (by fuel and year - dispatch only)
    outputList[["capacityByFuelYrDisp"]][[experiments[ex]]] <- outputList[["capacityByFuelYr"]][[experiments[ex]]] %>%  
      filter(steps == "dispatch") %>% 
      mutate(grp = "disp") %>% 
      # Include an average dispatch 'scenario'
      union_all(
        outputList[["capacityByFuelYr"]][[experiments[ex]]] %>%
          filter(steps == "dispatch") %>% 
          group_by(f, y) %>% 
          summarise(s_CAPACITY = mean(s_CAPACITY)) %>% 
          ungroup() %>% 
          mutate(
            scenarioSets = "AvgDispatch"
            , steps = "dispatch"
            , grp = "average"
          )
      )
    
    # Output generation (including emissions - by plant and year)
    outputList[["generationByPlantYr"]][[experiments[ex]]] <- repList[["s_GEN"]] %>%
      group_by(steps, scenarioSets, scenarios, g, y) %>%
      summarise(s_GEN = sum(s_GEN)) %>% 
      ungroup() %>% 
      left_join(externalVars$mapg_k, by = "g") %>% 
      left_join(externalVars$mapg_f, by = "g") %>% 
      left_join(externalVars$mapg_ild, by = "g") %>% 
      left_join(
        externalVars$i_heatrate %>% 
          rename(i_heatrate = value)
        , by = "g"
      ) %>% 
      left_join(
        externalVars$i_emissionFactors %>% 
          rename(i_emissionFactors = value)
        , by = "f"
      ) %>%  
      mutate(
        CO2e_tonnes = 1e-6 * i_heatrate * s_GEN * i_emissionFactors
        , CO2e_mill_tonnes = 1e-6 * CO2e_tonnes
      ) %>% 
      as_tibble()
    
    # Output generation (total by year)
    outputList[["generationTotalYr"]][[experiments[ex]]] <- outputList[["generationByPlantYr"]][[experiments[ex]]] %>% 
      group_by(steps, scenarioSets, y) %>% 
      summarise(
        s_GEN  = sum(s_GEN)
        , CO2e_tonnes  = sum(CO2e_tonnes, na.rm = TRUE)
        , CO2e_mill_tonnes = sum(CO2e_mill_tonnes, na.rm = TRUE)
      ) %>% 
      ungroup()
    
    # Output generation (by technology and year - dispatch only)
    outputList[["generationTotalYrDisp"]][[experiments[ex]]] <- outputList[["generationTotalYr"]][[experiments[ex]]] %>%  
      filter(steps == "dispatch") %>% 
      mutate(grp = "disp") %>% 
      # Include an average dispatch 'scenario'
      union_all(
        outputList[["generationTotalYr"]][[experiments[ex]]] %>%
          filter(steps == "dispatch") %>% 
          group_by(y) %>% 
          summarise(
            s_GEN  = mean(s_GEN)
            , CO2e_tonnes  = mean(CO2e_tonnes, na.rm = TRUE)
            , CO2e_mill_tonnes = mean(CO2e_mill_tonnes, na.rm = TRUE)  ) %>% 
          ungroup() %>% 
          mutate(
            scenarioSets = "AvgDispatch"
            , steps = "dispatch"
            , grp = "average"
          )
      )
    
    # Output generation (by technology and year)
    outputList[["generationByTechYr"]][[experiments[ex]]] <- outputList[["generationByPlantYr"]][[experiments[ex]]] %>% 
      group_by(steps, scenarioSets, k, y) %>% 
      summarise(
        s_GEN  = sum(s_GEN)
        , CO2e_tonnes  = sum(CO2e_tonnes, na.rm = TRUE)
        , CO2e_mill_tonnes = sum(CO2e_mill_tonnes, na.rm = TRUE)
      ) %>% 
      ungroup()
    
    # Output generation (by technology and year - dispatch only)
    outputList[["generationByTechYrDisp"]][[experiments[ex]]] <- outputList[["generationByTechYr"]][[experiments[ex]]] %>%  
      filter(steps == "dispatch") %>% 
      mutate(grp = "disp") %>% 
      # Include an average dispatch 'scenario'
      union_all(
        outputList[["generationByTechYr"]][[experiments[ex]]] %>%
          filter(steps == "dispatch") %>% 
          group_by(k, y) %>% 
          summarise(
            s_GEN  = mean(s_GEN)
            , CO2e_tonnes  = mean(CO2e_tonnes, na.rm = TRUE)
            , CO2e_mill_tonnes = mean(CO2e_mill_tonnes, na.rm = TRUE)  ) %>% 
          ungroup() %>% 
          mutate(
            scenarioSets = "AvgDispatch"
            , steps = "dispatch"
            , grp = "average"
          )
      )
    
    # Output generation (by fuel and year)
    outputList[["generationByFuelYr"]][[experiments[ex]]] <- outputList[["generationByPlantYr"]][[experiments[ex]]] %>% 
      group_by(steps, scenarioSets, f, y) %>% 
      summarise(
        s_GEN  = sum(s_GEN)
        , CO2e_tonnes  = sum(CO2e_tonnes, na.rm = TRUE)
        , CO2e_mill_tonnes = sum(CO2e_mill_tonnes, na.rm = TRUE)
      ) %>% 
      ungroup()
    
    # Output generation (by fuel and year - dispatch only)
    outputList[["generationByFuelYrDisp"]][[experiments[ex]]] <- outputList[["generationByFuelYr"]][[experiments[ex]]] %>%  
      filter(steps == "dispatch") %>% 
      mutate(grp = "disp") %>% 
      # Include an average dispatch 'scenario'
      union_all(
        outputList[["generationByFuelYr"]][[experiments[ex]]] %>%
          filter(steps == "dispatch") %>% 
          group_by(f, y) %>% 
          summarise(
            s_GEN  = mean(s_GEN)
            , CO2e_tonnes  = mean(CO2e_tonnes, na.rm = TRUE)
            , CO2e_mill_tonnes = mean(CO2e_mill_tonnes, na.rm = TRUE)  ) %>% 
          ungroup() %>% 
          mutate(
            scenarioSets = "AvgDispatch"
            , steps = "dispatch"
            , grp = "average"
          )
      )
    
  }
  
  # Collapse all output tables so the experiment name becomes a column
  for(outputName in names(outputList)){
    
    outputList[[outputName]] <- outputList[[outputName]] %>% 
      bind_rows(.id = "Experiment")
    
  }
  
  # Cycle through and output report data as CSVs
  for(rept in names(outputList)){
    
    write_csv(outputList[[rept]], paste0(outPath, "/Report/csv/", rept, ".csv"))
    
  }
  
  
  return(outputList)
}
