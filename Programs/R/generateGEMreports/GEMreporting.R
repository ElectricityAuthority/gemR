###############################################
### Title: GEM reporting                     ##
### Description: Create reports for current  ##
### GEM run.                                 ##
### Date: 7 January 2019                     ##
###############################################

###############################################
### 1. Load libraries                        ##
###############################################
# library(tidyverse)

###############################################
### 2. Additional setup                      ##
###############################################

# Load code for creating report data file
source("Programs/R/runGEM/createReportFile.R")

# Turn off messages from readr
options(readr.num_columns = 0)

createGEMreports <- function(runName){
  
  # ggplot shared settings
  theme_set(
    theme_bw() +
      theme(
        axis.text = element_text(size = 5)
        , axis.title = element_text(size = 7)
        , legend.text = element_text(size = 5)
        , legend.title = element_blank()
      )
  )
  
  outPath <- paste0("Output/", runName)
  
  # Generate data file
  reportData <- createReportFile(runName = runName)
  
  ###############################################
  ### 3. Plots/reports                         ##
  ###############################################
  
  # Solve report
  
  ## Write solve report to CSV
  write_csv(reportData$solveReport, paste0(outPath, "/Report/csv/solveReport.csv"))
  
  # Total cost
  png(file = paste0(outPath, "/Report/plots/totalCost.png")
      , width = 18.5, height = 10.5, units = "cm", res = 1000)
  
  print(
    reportData$totalCostDisp %>% 
      ggplot(aes(reorder(scenarioSets, s_TOTALCOST), s_TOTALCOST, fill = grp)) +
      geom_bar(stat = "identity", alpha = 0.6, width = 0.7) +
      scale_y_continuous(labels = scales::dollar_format()) +
      scale_fill_manual(
        values = c("disp" = "lightsteelblue", "average" = "steelblue")
        , guide = FALSE) +
      labs(x = "Dispatch scenarios", y = "Total cost ($million)") + 
      facet_wrap(~Experiment) +
      geom_text(
        data = reportData$totalCostDisp %>% filter(grp == "average")
        , aes(label = paste0(scales::dollar(s_TOTALCOST, accuracy = 1), " mill"), vjust = 1.25)
        , colour = "white"
        , size = 2
      )
  )
  
  dev.off()
  
  # Build schedule (new capacity)
  
  ## Build schedule - total by year
  png(file = paste0(outPath, "/Report/plots/buildScheduleTotalYr.png")
      , width = 18.5, height = 10.5, units = "cm", res = 1000)
  
  print(
    reportData$buildTotalYrDisp %>% 
      ggplot(aes(y, s_BUILD_cumsum, alpha = grp, group = grp)) +
      geom_step(lwd = 1, colour = "steelblue") +
      scale_y_continuous(labels = scales::comma_format()) +
      ggthemes::scale_fill_economist() +
      labs(x = "Year", y = "Cumulative capacity built (MW)") +
      scale_alpha_manual(
        values = c("disp" = 0.2, "average" = 1)
        , guide = FALSE
      ) + 
      facet_wrap(~Experiment) +
      expand_limits(y = 0)
  )
  
  dev.off()
  
  ## Build schedule - by technology and year
  png(file = paste0(outPath, "/Report/plots/buildScheduleByTechYr.png")
      , width = 18.5, height = 10.5, units = "cm", res = 1000)
  
  print(
    reportData$buildByTechYrDisp %>% 
      ggplot(aes(y, s_BUILD_cumsum, alpha = grp, group = grp)) +
      geom_step(lwd = 1, colour = "steelblue") +
      scale_y_continuous(labels = scales::comma_format()) +
      ggthemes::scale_fill_economist() +
      labs(x = "Year", y = "Cumulative capacity built (MW)") +
      scale_alpha_manual(
        values = c("disp" = 0.2, "average" = 1)
        , guide = FALSE
      ) + 
      facet_wrap(~Experiment + k) +
      expand_limits(y = 0)
  )
  
  dev.off()
  
  # Installed capacity 
  
  ## Cumulative capacity by plant and year
  png(file = paste0(outPath, "/Report/plots/installedCapacityTotalYr.png")
      , width = 18.5, height = 10.5, units = "cm", res = 1000)
  
  print(
    reportData$capacityTotalYrDisp %>% 
      ggplot(aes(y, s_CAPACITY, alpha = grp, group = grp)) +
      geom_step(lwd = 1, colour = "steelblue") +
      scale_y_continuous(labels = scales::comma_format()) +
      ggthemes::scale_fill_economist() +
      labs(x = "Year", y = "Installed capacity (MW)") +
      scale_alpha_manual(
        values = c("disp" = 0.2, "average" = 1)
        , guide = FALSE
      ) + 
      facet_wrap(~Experiment)
  )
  
  dev.off()
  
  ## Installed capacity by fuel and year
  png(file = paste0(outPath, "/Report/plots/installedCapacityByFuelYr.png")
      , width = 18.5, height = 10.5, units = "cm", res = 1000)
  
  print(
    reportData$capacityByFuelYrDisp %>% 
      filter(grp == "average") %>% 
      ggplot(aes(y, s_CAPACITY, fill = f, group = f)) +
      geom_area(alpha = 0.6) +
      scale_y_continuous(labels = scales::comma_format()) +
      ggthemes::scale_fill_economist() +
      labs(x = "Year", y = "Installed capacity (MW)") + 
      facet_wrap(~Experiment)
  )
  
  dev.off()
  
  # Generation
  
  ## Generation total by year
  png(file = paste0(outPath, "/Report/plots/generationTotalByYr.png")
      , width = 18.5, height = 10.5, units = "cm", res = 1000)
  
  print(
    reportData$generationTotalYrDisp %>% 
      ggplot(aes(y, s_GEN, alpha = grp, group = scenarioSets)) +
      geom_line(lwd = 1, colour = "steelblue") +
      scale_y_continuous(labels = scales::comma_format()) +
      scale_alpha_manual(
        values = c("disp" = 0.2, "average" = 1)
        , guide = FALSE
      ) +
      labs(x = "Year", y = "Generation (GWh)") + 
      facet_wrap(~Experiment)
  )
  
  dev.off()
  
  ## Generation by fuel and year
  png(file = paste0(outPath, "/Report/plots/generationByFuelYr.png")
      , width = 18.5, height = 10.5, units = "cm", res = 1000)
  
  print(
    reportData$generationByFuelYrDisp %>% 
      filter(grp == "average", s_GEN > 0) %>% 
      ggplot(aes(y, s_GEN, fill = f, group = f)) +
      geom_area(alpha = 0.6) +
      scale_y_continuous(labels = scales::comma_format()) +
      ggthemes::scale_fill_economist() +
      labs(x = "Year", y = "Generation (GWh)") + 
      facet_wrap(~Experiment)
  )
  
  dev.off()
  
  # Emissions
  
  ## CO2e emissions by plant and year
  png(file = paste0(outPath, "/Report/plots/emissionsTotalByYear.png")
      , width = 18.5, height = 10.5, units = "cm", res = 1000)
  
  print(
    reportData$generationTotalYrDisp %>% 
      ggplot(aes(y, CO2e_mill_tonnes, alpha = grp, group = scenarioSets)) +
      geom_line(lwd = 1, colour = "steelblue") +
      scale_y_continuous(labels = scales::comma_format()) +
      scale_alpha_manual(
        values = c("disp" = 0.2, "average" = 1)
        , guide = FALSE
      ) +
      labs(x = "Year", y = "CO2e emissions (million tonnes)") + 
      facet_wrap(~Experiment) +
      expand_limits(y = 0)
  )
  
  dev.off()
  
  ## CO2e emissions by fuel and year
  png(file = paste0(outPath, "/Report/plots/emissionsByFuelYear.png")
      , width = 18.5, height = 10.5, units = "cm", res = 1000)
  
  print(
    reportData$generationByFuelYrDisp %>% 
      filter(grp == "average", CO2e_mill_tonnes > 0) %>% 
      ggplot(aes(y, CO2e_mill_tonnes, fill = f, group = f)) +
      geom_area(alpha = 0.6) +
      scale_y_continuous(labels = scales::comma_format()) +
      ggthemes::scale_fill_economist() +
      labs(x = "Year", y = "CO2e emissions (million tonnes)") + 
      facet_wrap(~Experiment)
  )
  
  dev.off()
  
}
