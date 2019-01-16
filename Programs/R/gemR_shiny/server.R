function(input, output, session){
  
  observeEvent(input$updateSelect, {
    
    updateSelectInput(
      session, "runNameResult",
      label = NULL,
      choices = c("", list.dirs("Output", recursive = FALSE, full.names = FALSE))
    )
    
  })
  
  # Run GEM
  observeEvent(input$runGEM, {
    
    withProgress(message = "Running GEM", value = 0, {
      
      ###############################################
      ### Set up folders                           ##
      ###############################################
      incProgress(0.1, message = "Settuping up folders...")
      
      setupFolders(runName = input$runName)
      
      ###############################################
      ### Generate GEMsetup include file from CSV  ##
      ###############################################
      incProgress(0.1, message = "Running GEMsetup...")
      
      generateGEMsetup(
        runName = input$runName
        , runNameDesc = input$runNameDesc
        , firstYear = input$firstYear
        , lastYear = input$lastYear
      )
      
      ###############################################
      ### Generate GEMdata GDX file                ##
      ###############################################
      incProgress(0.1, message = "Generating GEM data GDX...")
      
      generateGEMdata(
        use_default_demand = FALSE
        , demand_path = input$demand_path$datapath
        , firstYear = input$firstYear
        , lastYear = input$lastYear
      )
      
      ###############################################
      ### Archive input GDX for current run        ##
      ###############################################
      incProgress(0.1, message = "Archiving GEM data GDX...")
      
      archiveGEMdataGDX(runName = input$runName)
      
      ###############################################
      ### Execute GEMdeclarations                  ##
      ###############################################
      
      if(input$GEMdeclarationsFlag){
        
        withProgress(
          executeGAMS(
            GAMS_filepath = "Programs/GAMS"
            , GAMS_filename = "GEMdeclarations"
            , GAMS_opts = "s = GEMdeclarations"
          )
          , message = "Running GEMdeclarations..."
        )
        
      }
      
      ###############################################
      ### Execute GEMsolve                         ##
      ###############################################
      incProgress(0.3
                  , message = "Running GEM solve..."
                  , detail = "This may take a while..."
      )
      
      executeGAMS(
        GAMS_filepath = "Programs/GAMS"
        , GAMS_filename = "GEMsolve"
        , GAMS_opts = paste0(
          "r = GEMdeclarations gdx = "
          , "../../Output/"
          , input$runName
          , "/GEMsolve_"
          , input$runName
          , ".gdx"
        )
      )
      
      ###############################################
      ### 11. Create reports                       ##
      ###############################################
      incProgress(0.3
                  , message = "Creating reports..."
      )
      
      createGEMreports(
        runName = input$runName
      )
      
      
      
    })
  })
  
  # Reporting
  
  ## Get external vars
  
  # Read in GEMsolve output GDX to use some subsets etc.
  GEMsolveGDX_path <- reactive({paste0("Output/", input$runNameResult, "/GEMsolve_", input$runNameResult, ".gdx")})
  
  GEMreport_path <- reactive({
    
    paste0(
      "Output/"
      , input$runNameResult
      , "/GDX/allExperimentsReportOutput - "
      , input$runNameResult
      , ".gdx")
    
  })
  
  ## Map generation to technology
  mapg_k <- reactive({
    rgdx.set(GEMsolveGDX_path(), "mapg_k") %>% 
      mutate_if(is.factor, as.character) %>% 
      rename(
        Plant = g
        , Technology = k
      ) %>% 
      as_tibble()
  })
  
  ## Map generation to fuel
  mapg_f <- reactive({
    rgdx.set(GEMsolveGDX_path(), "mapg_f") %>% 
      mutate_if(is.factor, as.character) %>% 
      rename(
        Plant = g
        , Fuel = f
      ) %>% 
      as_tibble()
  })
  
  ## Map generation to island
  mapg_ild <- reactive({
    rgdx.set(GEMsolveGDX_path(), "mapg_ild") %>%
      mutate_if(is.factor, as.character) %>%
      rename(
        Plant = g
        , Island = ild
      ) %>%
      as_tibble()
  })
  
  ## s_GEN - Generation by generating plant and block, GWh
  s_GEN <- reactive({
    rgdx.param(GEMsolveGDX_path(), "s_GEN") %>%
      mutate_if(is.factor, as.character) %>%
      rename(
        Plant = g
        , Year = y
        , Period = t
        , LoadBlock = lb
      ) %>%
      as_tibble()
  })
  
  ## i_heatrate - heat rate (GJ/GWh)
  i_heatrate <- reactive({
    rgdx.param(GEMsolveGDX_path(), "i_heatrate") %>%
      mutate_if(is.factor, as.character) %>%
      rename(
        Plant = g
      ) %>%
      as_tibble()
  })
  
  ## i_emissionFactors - CO2e emissions, toness CO2/PJ
  i_emissionFactors <- reactive({
    rgdx.param(GEMsolveGDX_path(), "i_emissionFactors") %>%
      mutate_if(is.factor, as.character) %>%
      rename(
        Fuel = f
      ) %>%
      as_tibble()
  })
  
  ## Solve report
  output$solveReport <- DT::renderDataTable({
    
    req(file.exists(GEMreport_path()))
    
    getResults(
      runName = input$runNameResult
      , variableName = "solveReport"
      , colNames = c("Experiments","Experiments2", "Steps", "ScenarioSets", "Variable", "Value")
    ) %>% 
      select(-Experiments2) %>% 
      DT::datatable(options = list(dom = 'lrtip', pageLength = 15))
    
  })
  
  ## Total cost
  output$totalCost <- renderPlot({
    
    req(file.exists(GEMreport_path()))
    
    totalCost <- getResults(
      runName = input$runNameResult
      , variableName = "s_TOTALCOST"
      , colNames = c("Experiments", "Steps", "ScenarioSets", "s_TOTALCOST")
    )
    
    totalCost %>% 
      mutate(Steps = str_to_title(Steps)) %>% 
      ggplot(aes(reorder(ScenarioSets, s_TOTALCOST), s_TOTALCOST, fill = Steps)) +
      geom_bar(stat = "identity", alpha = 0.6, width = 0.7) +
      scale_y_continuous(labels = scales::dollar_format()) +
      ggthemes::scale_fill_economist() +
      labs(x = "Scenario sets", y = "Total cost ($million)") + 
      facet_wrap(~Experiments)
    
  })
  
  ## Build schedule - total by year
  output$buildScheduleTotalYr <- renderPlot({
    
    req(file.exists(GEMreport_path()))
    
    buildScheduleByPlantYr <- getResults(
      runName = input$runNameResult
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
    
    buildScheduleTotalYr %>% 
      ggplot(aes(Year, s_BUILD_cumsum, colour = ScenarioSets, group = ScenarioSets)) +
      geom_step(alpha = 0.3, lwd = 1) +
      scale_y_continuous(labels = scales::comma_format()) +
      scale_x_discrete(breaks = scales::pretty_breaks(5)) +
      ggthemes::scale_fill_economist() +
      labs(x = "Scenario sets", y = "Cumulative capacity built (MW)")
    
    
  })
  
  ## Installed capacity by fuel and year
  output$installedCapacityByFuelYr <- renderPlot({
    
    req(file.exists(GEMreport_path()))
    
    ## Cumulative capacity by plant and year
    installedCapacityByPlantYr <- getResults(
      runName = input$runNameResult
      , variableName = "s_CAPACITY"
      , colNames = c("Experiments", "Steps", "ScenarioSets", "Plant", "Year", "s_CAPACITY")
    ) %>% 
      arrange(Experiments, Steps, ScenarioSets, Plant, Year)
    
    ## Installed capacity by fuel and year
    installedCapacityByFuelYr <- installedCapacityByPlantYr %>% 
      inner_join(
        mapg_f()
        , by = "Plant"
      ) %>% 
      group_by_at(vars(-c(Plant, s_CAPACITY))) %>% 
      summarise(
        s_CAPACITY_cumsum = sum(s_CAPACITY)
      ) %>% 
      arrange(Experiments, Steps, ScenarioSets, Fuel, Year) %>% 
      ungroup() 
    
    installedCapacityByFuelYr %>%  
      ggplot(aes(Year, s_CAPACITY_cumsum, fill = Fuel, group = Fuel)) +
      geom_area(alpha = 0.6) +
      scale_y_continuous(labels = scales::comma_format()) +
      scale_x_discrete(breaks = scales::pretty_breaks(5)) +
      ggthemes::scale_fill_economist() +
      labs(x = "Scenario sets", y = "Installed capacity (MW)") + 
      facet_wrap(~ScenarioSets)
    
  })
  
  ## CO2e emissions by fuel and year
  
  output$emissionsByFuelYear <- renderPlot({
    
    req(file.exists(GEMreport_path()))
    
    ## CO2e emissions by plant and year
    emissionsByPlantYear <- s_GEN() %>% 
      group_by_at(vars(-c(Period, LoadBlock, s_GEN))) %>% 
      summarise(s_GEN = sum(s_GEN)) %>% 
      ungroup() %>% 
      inner_join(
        i_heatrate()
        , by = "Plant"
      ) %>% 
      inner_join(
        mapg_f()
        , by = "Plant"
      ) %>% 
      inner_join(
        i_emissionFactors()
        , by = "Fuel"
      ) %>% 
      mutate(
        CO2e_tonnes = 1e-6 * i_heatrate * s_GEN * i_emissionFactors
        , CO2e_mill_tonnes = 1e-6 * CO2e_tonnes
      ) %>% 
      select(-c(s_GEN, i_heatrate, i_emissionFactors))
    
    emissionsByFuelYear <- emissionsByPlantYear %>% 
      group_by(steps, scenarioSets, scenarios, Fuel, Year) %>% 
      summarise(CO2e_mill_tonnes = sum(CO2e_mill_tonnes))
    
    emissionsByFuelYear %>% 
      ggplot(aes(Year, CO2e_mill_tonnes, fill = Fuel, group = Fuel)) +
      geom_area(alpha = 0.6) +
      scale_y_continuous(labels = scales::comma_format()) +
      scale_x_discrete(breaks = scales::pretty_breaks(5)) +
      ggthemes::scale_fill_economist() +
      labs(x = "Year", y = "CO2e emissions (million tonnes)") + 
      facet_wrap(~scenarioSets)
    
  })
  
}
