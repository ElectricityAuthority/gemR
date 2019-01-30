function(input, output, session){
  
  observeEvent(input$updateSelect, {
    
    updateSelectInput(
      session, 
      "runNameList",
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
      ### Create reports                           ##
      ###############################################
      incProgress(0.3
                  , message = "Creating reports..."
      )
      
      createGEMreports(
        runName = input$runName
      )
      
    })
  })
  
  # In-app reporting
  
  # Change ggplot shared settings for in-app plots
  ggplot_themes <- observeEvent(input$runNameList, {
    
    theme_set(
      theme_bw() +
        theme(
          axis.text = element_text(size = 12)
          , axis.title = element_text(size = 15)
          , legend.text = element_text(size = 12)
          , legend.title = element_blank()
        ) 
    )
  }
  )
  
  ## Total cost
  output$totalCost <- renderPlot({
    
    validate(
      need(input$runNameList, "Select at least one run to show plots.")
    )
    
    totalCostDisp <- input$runNameList %>% 
      map(~read_csv(
        paste0("Output/", .x, "/Report/csv/totalCostDisp.csv") 
      ) %>% 
        mutate(runName = .x)
      ) %>% 
      reduce(bind_rows)
    
    totalCostDisp %>% 
      filter(grp == "average") %>% 
      ggplot(aes(reorder(runName, s_TOTALCOST), s_TOTALCOST)) +
      geom_bar(stat = "identity", alpha = 0.6, width = 0.7, fill = "steelblue") +
      scale_y_continuous(labels = scales::dollar_format()) +
      labs(x = "Run name", y = "Total cost ($million)") +
      geom_text(
        aes(label = paste0(scales::dollar(s_TOTALCOST, accuracy = 1), " mill"), vjust = 1.25)
        , colour = "white"
        , size = 5
      )
    
  })
  
  ## Build schedule - total by year
  output$buildScheduleTotalYr <- renderPlot({
    
    validate(
      need(input$runNameList, "Select at least one run to show plots.")
    )
    
    buildTotalYrDisp <- input$runNameList %>% 
      map(~read_csv(
        paste0("Output/", .x, "/Report/csv/buildTotalYrDisp.csv") 
      ) %>% 
        mutate(runName = .x)
      ) %>% 
      reduce(bind_rows)
    
    buildTotalYrDisp %>% 
      filter(grp == "average") %>% 
      ggplot(aes(y, s_BUILD_cumsum, colour = runName)) +
      geom_step(lwd = 1) +
      scale_y_continuous(labels = scales::comma_format()) +
      ggthemes::scale_colour_economist() +
      labs(x = "Year", y = "Cumulative capacity built (MW)") +
      facet_wrap(~Experiment) +
      expand_limits(y = 0) +
      scale_x_continuous(breaks = scales::pretty_breaks())
    
  })
  
  ## Installed capacity by fuel and year
  output$installedCapacityByFuelYr <- renderPlot({
    
    validate(
      need(input$runNameList, "Select at least one run to show plots.")
    )
    
    capacityByFuelYrDisp <- input$runNameList %>% 
      map(~read_csv(
        paste0("Output/", .x, "/Report/csv/capacityByFuelYrDisp.csv")
      ) %>% 
        mutate(runName = .x)
      ) %>% 
      reduce(bind_rows)
    
    capacityByFuelYrDisp %>% 
      filter(grp == "average") %>% 
      ggplot(aes(y, s_CAPACITY, fill = f, group = f)) +
      geom_area(alpha = 0.6) +
      scale_y_continuous(labels = scales::comma_format()) +
      ggthemes::scale_fill_economist() +
      labs(x = "Year", y = "Installed capacity (MW)") + 
      facet_wrap(~runName + Experiment) +
      scale_x_continuous(breaks = scales::pretty_breaks())
    
  })
  
  ## CO2e emissions by fuel and year
  
  output$emissionsByFuelYear <- renderPlot({
    
    validate(
      need(input$runNameList, "Select at least one run to show plots.")
    )
    
    generationByFuelYrDisp <- input$runNameList %>% 
      map(~read_csv(
        paste0("Output/", .x, "/Report/csv/generationByFuelYrDisp.csv")
      ) %>% 
        mutate(runName = .x)
      ) %>% 
      reduce(bind_rows)
    
    generationByFuelYrDisp %>% 
      filter(grp == "average", CO2e_mill_tonnes > 0) %>% 
      ggplot(aes(y, CO2e_mill_tonnes, fill = f, group = f)) +
      geom_area(alpha = 0.6) +
      scale_y_continuous(labels = scales::comma_format()) +
      ggthemes::scale_fill_economist() +
      labs(x = "Year", y = "CO2e emissions (million tonnes)") + 
      facet_wrap(~runName + Experiment) +
      scale_x_continuous(breaks = scales::pretty_breaks())
    
  })
  
  ## Generation by fuel and year
  
  output$generationByFuelYear <- renderPlot({
    
    validate(
      need(input$runNameList, "Select at least one run to show plots.")
    )
    
    generationByFuelYrDisp <- input$runNameList %>% 
      map(~read_csv(
        paste0("Output/", .x, "/Report/csv/generationByFuelYrDisp.csv")
      ) %>% 
        mutate(runName = .x)
      ) %>% 
      reduce(bind_rows)
    
    generationByFuelYrDisp %>% 
      filter(grp == "average", CO2e_mill_tonnes > 0) %>% 
      ggplot(aes(y, s_GEN, fill = f, group = f)) +
      geom_area(alpha = 0.6) +
      scale_y_continuous(labels = scales::comma_format()) +
      ggthemes::scale_fill_economist() +
      labs(x = "Year", y = "Generation (GWh)") + 
      facet_wrap(~runName + Experiment) +
      scale_x_continuous(breaks = scales::pretty_breaks())
    
  })
  
}
