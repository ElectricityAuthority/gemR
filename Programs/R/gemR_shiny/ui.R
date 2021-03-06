# library(infuser)
# library(tidyverse)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(ggplot2)
library(tibble)
library(readtext)
library(shiny)
library(shinythemes)
library(purrr)
library(gdxtools)
library(gdxrrw)
library(rlist)

# Force working directory to be the project root (if it isn't already).
# This allows for the running of the app via the 'Run App' button in 
# RStudio - making source paths etc relative to the project root.
# This doesn't feel quite right - the idea is that GEM can be run
# either via the runGEM.R script or via the shiny app using the same
# code base.
if(getwd() != rstudioapi::getActiveProject()){
  setwd(rstudioapi::getActiveProject())
}

# Turn off messages from readr
options(readr.num_columns = 0)

# Load function for executing GAMS code
source("Programs/R/shared/executeGAMS.R")

# Source programs
source("Programs/R/runGEM/readInputSymbols.R")
source("Programs/R/runGEM/setupFolders.R")
source("Programs/R/runGEM/generateGEMsetup.R")
source("Programs/R/runGEM/generateGEMdata.R")
source("Programs/R/runGEM/archiveGEMdataGDX.R")
source("Programs/R/generateGEMreports/GEMreporting.R")

navbarPage(
  
  "gemR"
  
  , theme = shinytheme("simplex")
  
  , tabPanel(
    
    "Home"
    
    , icon = icon("home")
    
    , column(
      width = 4
      , offset = 4
      , style = "margin-top:200px;"
      , div(
        img(src = "img/gemR_logo3.png")
        , tags$i(h4("Generation expansion modelling using R and GAMS"))
        , style = "padding-top:100px;padding-bottom:100px;text-align:center;"
      )
    )
    
  )
  
  , tabPanel(
    
    "Run GEM"
    
    , icon = icon("play-circle")
    
    , fluidRow(
      
      column(
        
        width = 3
        , class = "jumbotron"
        , h3("Run GEM")
        
        , h4("Run name")
        , helpText("Choose a run name.")
        , textInput("runName", NULL, value = "standard_run")
        
        , h4("Run description")
        , helpText("Choose a run description.")
        , textInput("runNameDesc", NULL, value = "Standard run")
        
        , h4("First year")
        , helpText("Choose first year.")
        , textInput("firstYear", NULL, value = "2018")
        
        , h4("Last year")
        , helpText("Choose last year.")
        , textInput("lastYear", NULL, value = "2028")
        
        , h4("Demand file path")
        , helpText("Load demand file.")
        , fileInput(
          "demand_path"
          , NULL
          ,  accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv")
        )
        
        , h4("GEMdeclarations")
        , helpText("Run GEMdeclarations?")
        , radioButtons(
          "GEMdeclarationsFlag"
          , NULL
          , choices = c(TRUE, FALSE)
          , selected = FALSE
          , inline = TRUE
        )
        
        , h4("Solve GEM")
        , actionButton("runGEM", "Run", class = "btn-primary")
        
      )
      
    )
    
  )
  , tabPanel(
    "Results"
    
    , icon = icon("area-chart")
    
    , fluidRow(
      
      column(
        
        width = 3
        , class = "jumbotron"
        , h3("GEM results")
        
        , h4("Run name")
        , helpText("Select a list of runs to compare.")
        , selectizeInput(
          "runNameList"
          , NULL
          , choices = c("", list.dirs("Output", recursive = FALSE, full.names = FALSE))
          , multiple = TRUE
        )
        
        , h4("Refresh run list")
        , helpText("For when a new run has completed.")
        , actionButton("updateSelect", "Update", class = "btn-primary")
      )
      
      , column(
        
        width = 8
        
        , tabsetPanel(
          
          tabPanel(
            "Total cost"
            , plotOutput("totalCost", height = "800px")
          )
          
          , tabPanel(
            "Build schedule (total by year)"
            , plotOutput("buildScheduleTotalYr", height = "800px")
          )
          
          , tabPanel(
            "Installed capacity (by fuel type and year)"
            , plotOutput("installedCapacityByFuelYr", height = "800px")
          )
          
          , tabPanel(
            "CO2-e emissions (by fuel type and year)"
            , plotOutput("emissionsByFuelYear", height = "800px")
          )
          
          , tabPanel(
            "Generation (by fuel type and year)"
            , plotOutput("generationByFuelYear", height = "800px")
          )
        )
        
      )
      
    )
    
  )
  
  # , footer = column(
  #   12,
  #   align = "right",
  #   style = "bottom: 0; position: absolute;",
  #   HTML(
  #     "<div class='footer'>
  #     <div class='footer-logos'>
  #     <a href='http://www.ea.govt.nz/'>
  #     <img src='https://www.emi.ea.govt.nz/Content/images/ea-logo.png' alt='Electricity Authority - We're the regulator of the electricity market in New Zealand.'>
  #     </a>
  #     <a class='govt-logo' href='http://newzealand.govt.nz/'>
  #     <img src='https://www.emi.ea.govt.nz/Content/images/nz_gov_logo.png' alt='New Zealand government services' />
  #     </a>
  #     </div>
  #     <p>
  #     <span>Electricity Authority: <a href='P: + 64 4 460 8860'>+ 64 4 460 8860</a></span>
  #     <span>EMI email: <a href='mailto:emi@ea.govt.nz?subject=EMI enquiry: '>emi@ea.govt.nz</a></span>
  #     </p>
  #     <p>© 2019 Copyright material on this site is protected by copyright owned by the Electricity Authority or its licensors.</p>
  #     </div>"
  #   )
  # )
  
  , tags$head(
    tags$style(
      HTML(
        ".dataTables_length {padding-top: 1%; padding-left: 1%;}"
        , ".shiny-plot-output {padding-top: 1%; padding-left: 1%;}"
        , paste0(
          ".navbar-default .navbar-nav>.active>a, .navbar-default"
          ," .navbar-nav>.active>a:hover, .navbar-default", 
          ".navbar-nav>.active>a:focus {border-bottom: 4px solid #ff8b05; color: white !important;}"
        )
        , ".navbar-default .navbar-nav>li>a {color: white; text-transform: uppercase; font-size: 14px;}"
        , ".navbar-default .navbar-brand {color: white; font-size: 25px;}"
        , ".navbar-default {background-color: #00346b;}"
        , "body {font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;}"
        , ".navbar-default .navbar-nav>li>a:hover, .navbar-default .navbar-nav>li>a:focus {color: white;}"
        , ".progress-bar {background-color: #3397c8;}"
        , ".btn {background: #3397c8; border-color: #3397c8;}"
        , ".btn:hover, .btn:active, .btn:focus, .btn:active:focus, .btn:active:hover {background: #3397c8; border-color: #3397c8; opacity: 0.7;}"
        , "a, a:hover, a:focus {color: #3397c8;}"
      )
    )
  )
)
