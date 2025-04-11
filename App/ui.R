library(DT)
library(plotly)
library(shiny)
library(shinythemes)
library(bslib)
# Define UI for application that draws a histogram
navbarPage(
  theme = bs_theme(
    bootswatch = "journal",
    primary = "gray",
    base_font = font_google("Open Sans"),
    heading_font = font_google("Poppins")
  ),
  
  # Application title
  title = "Real-Time GDP Forecasting",
  # Data Page
  tabPanel(h6("Datasets"),
          #Setting for upload the data
           sidebarLayout(
             sidebarPanel(h3("Data Options"), width = 3,
                          selectInput("data_source", "Select Data Source:",
                                      choices = c("FED Quarterly Data" = "quarterly",
                                                  
                                                  "Upload Data" = "upload")),
                          conditionalPanel(
                            condition = "input.data_source == 'upload'",
                            helpText(HTML(
                              "<b>Note:</b> Uploaded data should be quarterly data in format used by the 
     <a href=https://www.philadelphiafed.org/surveys-and-data/real-time-data-research/real-time-data-set-for-macroeconomists target='_blank'>
     FED Real-Time Data</a>."
                            ))
                          ),
                          
                          # Show fileInput only when "upload" is selected
                          conditionalPanel(
                            condition = "input.data_source == 'upload'",
                            fileInput("file", "Upload Your Dataset:",
                                      accept = c(".csv", ".xlsx", ".xls", ".json"))
                          ),
                          checkboxInput("growth_transform", "Growth", value = FALSE),
                          numericInput(inputId = "vintage_year", label = "Select Starting Year", 
                                      min = 1965, max = 2024, value = 2000),
                          
                          uiOutput("vintage_period_ui"),
                          uiOutput("end_year_ui"),
                          uiOutput("end_quarter_ui")
             ),
             
             mainPanel(width = 9,
                       tabsetPanel(
                        
                         tabPanel("Preview", 
                                  DTOutput("data_preview")),
                         tabPanel("Plot",
                                  plotOutput("data_plot"))
                         
                       )
             )
           )
  ),
  
  #Model Panel
  tabPanel(h6("Model"),
           sidebarLayout(
             sidebarPanel(width = 3,
                          titlePanel("Model Selection"),
                          actionButton("add_model", "+ Add a Model"),
                          br(), br(),
                          uiOutput("all_models_ui"),
                          titlePanel("Features Selection"), 
                          actionButton("add_feature", "+ Add a Feature"),
                          uiOutput("all_features_ui"),
                          br(), br(),
                          
                          titlePanel("Forecast Setting"),
                          numericInput(inputId = 'forecast_horizon', label = "Select Forecast Horizon", max = 12, min = 1,value = 1),
                          
      
                          actionButton("run_model", "Run All Models")
                          
                         
             ),
             
             mainPanel(width = 9,
                       
                       tabsetPanel(
                           tabPanel("Model Forecast", uiOutput('resulttable')),
                           tabPanel("Model Comparison", uiOutput('comparison')),
                           tabPanel("Visualization", uiOutput('modelplot'))
                         )
                         
                       )
           )
  ),
  tabPanel(h6("Development"),
           sidebarLayout(
             sidebarPanel(width = 3,
                          selectInput(inputId = "model1", label = "Select Model 1", choices = c("Model A", "Model B", "Model C"),
                                      selected = "Model A"),
                          selectInput(inputId = "model2", label = "Select Model 2", choices = c("Model A", "Model B", "Model C"),
                                      selected = "Model B")
                          ),
             mainPanel(width = 9)
           ))
)

