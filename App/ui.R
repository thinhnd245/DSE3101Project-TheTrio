library(DT)
library(plotly)
library(shiny)

# Define UI for application that draws a histogram
navbarPage(
  # Application title
  title = "DEMO",
  # Data Page
  tabPanel(h4("Datasets"),
          #Setting for upload the data
           sidebarLayout(
             sidebarPanel(h3("Data Options"), width = 3,
                          selectInput("data_source", "Select Data Source:",
                                      choices = c("FED Quarterly Data" = "quarterly",
                                                  "FED Monthly Data" = "monthly",
                                                  "Upload Data" = "upload")),
                          conditionalPanel(
                            condition = "input.data_source == 'upload'",
                            helpText(HTML(
                              "<b>Note:</b> Uploaded data should be in format used by the 
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
                          checkboxInput("log_transform", "Log-transform GDP", value = FALSE),
                          numericInput(inputId = "vintage_year", label = "Select Vintage Year", 
                                      min = 1965, max = 2025, value = 2000),
                          uiOutput("vintage_period_ui")
             ),
             
             mainPanel(width = 9,
                       tabsetPanel(
                        
                         tabPanel("Preview", 
                                  DTOutput("data_preview")),
                         tabPanel("Plot",
                                  plotlyOutput("data_plot")),
                         tabPanel("Stats")
                       )
             )
           )
  ),
  
  #Model Panel
  tabPanel(h4("Model"),
           sidebarLayout(
             sidebarPanel(width = 3,
                          titlePanel("Model Setting:"),
                          selectInput(inputId = "model", label = "Select Model", 
                                      choices = c("Autoregressive (AR)" = "AR", 
                                                  "Autoregressive Distributed Lag (ADL)" = "ADL")),
                          radioButtons("estimation_mode", "Choose Mode:",
                                       choices = c("Optimized" = "optimized", "Customize" = "custom")),
                          
                          conditionalPanel(
                            condition = "input.model == 'AR' && input.estimation_mode == 'custom'",
                            
                            
                            numericInput(inputId = "ar_lag", "Select AR lag p", min = 0, max = 12, value = 2),
                    
                          ), 
                          titlePanel("Training Setting:"), 
            
                          numericInput("forecast_h", "Forecast Horizon (h):", value = 1, min = 1),
                          numericInput("noos", "Out-of-Sample Period (noos):", value = 1, min = 1),
                          actionButton("run_model", "Run AR Forecast")
                          
                         
             ),
             
             mainPanel(width = 9,
                       
                       tabsetPanel(
                           tabPanel("Forecast Plot", plotOutput("ar_plot")),
                           tabPanel("Model Summary", verbatimTextOutput("ar_summary")),
                           tabPanel("Test", tableOutput("fc"))
                         )
                         
                       )
           )
  ),
  tabPanel("Comparison",
           sidebarLayout(
             sidebarPanel(width = 3,
                          selectInput(inputId = "model1", label = "Select Model 1", choices = c("Model A", "Model B", "Model C"),
                                      selected = "Model A"),
                          selectInput(inputId = "model2", label = "Select Model 2", choices = c("Model A", "Model B", "Model C"),
                                      selected = "Model B")
                          ),
             mainPanel(width = 9)
           )),
  tabPanel("About")
)

