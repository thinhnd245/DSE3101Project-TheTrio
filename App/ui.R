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
                          sliderInput(inputId = "year", label = "Select Year", 
                                      min = 1965, max = 2025, value = 1965),
                          selectInput(inputId = "quarter", label = "Select Quarter", 
                                      choices = c(1,2,3,4), selected = 1),
                          sliderInput(inputId = "vintage_year", label = "Select Vintage Year", 
                                      min = 1965, max = 2025, value = 2000),
                          selectInput(inputId = "vintage_quarter", label = "Select Vintage Quarter", 
                                      choices = c(1,2,3,4), selected = 1)
             ),
             
             mainPanel(width = 9,
                       tabsetPanel(
                         tabPanel("Preview", DTOutput("data_preview")),
                         tabPanel("Plot"),
                         
                         tabPanel("Stats")
                       )
             )
           )
  ),
  
  #Model Panel
  tabPanel("Model",
           sidebarLayout(
             sidebarPanel(width = 3,
                          selectInput(inputId = "model", label = "Select Model", choices = c("AR", "ADL")),
                          
                          sliderInput(inputId = "year", label = "Select Year", 
                                      min = 1965, max = 2025, value = 1965),
                          selectInput(inputId = "quarter", label = "Select Quarter", 
                                      choices = c(1,2,3,4), selected = 1),
                          sliderInput(inputId = "vintage_year", label = "Select Vintage Year", 
                                      min = 1965, max = 2025, value = 2000),
                          selectInput(inputId = "vintage_quarter", label = "Select Vintage Quarter", 
                                      choices = c(1,2,3,4), selected = 1)
             ),
             
             mainPanel(width = 9,
                       tabsetPanel(
                         tabPanel("Summary", tableOutput("summary_arima")),
                         tabPanel("Plot"),
                         tabPanel("Performance")
                       ))
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

