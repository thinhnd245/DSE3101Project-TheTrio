library(DT)
library(plotly)
library(shiny)

# Define UI for application that draws a histogram
navbarPage(
  # Application title
  title = "Testing App",
  # Dataset Panel
  tabPanel("Dataset",
           
           
           sidebarLayout(
             sidebarPanel(width = 3,
                          fileInput("file", "Upload your data", accept = c(".csv", ".xlsx", "xls", ".json")),
                          checkboxInput("default_dataset", "Use Default Dataset", FALSE),
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
                         tabPanel("Data Preview", DTOutput("dataset")),
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
  tabPanel("Comparison"),
  tabPanel("About")
)

