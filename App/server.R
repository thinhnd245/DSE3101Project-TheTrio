library(tidyverse)
library(stringr)
library(readxl)
library(shiny)

# Define server logic required to draw a histogram
function(input, output) {
  dataset <- reactive({
    req(input$file)
    extension <- tools::file_ext(input$file$name) 
    if (input$default_dataset) {
      
    }
    if (extension == "csv") {
      read.csv(input$file$datapath)
    }
    else if (extension == "xlsx" || extension == "xls") {
      read_excel(input$file$datapath)
    }
    else if (extension == "json") {
      fromJSON(input$file$datapath)
    }
    else {
      stop("Unsupported file format!")
    }
    
  })
  
  #Dataset Tab
  gdp_quarter <- reactive({dataset() %>%
      mutate(across(2:239, as.numeric)) %>%
      pivot_longer(cols = `ROUTPUT65Q4`:`ROUTPUT25Q1`, names_to = "Vintage", values_to = "gdp") %>%
      mutate(Vintage = ifelse(as.numeric(substr(Vintage, 8,9)) >= 65, 
                              paste(19, substr(Vintage, 8,11), sep = ""),
                              paste(20, substr(Vintage, 8,11), sep = "")),
             v_quarter = as.numeric(substr(Vintage, 6,6)),
             v_year = as.numeric(substr(Vintage, 1,4))) %>%
      drop_na() %>%
      select(DATE, v_year, v_quarter, gdp) })       
  selected_gdp <- reactive({
    gdp_quarter() %>%
      filter(DATE == paste(input$year, ":Q", input$quarter, sep = "")
      ) 
  })
  output$dataset = DT::renderDT({selected_gdp()})
}
