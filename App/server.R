library(tidyverse)
library(stringr)
library(readxl)
library(shiny)
library(tools)
library(lubridate)
library(dint)

function(input, output) {
  
  ############
  # Data Tab #
  ############
  
  
  ### Helper Functions ###
  
  detect_frequency <- function(data) {
    # Get first vintage column name
    first_col <- names(data)[2]
    
    if (str_detect(first_col, "M\\d+$")) {
      return("monthly")
    } else if (str_detect(first_col, "Q\\d$")) {
      return("quarterly")
    } else {
      stop("Unsupported Uploaded File!")
      
    }
  }
  ## Get the right century for the year
  clean_columns <- function(data) {
    
    data_cols <- names(data)[-1] %>%
      str_remove(pattern = "ROUTPUT")
    
    yy <- str_sub(data_cols, start = 1, end = 2)
    prev_yy <- yy[1]
    century = 19
    complete_year <- c()
    for (i in 1:length(yy)) {
      cur_yy <- yy[i]
      if (as.numeric(cur_yy) < as.numeric(prev_yy)) {
        century = century + 1
      }
      complete_year[i] <- paste0(century, cur_yy)
      prev_yy <- cur_yy
    }
    return(complete_year)
  }
  
  ## Clean the quarter data
  clean.data <- function(data, vintage_freq = "quarterly") {
    
    q <- names(data)[-1] %>% str_remove(pattern = "ROUTPUT") %>% str_sub(start = 1)
    clean_cols <- paste0(clean_columns(data), q)
    names(data)[-1] <- clean_cols
    total_col <- length(names(data))
    ## Clean the data
    clean_data <- data %>%
      mutate(across(2:total_col, as.numeric)) %>%
      pivot_longer(cols = -1, names_to = "vintage", values_to = "gdp") %>%
      mutate(year = str_sub(DATE, 1,4),
             quarter = str_sub(DATE, 7,7), 
             v_year = str_sub(vintage, 1,4),
             log_gdp = log(gdp)) %>%
      drop_na()
    
    if (vintage_freq == "quarterly") {
      final_data <- clean_data %>% 
        mutate(v_quarter = str_extract(vintage, pattern = "(?<=Q).*$")) %>%
        select(year, quarter, v_year, v_quarter, gdp, log_gdp) %>%
        mutate(across(1:6, as.numeric))
    }
    else {
      final_data <- clean_data %>%
        mutate(v_month = str_extract(vintage, pattern = "(?<=M).*$")) %>%
        select(year, quarter, v_year, v_month, gdp, log_gdp) %>%
        mutate(across(1:6, as.numeric))
    }
    
    
    return(final_data)
  }
  ts_transform <- function(data) {
    freq <- ifelse("v_quarter" %in% names(data), 4, 12)
    year <- data$year[1]
    dat <- data %>% select(gdp)
    ts_dat <- ts(dat, start = year, frequency = freq) 
    return(ts_dat)
  }
  
  ### Data ###
  ## Preview 
  raw_data <- reactive({
    if (input$data_source == "quarterly") {
      readxl::read_excel("../data/ROUTPUTQvQd.xlsx")
      
    } else if (input$data_source == "monthly") {
      readxl::read_excel("../data/routputMvQd.xlsx")
      
    } else if (input$data_source == "upload") {
      req(input$file)
      extension <- tools::file_ext(input$file$name)
      
      if (extension == "csv") {
        readr::read_csv(input$file$datapath)
      }
      else if (extension == "xlsx" || extension == "xls") {
        readxl::read_excel(input$file$datapath)
        
      }
      else if (extension == "json") {
        fromJSON(input$file$datapath)
      }
      else {
        stop("Unsupported file format!")
      }
    }
    else {
       NULL
     } 
    }
  )
  
  cleaned_data <- reactive({
    req(raw_data())
    raw_dat <- raw_data()
    freq <-  detect_frequency(raw_dat)
    temp <- clean.data(raw_data(), freq) 
    })
  
  data_frequency <- reactive({
    req(raw_data())
    detect_frequency(raw_data())
  })
  
  
  filtered_data <- reactive({
    if (data_frequency() == "quarterly") {
      cleaned_data() %>% filter(v_year ==input$vintage_year,
                      v_quarter == input$vintage_period)
    }
    else if (data_frequency() == "monthly") {
      cleaned_data() %>% filter(v_year == input$vintage_year,
                      v_month == input$vintage_period)
    }
    
    
  })
   
  
  output$vintage_period_ui <- renderUI({
    req(data_frequency())
    
    if (data_frequency() == "quarterly") {
      selectInput("vintage_period", "Select Quarter:", choices = 1:4, selected = 1)
    } else if (data_frequency() == "monthly") {
      selectInput("vintage_period", "Select Month:", choices = 1:12, selected = 1)
    }
  })
  gdp <- reactive({
    filtered_data() %>% select(-log_gdp) 
    
    
  })
  log_gdp <- reactive({
    filtered_data() %>% select(-gdp)
  })
  
  
  output$data_preview <- DT::renderDataTable({
    
    if (input$log_transform) {
      DT::datatable(log_gdp())
    }
    else {
      DT::datatable(gdp())
    }
  
  })
  
  ## Plot
  
  gdp_plot <- reactive({
    temp_dat <- gdp() 
    ggplot(data = temp_dat, mapping = aes(x = date_yq(year,quarter), y = gdp)) +
    scale_x_date_yq(labels = function(x) format(x, "%Y Q%q"))  +
    geom_line() +
    theme_bw()
    
  })
  log_gdp_plot <- reactive({
    temp_dat <- log_gdp() 
    ggplot(data = temp_dat, mapping = aes(x = date_yq(year,quarter), y = log_gdp)) +
    scale_x_date_yq(labels = function(x) format(x, "%Y Q%q")) + 
    geom_line() +
    theme_bw()
  })
  output$data_plot <- renderPlotly({
    if (input$log_transform) {
      log_gdp_plot()
    }
    else if (!input$log_transform) {
      gdp_plot()
    }
    
  })
  
  ## Stats
  
  
  ############
  # Model Tab #
  ############
  
  
}
