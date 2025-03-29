library(tidyverse)
library(stringr)
library(readxl)
library(shiny)
library(tools)

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
      return("unknown")
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
  clean_data <- function(data, vintage_freq = "quarterly") {
    
    q <- names(data)[-1] %>% str_remove(pattern = "ROUTPUT") %>% str_sub(start = 3, end = 4)
    clean_cols <- paste0(clean_columns(data), q)
    names(data)[-1] <- clean_cols
    total_col <- length(names(data))
    ## Clean the data
    clean_data <- data %>%
      mutate(across(2:total_col, as.numeric)) %>%
      pivot_longer(cols = -1, names_to = "vintage", values_to = "gdp") %>%
      mutate(year = str_sub(DATE, 1,4),
             quarter = str_sub(DATE, 7,7), 
             v_year = str_sub(vintage, 1,4)) %>%
      drop_na()
    
    if (vintage_freq == "quarterly") {
      final_data <- clean_data %>% 
        mutate(v_quarter = str_extract(vintage, pattern = "(?<=Q).*$")) %>%
        select(year, quarter, v_year, v_quarter, gdp) %>%
        mutate(across(1:5, as.numeric))
    }
    else {
      final_data <- clean_data %>%
        mutate(v_month = str_extract(vintage, pattern = "(?<=M).*$")) %>%
        select(year, quarter, v_year, v_month, gdp) %>%
        mutate(across(1:5, as.numeric))
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
    clean_data(raw_data(), freq)
    
  })
  output$data_preview <- DT::renderDataTable({
    DT::datatable(cleaned_data())
  })
  
  
  # Model Tab
}
