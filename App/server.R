library(tidyverse)
library(stringr)
library(readxl)
library(shiny)
library(tools)
library(lubridate)
library(zoo)
library(forecast)
library(tsfknn)
library(fredr)
library(stats)


function(input, output, session) {
  
  ######Import Additional Features #######
  
  key <- '767c9c44fa69c7b7341535020cea9134'
  fredr_set_key(key = key)

  ################### Data Tab ########################
  
  
  
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
      pivot_longer(cols = -1, names_to = "vintage", values_to = "current_vintage") %>%
      mutate(year = str_sub(DATE, 1,4),
             quarter = str_sub(DATE, 7,7), 
             v_year = str_sub(vintage, 1,4),
             log_current_vintage = log(current_vintage)) %>%
      drop_na()
    
    if (vintage_freq == "quarterly") {
      final_data <- clean_data %>% 
        mutate(v_quarter = str_extract(vintage, pattern = "(?<=Q).*$")) %>%
        select(year, quarter, v_year, v_quarter, current_vintage, log_current_vintage) %>%
        mutate(across(1:6, as.numeric))
    }
    else {
      final_data <- clean_data %>%
        mutate(v_month = str_extract(vintage, pattern = "(?<=M).*$")) %>%
        select(year, quarter, v_year, v_month, current_vintage, log_current_vintage) %>%
        mutate(across(1:6, as.numeric))
    }
    
    
    return(final_data)
  }
  
  ### Data ###
  ## Preview 
  raw_data <- reactive({
    if (input$data_source == "quarterly") {
      readxl::read_excel("data/ROUTPUTQvQd.xlsx")
      
    } else if (input$data_source == "monthly") {
      readxl::read_excel("data/routputMvQd.xlsx")
      
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
  })
  
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
  # Latest vintage data 
  latest_vintage_data <- reactive({
    if (data_frequency() == "quarterly") {
      latest_quarter <- cleaned_data() %>%
        filter(v_year == 2025) %>% 
        summarize(max_quarter = max(v_quarter, na.rm = TRUE)) %>%
        pull(max_quarter)
      
      cleaned_data() %>% filter(v_year == 2025,
                                v_quarter == latest_quarter) %>%
        rename(latest_vintage = current_vintage,
               log_latest_vintage = log_current_vintage) %>%
        mutate(lag_latest_vintage = lag(latest_vintage,1),
               
               log_lag_latest_vintage = log(lag_latest_vintage),
               latest_growth = 400*(log_latest_vintage - log_lag_latest_vintage)) %>%
        select(-v_quarter, -v_year)
    }
    else if (data_frequency() == 'monthly') {
      latest_month <- cleaned_data() %>%
        filter(v_year == 2025) %>%
        summarize(max_month = max(v_month, na.rm =TRUE)) %>%
        pull(max_month)
      
      cleaned_data() %>% filter(v_year == 2025,
                                v_month == latest_month) %>%
        rename(latest_vintage = current_vintage,
               log_latest_vintage = log_current_vintage)%>%
        select(-v_quarter, -v_year)
    }
  })
  output$vintage_period_ui <- renderUI({
    req(data_frequency())
    
    if (data_frequency() == "quarterly") {
      selectInput("vintage_period", "Select Starting Quarter:", choices = 1:4, selected = 1)
    } else if (data_frequency() == "monthly") {
      selectInput("vintage_period", "Select Vintage Month:", choices = 1:12, selected = 1)
    }
  })
  filter_function <- function(v_year1, v_quarter1) {
    cleaned_data() %>% filter(v_year == v_year1,
                              v_quarter == v_quarter1) %>%
      
      mutate(lag_current_vintage = lag(current_vintage,1),
             log_lag_current_vintage = log(lag_current_vintage),
             current_growth = 400*(log_current_vintage - log_lag_current_vintage))
    
  }
  # Data at chosen vintage  
  filtered_data <- reactive({
    
      temp <- cleaned_data() %>% filter(v_year == input$vintage_year,
                                        v_quarter == input$vintage_period) %>%
        left_join(latest_vintage_data(), by = c('year', 'quarter')) %>%
        mutate(lag_current_vintage = lag(current_vintage,1),
               lag_latest_vintage = lag(latest_vintage,1),
               log_lag_current_vintage = log(lag_current_vintage),
               log_lag_latest_vintage = log(lag_latest_vintage),
               
               current_growth = 400*(log_current_vintage - log_lag_current_vintage), 
               latest_growth = 400*(log_latest_vintage - log_lag_latest_vintage))

  })
  
  
  
  
  
  
  
  
  ## Data for building gdp model
  
  #gdp <- reactive({
  #  filtered_data() %>% select(-log_current_vintage, -log_latest_vintage, -v_year, -any_of(c("v_month", "v_quarter"))) 
  #})
  
  ## Data for building GDP model
  #log_gdp <- reactive({
  #  filtered_data() %>% select(-current_vintage, -latest_vintage,-v_year, -any_of(c("v_month", "v_quarter")))
  #})
  
  
  output$data_preview <- DT::renderDataTable({
    
    if (input$growth_transform) {
      filtered_data() %>% filter(year >= 1965) %>% select(year,quarter,current_growth, latest_growth) %>%
        rename("Year" = "year", "Quarter" = "quarter", "Current Growth" = "current_growth", "Latest Growth" = "latest_growth")
       
    }
    else {
      filtered_data() %>% filter(year >= 1965) %>% select(year,quarter, current_vintage, latest_vintage) %>%
        rename("Year" = "year", "Quarter" = "quarter", "Current Vintage" = "current_vintage", "Latest Vintage" = "latest_vintage")
    }
    
  })
  
  ######## Plot ###########
  
  gdp_plot <- reactive({
    filtered_data() %>% filter(year >= 1965) %>% select(year,quarter, current_vintage, latest_vintage) %>%
      pivot_longer(`current_vintage`:`latest_vintage`, names_to = "type", values_to = "value") %>%
      ggplot(mapping = aes(y = value, x = as.Date(zoo::as.yearqtr(paste0(year, " Q", quarter))),color = type)) +
      geom_line(size = 1) +
      scale_x_date(labels = function(x) zoo::format.yearqtr(x, "%YQ%q"))  +
      scale_color_manual(
        values = c("current_vintage" = "cyan3", "latest_vintage" = "indianred"),
        labels = c("Current Vintage", "Latest Vintage"),
        name = ""
      ) +
      
      labs(
        title = "Real-Time GDP Level Over Time",
        x = "",
        y = "GDP Level"
      ) + 
      theme_classic() +
      theme(
        legend.position = "top",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11)
      )
      
    
  })
  growth_plot <- reactive({
    filtered_data() %>% filter(year >= 1965) %>% select(year, quarter, current_growth, latest_growth) %>%
      pivot_longer(`current_growth`:`latest_growth`, names_to = "type", values_to = "value") %>%
    ggplot(mapping = aes(y = value, x = as.Date(zoo::as.yearqtr(paste0(year, " Q", quarter))),color = type)) +
      geom_line(alpha = 0.8, size = 1, show.legend =TRUE) + 
      scale_x_date(labels = function(x) zoo::format.yearqtr(x, "%YQ%q")) + 
      scale_color_manual(
        values = c("current_growth" = "cyan3", "latest_growth" = "indianred"),
        labels = c("Current Vintage", "Latest Vintage"),
        name = ""
      ) +
      labs(
        title = "Real-Time GDP Growth Over Time",
        x = "",
        y = "GDP Growth (%)") + 
      theme_classic()  +
      theme(
        legend.position = "top",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11)
      )
  })
  output$data_plot <- renderPlot({
    if (input$growth_transform) {
     growth_plot()
    }
    else if (!input$growth_transform) {
      gdp_plot() 
    }
    
  })
  
  
  
  ## From here on, we will only working with Quarterly Vintages
  
  
  ############## Model Tab ##############
  
  
  min_end_date_from_start <- function(start_year, start_quarter, min_quarters = 30) {
    start_year <- as.numeric(start_year)
    start_quarter <- as.numeric(start_quarter)
    start_index <- start_year * 4 + (start_quarter - 1)
    end_index <- start_index + (min_quarters - 1)
    max_index <- 2024 * 4 + 3
    end_index <- min(end_index, max_index)
    end_year <- end_index %/% 4
    end_quarter <- (end_index %% 4) + 1
    list(year = end_year, quarter = end_quarter)
  }
  observeEvent({
    input$vintage_year
    input$vintage_period
  }, {
    req(input$vintage_year, input$vintage_period)
    
    min_date <- min_end_date_from_start(input$vintage_year, input$vintage_period, min_quarters = 30)
    
    # Fallback values if inputs are NULL
    end_year_val <- if (!is.null(input$end_year)) max(input$end_year, min_date$year) else min_date$year
    end_quarter_val <- if (!is.null(input$end_quarter)) max(input$end_quarter, min_date$quarter) else min_date$quarter
    
    updateNumericInput(session, "end_year", min = min_date$year, value = end_year_val, max = 2024)
    updateSelectInput(session, "end_quarter", selected = end_quarter_val)
  })
  
  count_oos_forecasts <- function(start_year, start_quarter, end_year, end_quarter) {
    start_index <- start_year * 4 + (start_quarter - 1)
    end_index <- end_year * 4 + (end_quarter - 1)
    return(end_index - start_index + 1)
  }
  
  observeEvent({
    input$vintage_year
    input$vintage_period
  }, {
    req(input$vintage_year, input$vintage_period)
    
    min_date <- min_end_date_from_start(as.numeric(input$vintage_year), as.numeric(input$vintage_period), 30)
    
    # Render end year input
    output$end_year_ui <- renderUI({
      numericInput(
        "end_year",
        "Select End Year (for forecast)",
        value = min_date$year,
        min = min_date$year,
        max = 2024
      )
    })
    # Render end quarter input
    output$end_quarter_ui <- renderUI({
      selectInput(
        "end_quarter",
        "Select End Quarter (for forecast)",
        choices = 1:4,
        selected = min_date$quarter
      )
    })
  })
  
  
  
  
  # Record the number of model
  model_ids <- reactiveVal(c())
  feature_ids <- reactiveVal(c())
  
  ## Add model, features (Limit to 3)
  observeEvent(input$add_model, {
    current_ids <- model_ids()
    new_id <- 1
    if (length(current_ids) < 3) {
      if (length(current_ids) > 0) {
        new_id <- max(current_ids) + 1
      }
      
      model_ids(c(current_ids, new_id))
    }
    
    else {
      shinyjs::disable("add_model")
    }
  })
  
  
  
  observeEvent(input$add_feature, {
    current_ids <- feature_ids()
    new_id <- 1
    if (length(current_ids) < 3) {
      if (length(current_ids) > 0) {
        new_id <- max(current_ids) + 1
      }
      
      feature_ids(c(current_ids, new_id))
    }
    
    else {
      shinyjs::disable("add_feature")
    }
  })
  
  
  ## Remove model, feature
  observe({
    lapply(model_ids(), function(i) {
      observeEvent(input[[paste0("delete_model_", i)]], {
        updated_ids <- model_ids()
        updated_ids <- updated_ids[updated_ids != i]
        model_ids(updated_ids)
        
        if (length(updated_ids) < 3) {
          shinyjs::enable("add_model")
        }
      }, ignoreInit = TRUE)
    })
  })
  
  observe({
    lapply(feature_ids(), function(i) {
      observeEvent(input[[paste0("delete_feature_", i)]], {
        updated_ids <- feature_ids()
        updated_ids <- updated_ids[updated_ids != i]
        feature_ids(updated_ids)
        
        if (length(updated_ids) < 3) {
          shinyjs::enable("add_feature")
        }
      }, ignoreInit = TRUE)
    })
  })
  
  
  
  # Store Model for Running Ui
  output$all_models_ui <- renderUI({
    lapply(model_ids(), function(i) {
      wellPanel(
        h4(paste("Model", i)),
        
        # Model type selection
        selectInput(paste0("model_type_", i), "Choose a model:",
                    choices = c("KNN", "AR", "ADL")),
        
        # Placeholder for model-specific parameters
        uiOutput(paste0("model_params_", i)),
        
        # Delete button
        actionButton(paste0("delete_model_", i), "Delete", class = "btn-danger")
      )
    }) %>% tagList()
  })
  output$all_features_ui <- renderUI({
    lapply(feature_ids(), function(i) {
      wellPanel(
        h4(paste("Feature", i)),
        textInput(paste0("feature_id_", i), label = "Feature Series ID"),
        numericInput(paste0("feature_lag_id_", i), label = "Lags (optional)", value = 1, min = 1, max = 5),
        textInput(paste0("feature_transform_id_", i), label = "Transformation (optional)", value = "lin"),
        actionButton(paste0("delete_feature_", i), "Delete", class = "btn-danger")
      )
    }) %>% tagList()
  })
  
  
  
  
  # Model Specific Parameters
  observe({
    lapply(model_ids(), function(i) {
      output[[paste0("model_params_", i)]] <- renderUI({
        model_type <- input[[paste0("model_type_", i)]]
        req(model_type)
        
        if (model_type == "KNN") {
          tagList(textInput(paste0("knn_k_",i), "Enter a number or a comma-separated vector:", value = "5"),
                  selectInput(paste0("knn_cf_", i), "CF", choices = c("mean","median", "weighted"), selected = "mean"))
          
        } else if (model_type == "AR") {
          tagList(numericInput(paste0("ylag_ar_",i), "Enter how many lag for Y", value = 1, min = 1))
        } else if (model_type == "ADL") {
          tagList(numericInput(paste0("ylag_adl_",i), "Enter how many lag for Y", value = 2, min = 1))
        }
      })
    })
  })
  
  observe({
    lapply(feature_ids(), function(i) {
      observeEvent(input[[paste0("delete_feature_", i)]], {
        updated_ids <- setdiff(feature_ids(), i)
        feature_ids(updated_ids)
        if (length(updated_ids) < 3) shinyjs::enable("add_feature")
      }, ignoreInit = TRUE)
    })
  })
  
  feature_lag_series <- reactive({
    ids <- feature_ids()
    sapply(ids, function(i) {
      val <- input[[paste0("feature_lag_id_", i)]]
      if (is.null(val)) NA else val
    })
  })
  
  feature_transform_series <- reactive({
    ids <- feature_ids()
    sapply(ids, function(i) {
      val <- input[[paste0("feature_transform_id_", i)]]
      if (is.null(val)) "" else val
    })
  })
  # Extract the feature
  feature_series_ids <- reactive({
    ids <- feature_ids()
    sapply(ids, function(i) input[[paste0("feature_id_", i)]])
  })
  
  is_valid_series <- function(series_id) {
    tryCatch({
      fredr_series_observations(
        series_id = series_id,
        frequency = "q",
        aggregation_method = "avg"
      )
      TRUE 
    }, error = function(e) {
      FALSE  
    })
  }
  # Run all models
  modeltype = reactiveVal(list())
  results = reactiveVal(list())
  performance = reactiveVal(list())
  
  observeEvent(input$run_model, {
    
    req(length(model_ids()) > 0)
    modeltype(list())
    results(list())
    performance(list())
    h <- as.numeric(input$forecast_horizon)
    
    model_lst <- list()
    res_lst <- list()
    per_lst <- list()
    for (i in seq_along(model_ids())) {
      
      model_type <- input[[paste0("model_type_", i)]]
      req(model_type)
      
      if (model_type == "KNN") {
        k_input <- input[[paste0("knn_k_", i)]]
        k <- as.numeric(unlist(strsplit(as.character(k_input), ",")))
        cf <- input[[paste0('knn_cf_', i)]]
        
        knn_result_df <- run_prediction_knn(h = h, k = k, cf = cf) %>% 
          left_join(latest_vintage_data(), by = c('year', 'quarter')) %>%
          select(year,quarter, cur_forecast, latest_forecast, latest_growth) %>%
          rename("cur_pred" = "cur_forecast",
                 "latest_pred" = "latest_forecast")
            
        
        knn_performance_df <- knn_result_df %>% 
            summarize(cur_mae = round(mean(abs(cur_pred - latest_growth)),2),
                      cur_rmse = round(sqrt(mean((cur_pred - latest_growth)^2)),2),
                      latest_mae = round(mean(abs(latest_pred - latest_growth)),2),
                     latest_rmse = round(sqrt(mean((latest_pred - latest_growth)^2)),2)) 
        model_lst <- append(model_lst, list(model_type))
        res_lst <- append(res_lst, list(knn_result_df))
        per_lst <- append(per_lst, list(knn_performance_df))
        
        
        
      }
      
      else if (model_type == "ADL") {
        ylag_input_adl <- input[[paste0("ylag_adl_", i)]]
        if (length(feature_series_ids()) == 0) {
          showNotification("Please add a feature for ADL or use AR model instead.", type = "error")
          return()
        }
        invalid_ids <- feature_series_ids()[!sapply(feature_series_ids(), is_valid_series)]
        
        
        if (length(invalid_ids) > 0) {
          showNotification(paste("The following series IDs are invalid:", paste(invalid_ids, collapse = ", ")))
          return()    
        }
        features <- c()
        feature_lags <- c() 
        feature_transforms <- c()
        for (feature in feature_series_ids()) {
          features <- c(features,feature)
        }
        
        for (feature_lag in feature_lag_series()) {
          feature_lags <- c(feature_lags, feature_lag)
        }
        for (feature_transform in feature_transform_series()) {
          feature_transforms <- c(feature_transforms, feature_transform)
        }
        
        adl_result_df <- run_adl_model(h = h, features = features, lag_y = ylag_input_adl, lags = feature_lags,units = feature_transforms) %>% 
          rename("latest_growth" = "value") 
        adl_per_df <- adl_result_df %>% summarize(cur_mae = round(mean(abs(cur_pred - latest_growth)),2),
                                      latest_mae = round(mean(abs(latest_pred - latest_growth)),2),
                                      cur_rmse = round(sqrt(mean((cur_pred - latest_growth)^2)),2), 
                                      latest_rmse =round(sqrt(mean((latest_pred - latest_growth)^2)),2)) 
                                      
        
        model_lst <- append(model_lst, list(model_type))
        res_lst <- append(res_lst, list(adl_result_df))
        per_lst <- append(per_lst, list(adl_per_df))
        
      }
      
      if (model_type == "AR") {
        ylag_input_ar <- input[[paste0("ylag_ar_", i)]]
        ar_result_df <- run_ar(h = h, p = ylag_input_ar)
        ar_per_df <- ar_result_df %>% summarize(cur_mae = round(mean(abs(cur_pred - latest_growth)),2),
                                                  latest_mae = round(mean(abs(latest_pred - latest_growth)),2),
                                                  cur_rmse = round(sqrt(mean((cur_pred - latest_growth)^2)),2), 
                                                  latest_rmse =round(sqrt(mean((latest_pred - latest_growth)^2)),2))
        
        model_lst <- append(model_lst, list(model_type))
        res_lst <- append(res_lst, list(ar_result_df))
        per_lst <- append(per_lst, list(ar_per_df))
        
     
      }
    }
    modeltype(model_lst)
    results(res_lst)
    modeltype(model_lst)
    results(res_lst)
    
    # Combine and rank performance
    perf_df <- do.call(rbind, lapply(seq_along(per_lst), function(i) {
      df <- per_lst[[i]]
      model <- model_lst[[i]]
      data.frame(
        model = model,
        cur_rmse = df$cur_rmse[1],
        cur_mae = df$cur_mae[1],
        latest_rmse = df$latest_rmse[1],
        latest_mae = df$latest_mae[1]
      )
    }))
    
    # Create ranking dataframes
    rank_cur <- perf_df %>%
      arrange(cur_rmse) %>%
      mutate(rank = row_number()) %>%
      select(rank, model, cur_rmse, cur_mae)
    
    rank_latest <- perf_df %>%
      arrange(latest_rmse) %>%
      mutate(rank = row_number()) %>%
      select(rank, model, latest_rmse, latest_mae)
    
    performance(list(cur = rank_cur, latest = rank_latest))

  })
  
  
  output$resulttable <- renderUI({
  req(results(), modeltype())
  req(length(results()) > 0)

  res <- results()
  type <- modeltype()

  output_list <- lapply(seq_along(res), function(i) {
    output_id <- paste0("result_", i)

    output[[output_id]] <- DT::renderDataTable({
      req(res[[i]])
      res[[i]]
    })

    tagList(
      h4(paste("Model", i, "-", type[[i]])),
      DT::DTOutput(output_id),
      tags$hr()
    )
  })

  do.call(tagList, output_list)
})
  ## Comparison Tab
  output$comparison <- renderUI({
    req(performance())
    
    output$cur_perf_table <- DT::renderDataTable({
      performance()$cur
    })
    
    
    output$latest_perf_table <- DT::renderDataTable({
      performance()$latest
    })
    # Bar Chart: Current Performance
    output$cur_perf_plot <- renderPlot({
      req(length(performance()) > 0)
      cur_df <- performance()$cur
      
      cur_df_long <- tidyr::pivot_longer(
        cur_df,
        cols = c("cur_rmse", "cur_mae"),
        names_to = "metric",
        values_to = "value"
      )
      
      ggplot(cur_df_long, aes(x = reorder(model, value), y = value, fill = metric)) +
        geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.5) +
        scale_fill_manual(values = c("cur_rmse" = "steelblue", "cur_mae" = "yellow2")) +
        labs(
          title = "Current Forecast: RMSE and MAE",
          x = "Model",
          y = "Error Value",
          fill = "Metric"
        ) +
        theme_minimal() 
        
    })
    
    #Bar Chart: Latest Forecast
    output$latest_perf_plot <- renderPlot({
      req(length(performance()) > 0)
      latest_df <- performance()$latest
      
      latest_df_long <- tidyr::pivot_longer(
        latest_df,
        cols = c("latest_rmse", "latest_mae"),
        names_to = "metric",
        values_to = "value"
      )
      
      ggplot(latest_df_long, aes(x = reorder(model, value), y = value, fill = metric)) +
        geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.5) +
        scale_fill_manual(values = c("latest_rmse" = "steelblue", "latest_mae" = "yellow2")) +
        labs(
          title = "Latest Forecast: RMSE and MAE",
          x = "Model",
          y = "Error Value",
          fill = "Metric"
        ) +
        theme_minimal() 
        
    })
    
    tagList(
      h6("Model Performance Comparison"),
    
      tabsetPanel(
        tabPanel("Current Forecast", 
                 plotOutput("cur_perf_plot"),
                 DT::DTOutput("cur_perf_table")),
        tabPanel("Latest Forecast", 
                 plotOutput("latest_perf_plot"),
                 DT::DTOutput("latest_perf_table"),)
      )
    )
  })
  # Visualization Tab
  output$modelplot <- renderUI({
    req(results())
    res <- results()
    types <- modeltype()
    
    output_list <- lapply(seq_along(res), function(i) {
      plot_id <- paste0("model_plot_", i)
      
      output[[plot_id]] <- renderPlot({
        df <- res[[i]]

        ggplot(df, aes(x = as.Date(zoo::as.yearqtr(paste0(df$year, " Q", df$quarter))))) +
          geom_line(size = 1,aes(y = cur_pred, color = "Current Forecast")) +
          geom_line(size = 1,aes(y = latest_pred, color = "Latest Forecast")) +
          geom_line(size = 1,aes(y = latest_growth, color = "Actual Growth"), linetype = "dashed") +
          scale_color_manual(values = c(
            "Current Forecast" = "blue",
            "Latest Forecast" = "indianred",
            "Actual Growth" = "black")) +
          scale_x_date(labels = function(x) zoo::format.yearqtr(x, "%YQ%q")) +
          labs(
            x = "Quarter", y = "GDP Growth (%)", color = NULL
          ) +
          theme_classic() +
          theme(legend.position = "top")
      })
      
      tagList(
        h4(paste("Model", i, "-", types[[i]])),
        plotOutput(plot_id),
        tags$hr()
      )
    })
    
    do.call(tagList, output_list)
  })
  
  
  ts_transform <- function(data) {
    year <- data$year[1]
    quarter <- data$quarter[1]
    ts(data$current_growth, start = c(year,quarter), freq = 4, names = "current_growth")
  }
  ts_transform_latest <- function(data) {
    year <- data$year[1]
    quarter <- data$quarter[1]
    ts(data$latest_growth, start = c(year,quarter), freq = 4, names = "latest_growth")
  }
  
  
  ########
  ## Count number of outofsample and get the forecast_list(), which is all the quarter we are making forecast
  noos_count <- reactive({
    count_oos_forecasts(start_year = as.numeric(input$vintage_year), start_quarter = as.numeric(input$vintage_period),
                        end_year = as.numeric(input$end_year), end_quarter = as.numeric(input$end_quarter))})
  
  determine_noos <- function(noos) {
    
    cur_year <- as.numeric(input$vintage_year)
    cur_period <- as.numeric(input$vintage_period)
    
    pred_year <- cur_year
    pred_period <- cur_period
    forecast_list <- data.frame('year'= numeric(0), 'quarter' = numeric(0))
    
    if (data_frequency() == 'quarterly') {
      for (i in 1:noos) {
        temp <- data.frame('year' = pred_year, 'quarter' = pred_period)
        forecast_list <- rbind(forecast_list, temp)
        if (pred_year >= 2025 && pred_period >= 1) {
          h = i - 1
          break
        }
        else {
          if (pred_period < 4) {
            pred_period = pred_period + 1
          }
          else {
            pred_year = pred_year + 1
            pred_period = 1
          }
        }
        
      }
    }
    else {
      if (pred_year >= 2025 && pred_period >= 4) {
        h = i - 1
        break
      }
      else {
        for (i in 1:h) {
          if (pred_month < 12) {
            pred_period = pred_period + 1
            
          }
          else {
            pred_year = pred_year + 1
            pred_period = 1
          }
          forecast_list <- append(forecast_list, list(list(year = pred_year, period = pred_period)))
        }
      }
    }
    list(noos = noos, forecast_list = forecast_list)

  }

  air <- reactive({
    noos <- noos_count()
    determine_noos(noos)
  })
  

  forecast_list <- reactive({
    air()$forecast_list
  })
  
  actual_data <- reactive({
    df <- data.frame(
      year = numeric(0),
      quarter = numeric(0),
      value = numeric(0)
    )
  
    for (i in 1:nrow(forecast_list()))  {
      item = forecast_list()[i,]
      year_f = item$year
      quarter_f = item$quarter
      
      if (quarter_f < 4) {
        v_quarter_f = quarter_f +1
        v_year_f = year_f
      }
      else {
        v_year_f = year_f + 1
        v_quarter_f = 1
        
      }

      #value <- cleaned_data() %>% filter(year == year, quarter == quarter, v_year == v_year, v_quarter == v_quarter) %>% pull(current_vintage)
      value <- latest_vintage_data() %>% filter(year == year_f, quarter == quarter_f) %>% pull(latest_growth)
      df <- rbind(df, data.frame(year = year_f, quarter = quarter_f, value = value))
    }
    
    df
  })
  
  
  
  
  
  
  ####### MODEL CONSTRUCTION #######
  
  
  
  #### KNN #### 

  # We have actual_data()
  # Now we need forecast_data from the real-time vintage vs latest_vintage
  
    
  
  # lag-step forecast
  run_prediction_knn <- function(h, k, cf) {
    results = data.frame(year = numeric(0), quarter = numeric(0), cur_forecast = numeric(0), latest_forecast = numeric(0))
    for (i in 1:nrow(forecast_list())) {
      item = forecast_list()[i,]
      year_f = item$year
      quarter_f = item$quarter
      data <- filter_function(v_year1 = year_f, v_quarter1 = quarter_f) %>% left_join(latest_vintage_data(), by = c("year", "quarter")) %>%
        select(year, quarter, current_growth, latest_growth) %>% filter(year >= 1965)
      data <- data[1:(nrow(data)-h+1),]
      train_data_cur <- data %>% select(-latest_growth)
      train_data_latest <- data %>% select(-current_growth)
      ts_train_cur <- ts_transform(train_data_cur) 
      ts_train_latest <- ts_transform_latest(train_data_latest)
      
      
      
      model_current <- knn_forecasting(ts_train_cur, h = h, lags = 1:4, k = k, msas = "MIMO", cf = cf)
      cur_pred <- tail(model_current$prediction, 1)
      model_latest <-  knn_forecasting(ts_train_latest, h = h, lags = 1:4, k = k, msas = "MIMO", cf = cf)
      latest_pred <- tail(model_current$prediction, 1)
      results[i, 'year'] = year_f
      results[i, 'quarter'] = quarter_f
      results[i, 'cur_forecast'] = cur_pred
      results[i, 'latest_forecast'] = latest_pred      
    
    }
    return(results)
  }
  

  ##### Linear Regression #####
  
  
  quarter_to_date <- function(year, quarter) {
    month_lookup <- c("01", "04", "07", "10")
    date_str <- paste0(year, "-", month_lookup[quarter], "-01")
    date_obj <- as.Date(date_str)
    return(date_obj)
  }
  
  date_to_year_quarter <- function(date) {
    date <- as.Date(date)  # ensure it's a Date object
    year <- as.numeric(format(date, "%Y"))
    month <- as.numeric(format(date, "%m"))
    quarter <- ceiling(month / 3)
    data.frame(year = year, quarter = quarter)
  }
  clean_feature <- function(feature_data) {
    result <- feature_data %>% 
      mutate(year = date_to_year_quarter(date)$year,
             quarter = date_to_year_quarter(date)$quarter) %>%
      select(year,quarter, series_id, value) %>%
      pivot_wider(names_from = series_id, values_from = value)
    return(result)
  }
  get_feature_data <- function(series_id, v_year, v_quarter, units = 'lin') {
    result_df <- fredr_series_observations(series_id = series_id, 
                                        #observation_start = quarter_to_date(1947, 1), 
                                        #observation_end = quarter_to_date(2024,4),
                                        units = 'lin',
                                        frequency = 'q',
                                        #vintage_dates = quarter_to_date(v_year, v_quarter),
                                        aggregation_method = 'avg')
    return(result_df)
  }


  get_lm_data <- function(features, v_year1, v_quarter1, units = units) {
    final_data <- cleaned_data() %>% filter(v_year == v_year1, v_quarter == v_quarter1) %>%
      select(year, quarter, current_vintage) %>% right_join(latest_vintage_data(), by = c('year', 'quarter')) %>% 
      select(-log_latest_vintage)
    for (i in 1:length(features)) {
      feature_data <- clean_feature(get_feature_data(series_id = features[i], 
                                                     v_year = next_year(year = v_year1, quarter = v_quarter1), 
                                                     v_quarter = next_quarter(year = v_year1,quarter =  v_quarter1),
                                                     units = units[i]))
      latest_feature_data <- clean_feature(get_feature_data(series_id = features[i], 
                                                            v_year = 2025,
                                                            v_quarter = 2,
                                                            units = units[i]))
      final_data <- final_data %>% full_join(feature_data, by = c('year', 'quarter'))
      final_data <- final_data %>% full_join(latest_feature_data, by = c('year', 'quarter'), suffix = c("_current", "_latest"))
    }
    return(final_data)
  }
  
  prediction_lm <- function(h, data, features, v_year1, v_quarter1) {
    for (feature in features) {
      lag_feature <- lag(data[[paste0(feature, "_current")]], h)
      latest_lag_feature <- lag(data[[paste0(feature, "_latest")]],h)
      lag_feature_df <- data.frame('year' = data$year, 'quarter' = data$quarter, 
                                   setNames(data.frame(c(lag_feature)), paste0(feature, "_lag", h, "_current")))
      latest_lag_feature_df <- data.frame('year' = data$year, 'quarter' = data$quarter, 
                                   setNames(data.frame(c(latest_lag_feature)), paste0(feature, "_lag", h, "_latest")))
      
      data <- data %>% full_join(lag_feature_df, by = c('year', 'quarter'))
      data <- data %>% full_join(latest_lag_feature_df, by = c('year', 'quarter'))
      data <- data %>% filter(year <= input$vintage_year)
    }
    
    # Current Prediction
    lagged_features_1 <- paste0(features,"_lag", h, "_current")
    formula1 <- as.formula(paste('current_vintage', "~", paste(lagged_features_1, collapse = " + ")))
    model1 <- lm(data = data[-nrow(data),] %>% drop_na(), formula = formula1)
    latest_input1 <- tail(data, 1) %>% select(!!!syms(lagged_features_1))
    
    pred1 <- predict(model1, newdata = latest_input1)
    
    # Latest Prediction
    lagged_features_2 <- paste0(features,"_lag", h, "_latest")
    formula2 <- as.formula(paste('latest_vintage', "~", paste(lagged_features_2, collapse = " + ")))
    model2 <- lm(data = data[-nrow(data),] %>% drop_na(), formula = formula2)
    latest_input2 <- tail(data, 1) %>% select(!!!syms(lagged_features_2))
    
    pred2 <- predict(model2, newdata = latest_input2)
    result <- data.frame('year' = v_year1, 'quarter' = v_quarter1, 'cur_pred' = pred1, 'latest_pred' = pred2)
    result <- result %>% left_join(latest_vintage_data(), by = c('year', 'quarter')) %>% select(-log_latest_vintage)
    result <- result %>% mutate(cur_mae = mean(abs(cur_pred - latest_vintage)),
                                latest_mae = mean(abs(latest_pred - latest_vintage)),
                                cur_mse = mean((cur_pred - latest_vintage)^2), 
                                latest_mse = mean((latest_pred - latest_vintage)^2))
    return(result)
    
  }
  
  next_quarter <- function(quarter, year) {
    if (quarter == 4) {
      return(1)
    } else {
      return(quarter + 1)
    }
  }
  
  next_year <- function(quarter, year) {
    if (quarter == 4) {
      return(year + 1)
    } else {
      return(year)
    }
  }
  
  
  
  
  

  
  
  #### AR ####
  
  ## Helper Function
  fitARp=function(Y,p,h){
    
    #Inputs: Y- predicted variable,  p - AR order, h -forecast horizon
    aux=embed(Y,p+h) #create p lags + forecast horizon shift (=h option)
    y=aux[,1] #  Y variable aligned/adjusted for missing data due to lags
    X=as.matrix(aux[,-c(1:(ncol(Y)*h))]) # lags of Y corresponding to forecast horizon 
    if(h==1){ 
      X.out=tail(aux,1)[1:ncol(X)] #retrieve last p observations if one-step forecast 
    }else{
      X.out=aux[,-c(1:(ncol(Y)*(h-1)))] #delete first (h-1) columns of aux,  
      X.out=tail(X.out,1)[1:ncol(X)] #last p observations to predict T+1 
    }
    
    
    model=lm(y~X) #estimate direct h-step AR(p) by OLS 
    coef=coef(model) #extract coefficients
    pred=c(1,X.out)%*%coef #make a forecast using the last few observations: a direct h-step forecast.
    #note the addition of a constant to the test observation vector
    return(list("pred"=pred)) #save estimated AR regression, prediction, and estimated coefficients
  }
  run_ar <- function(h, p) {
    results <- data.frame('year' = numeric(0),
                          'quarter' = numeric(0),
                          'cur_pred' = numeric(0),
                          'latest_pred' = numeric(0),
                          'latest_growth' = numeric(0))
                          
    
    for (i in 1:nrow(forecast_list())) {
      item <- forecast_list()[i,]
      year_f <- item$year
      quarter_f <- item$quarter 
      final_data <- filter_function(v_year1 = year_f, v_quarter1 = quarter_f) %>%
        select(year, quarter, current_growth) %>% right_join(latest_vintage_data(), by = c('year', 'quarter')) %>% 
        filter(year >= 1965, year <= year_f) %>% drop_na() %>%
        select(year,quarter, current_growth, latest_growth) 
      Y_cur <- as.matrix(final_data %>% pull(current_growth))
      Y_latest <- as.matrix(final_data %>% pull(latest_growth))
      model_cur <- fitARp(Y = Y_cur, p = p, h = h)
      model_latest <- fitARp(Y = Y_latest, p = p, h = h)
      pred_cur <- model_cur$pred[1]
      pred_latest <- model_latest$pred[1]
      result_df <- data.frame("year" = year_f, "quarter" = quarter_f, "cur_pred" = pred_cur, "latest_pred" = pred_latest)
      result_df <- result_df %>% left_join(actual_data(), by = c("year", "quarter")) %>% rename("latest_growth" ="value")
      results <- rbind(results, result_df)
    }
    return(results)
  }
    output$test10 <- renderTable({
      run_ar(1,2)
    })

  
  
  ##### ADL model #####
  quarter_to_date <- function(year, quarter) {
    month_lookup <- c("01", "04", "07", "10")
    date_str <- paste0(year, "-", month_lookup[quarter], "-01")
    date_obj <- as.Date(date_str)
    return(date_obj)
  }
  
  date_to_year_quarter <- function(date) {
    date <- as.Date(date)  # ensure it's a Date object
    year <- as.numeric(format(date, "%Y"))
    month <- as.numeric(format(date, "%m"))
    quarter <- ceiling(month / 3)
    data.frame(year = year, quarter = quarter)
  }
  clean_feature <- function(feature_data) {
    result <- feature_data %>% 
      mutate(year = date_to_year_quarter(date)$year,
             quarter = date_to_year_quarter(date)$quarter) %>%
      select(year,quarter, series_id, value) %>%
      pivot_wider(names_from = series_id, values_from = value)
    return(result)
  }
  get_feature_data <- function(series_id, v_year, v_quarter, units = 'lin') {
    result_df <- fredr_series_observations(series_id = series_id, 
                                           #observation_start = quarter_to_date(1963, 1), 
                                           #observation_end = quarter_to_date(2024,4),
                                           units = units,
                                           frequency = 'q',
                                           #vintage_dates = quarter_to_date(v_year, v_quarter),
                                           aggregation_method = 'avg')
    return(result_df)
  }
  
  
  get_adl_data <- function(features,  v_year2, v_quarter2, units) {
    final_data <- filter_function(v_year1 = v_year2, v_quarter1 = v_quarter2) %>%
      select(year, quarter, current_growth) %>% right_join(latest_vintage_data(), by = c('year', 'quarter')) %>%
      select(year,quarter, current_growth, latest_growth) 
    for (i in 1:length(features)) {
      
      
      latest_feature_data <- clean_feature(get_feature_data(series_id = features[i], 
                                                            v_year = 2025,
                                                            v_quarter = 2,
                                                            
                                                            units = units[i]))
      final_data <- final_data %>% full_join(latest_feature_data, by = c('year', 'quarter')) %>%
                    mutate(across(everything(), ~replace_na(.x, 0)))
    }
    final_data <- final_data %>% filter(year >= 1963, year <= v_year2) 
     
    return(final_data)
  }
  
  prediction_adl <- function(h, data, features, lags, lag_y, v_year1, v_quarter1) {
    start_idx <- 1965*4 
    end_idx <- v_year1 * 4 + (v_quarter1 - 1)
    n <- end_idx - start_idx + 1
    for (i in 1:lag_y) {
      y_lag <- lag(data$current_growth, h-1+i)
      
      y_lag_df <- data.frame('year' = data$year, 'quarter' = data$quarter, 
                             setNames(data.frame(c(y_lag)), paste0("current_growth_lag", h-1+i)))
      data <- data %>% full_join(y_lag_df, by = c("year", "quarter"))
    }
    for (i in 1:lag_y) {
      y_lag_latest <- lag(data$latest_growth, h-1+i)
      
      y_lag_latest_df <- data.frame('year' = data$year, 'quarter' = data$quarter, 
                             setNames(data.frame(c(y_lag_latest)), paste0("latest_growth_lag", h-1+i)))
      data <- data %>% full_join(y_lag_latest_df, by = c("year", "quarter"))
    }
    for (i in 1:length(features)) {
      feature <- features[i]
      lag <- lags[i]

      for (j in 1:lag) {
        
        
        latest_lag_feature <- lag(data[[paste0(feature)]], h-1+j)
        latest_lag_feature_df <- data.frame('year' = data$year, 'quarter' = data$quarter, 
                                            setNames(data.frame(c(latest_lag_feature)), paste0(feature, "_lag", h-1+j)))
        data <- data %>% full_join(latest_lag_feature_df, by = c('year', 'quarter'))
      }
      
    }
    
    
    
    
    #data <- data[, -c(5:(4+length(features)))]
    data <- data %>% filter(year >= 1965) %>% mutate
    data <- data[1:n,]
  
    # Current Prediction
    x_var <- names(data)
    x_var_cur <- x_var[c(5:(4+lag_y), (5+2*lag_y):length(x_var))]
    x_var_cur <- x_var_cur[x_var_cur %in% names(data)] 
    x_var_latest <- x_var[c(5+lag_y:length(x_var))] 
    x_var_latest <- x_var_latest[x_var_latest %in% names(data)] 
    
    
    formula1 <- as.formula(paste('current_growth', "~", paste(x_var_cur, collapse = " + ")))
    model1 <- lm(data = data %>% drop_na(), formula = formula1)
    latest_input1 <- data %>% filter(year == v_year1, quarter == v_quarter1) %>% select(!!!syms(x_var_cur))
    pred1 <- predict(model1, newdata = latest_input1)
    
    # Latest Prediction
    
    formula2 <- as.formula(paste('latest_growth', "~", paste(x_var_latest, collapse = " + ")))
    model2 <- lm(data = data %>% drop_na(), formula = formula2)
    latest_input2 <- data %>% filter(year == v_year1, quarter == v_quarter1) %>% select(!!!syms(x_var_latest))
    pred2 <- predict(model2, newdata = latest_input2)
    
    
    result <- data.frame('year' = v_year1, 'quarter' = v_quarter1, 'cur_pred' = pred1, 'latest_pred' = pred2)
    result <- result %>% left_join(actual_data(), by = c('year', 'quarter')) 
    #result <- result %>% mutate(cur_mae = mean(abs(cur_pred - value)),
    #                           latest_mae = mean(abs(latest_pred - value)),
    #                            cur_mse = mean((cur_pred - value)^2), 
    #                            latest_mse = mean((latest_pred - value)^2)) %>%
     # rename("latest_growth" = "value")
    return(result)
  }
  
  
  run_adl_model <- function(h,features, lags, lag_y, units) {
    results <- data.frame('year' = numeric(0),
                          'quarter' = numeric(0),
                          'cur_pred' = numeric(0),
                          'latest_pred' = numeric(0),
                          'latest_growth' = numeric(0),
                          'cur_mae' = numeric(0),
                          'latest_mae' = numeric(0),
                          'cur_mse' = numeric(0),
                          'latest_mse' = numeric(0))
    
    for (i in 1:nrow(forecast_list())) {
      item <- forecast_list()[i,]
      year_f <- item$year
      quarter_f <- item$quarter
      dat <- get_adl_data(features = features,v_year2 = year_f, 
                        v_quarter2 = quarter_f, units = units)
      result_df <- prediction_adl(h = h, data = dat, features = features, lags = lags,lag_y = lag_y, v_year1 = year_f,
                                 v_quarter1 = quarter_f)
      #result_df <- prediction_adl(h = 2, data = dat, features, lags = lags,lag_y = lag_y, v_year1 = year_f,v_quarter1 = quarter_f)
      results <- rbind(results, result_df)
      
    }
    
    return(results)
      
  }
  
 
  

  
  next_quarter <- function(quarter, year) {
    if (quarter == 4) {
      return(1)
    } else {
      return(quarter + 1)
    }
  }
  
  next_year <- function(quarter, year) {
    if (quarter == 4) {
      return(year + 1)
    } else {
      return(year)
    }
  }
  
  
  
  
  

  
  
  
  
  
  
  
 
  
  
  

  
  
  
  
  
  
  
  
  
  
  
}
