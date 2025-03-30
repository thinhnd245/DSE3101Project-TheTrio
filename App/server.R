library(tidyverse)
library(stringr)
library(readxl)
library(shiny)
library(tools)
library(lubridate)
library(zoo)
library(forecast)

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
  
  ## Data for building gdp model
  gdp <- reactive({
    filtered_data() %>% select(-log_gdp) 
  })
  
  ## Data for building gdp growth model
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
    ggplot(data = temp_dat, mapping = aes(x = as.Date(zoo::as.yearqtr(paste0(year, " Q", quarter))), y = gdp)) +
    scale_x_date(labels = function(x) zoo::format.yearqtr(x, "%YQ%q"))  +
    labs(x = "") +
    geom_line() +
    theme_classic()
    
  })
  log_gdp_plot <- reactive({
    temp_dat <- log_gdp() 
    ggplot(data = temp_dat, mapping = aes(x = as.Date(zoo::as.yearqtr(paste0(year, " Q", quarter))), y = log_gdp)) +
      scale_x_date(labels = function(x) zoo::format.yearqtr(x, "%YQ%q")) + 
    labs(x = "") +
    geom_line() +
    theme_classic()
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
  
  
  #############
  # Model Tab #
  #############
  
  ##### AR MODEL #####
  
  # Prepare data
  ar_model_data <- reactive({
    ts_transform(gdp())
  })
  
  
  
  observeEvent(input$run_model, {
      Y <- as.matrix(ar_model_data())
      h <- input$forecast_h
      noos <- input$noos
      if (noos >= length(Y)) {
        stop("Number Out of Sample exceeds the Data Points!")
      }
      
      if (input$estimation_mode == "custom") {
        
        # === Customize through AR(p) === #
        
        p <- input$ar_lag
        result <- arp.rolling.window(Y, noos = noos, p = p, h = h)
        forecast <- result$pred
        error <- result$errors
        output$ar_summary <- renderPrint({
          cat("AR(", p, ") Summary:\n", sep = "")
          cat("RMSE:", round(result$errors["rmse"], 4), "\n")
          cat("MAE :", round(result$errors["mae"], 4), "\n")
        })
        
        
      } else {  
        
        # === Optimized via auto.arima === #
        
        train_ts <- window(Y, end = Y[length(Y) - noos])
        auto_model <- forecast::auto.arima(train_ts)
        fc <- forecast::forecast(auto_model, h = noos)
        actual <- tail(Y, input$noos)
        Y_df <- data.frame(
          date = as.Date(time(Y)),
          value = as.numeric(Y)
        )
        plot_df <- data.frame(
          date = tail(time(Y), input$noos),
          actual = as.numeric(actual),
          forecast = as.numeric(fc$mean)
        )
        output$ar_plot <- renderPlot({
          ggplot(plot_df, aes(x = as.Date(date))) + 
            geom_line(data = Y_df, mapping = aes(x = date, y = value), color = "black") + 
            geom_line(mapping = aes(y = forecast), color = "red") + 
            scale_x_date(labels = function(x) zoo::format.yearqtr(x, "%YQ%q")) + 
            theme_classic()
          
        })
        
        output$ar_summary <- renderPrint({
          cat("Auto ARIMA Summary:\n")
          print(summary(auto_model))
          cat("\nRMSE:", round(sqrt(mean((tail(Y, noos) - fc$mean)^2)), 4), "\n")
        })
      }
  })
  
  ## Helper function ##
  
  ts_transform <- function(data) {
    start <- data$year[1]
    ts(data$gdp, start = start, freq = 4, names = "gdp")
  }
  
  fitARp=function(Y,p,h){
    
    #Inputs: Y- predicted variable,  p - AR order, h -forecast horizon
    
    
    aux=embed(Y,p+h) #create p lags + forecast horizon shift (=h option)
    y=aux[,1] #  Y variable aligned/adjusted for missing data due to lags
    X=as.matrix(aux[,-c(1:(ncol(Y)*h))]) # lags of Y (predictors) corresponding to forecast horizon (prevent leakage)  
    
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
    
    rmsfe=sqrt(sum(model$residuals^2)/nrow(X)) #get unadjusted rmsfe (ignoring estimation uncertainty)
    
    return(list("model"=model,"pred"=pred,"coef"=coef, "rmsfe"=rmsfe)) #save estimated AR regression, prediction, and estimated coefficients
  }
  
  
  
  
  arp.rolling.window=function(Y,noos,p=1,h=1){ #equality here  means deafult inputs
    
    save.coef=matrix(NA,noos,p+1) #blank matrix for coefficients at each iteration (constant+ p lags)
    save.pred=matrix(NA,noos,1) #blank for forecasts
    for(i in noos:1){  #NB: backwards FOR loop: going from noos down to 1
      
      Y.window=Y[(1+noos-i):(nrow(Y)-i),] #define the estimation window 
      Y.window=as.matrix(Y.window)
      winfit=fitARp(Y.window,p,h) #call the function to fit the AR(p) and generate h-step forecast
      save.coef[(1+noos-i),]=winfit$coef #save estimated coefficients
      save.pred[(1+noos-i),]=winfit$pred #save the forecast
      #cat("iteration",(1+noos-i),"\n") #display iteration number (useful for slower ML methods)
    }
    
    #Some useful post-prediction misc stuff:
    real=Y #get actual values
    #plot(real,type="l")
    #lines(c(rep(NA,length(real)-noos),save.pred),col="red") #padded with NA for blanks, plot predictions vs. actual
    
    rmse=sqrt(mean((tail(real,noos)-save.pred)^2)) #compute RMSE
    mae=mean(abs(tail(real,noos)-save.pred)) #compute MAE (Mean Absolute Error)
    errors=c("rmse"=rmse,"mae"=mae) #stack errors in a vector
    
    return(list("pred"=save.pred,"coef"=save.coef,"errors"=errors)) #return forecasts, history of estimated coefficients, and RMSE and MAE for the period.
  }
  
  
  ###### ADL MODEL ######
  
  # Helper function 
  fitADL=function(X,Y,p,h){
    aux_Y=embed(Y,p+h)
    
    aux_X= embed(X,p+h)
    #create p lags + forecast horizon shift (=h option)
    y=aux_Y[,1] #  Y variable align[d/adjusted for missing data due to lags
    X_matrix=cbind(aux_Y[,-1],aux_X[,-1])
    # lags of Y (predictors) corresponding to forecast horizon (prevent leakage)  
    
    if(h==1){ 
      X_out=tail(X_matrix,1) #retrieve last p observations if one-step forecast 
    }else{
      X_out=aux_X[,-(1:(ncol(X)*h))] #delete first (h-1) columns of aux,  
      X_out=tail(X_out,1) #last p observations to predict T+1 
    }
    
    
    
    model=lm(y~X_matrix) #estimate direct h-step AR(p) by OLS 
    coef=coef(model) #extract coefficients
    
    
    
    
    
    
    pred=c(1,X_out)%*%coef #make a forecast using the last few observations: a direct h-step forecast.
    #note the addition of a constant to the test observation vector
    
    rmsfe=sqrt(sum(model$residuals^2)/nrow(X_matrix)) #get unadjusted rmsfe (ignoring estimation uncertainty)
    
    return(list("model"=model,"pred"=pred,"coef"=coef, "rmsfe"=rmsfe))
  }
  
  arp.rolling.window_ADL=function(X,Y,noos,p=1,h=1){ #equality here  means deafult inputs
    
    
    save.coef=matrix(data = NA, nrow = noos, ncol = p+1) #blank matrix for coefficients at each iteration (constant+ p lags)
    save.pred=matrix(data = NA, nrow = noos,ncol = 1) #blank for forecasts
    for(i in noos:1){  #NB: backwards FOR loop: going from noos down to 1
      
      Y.window=Y[(1+noos-i):(nrow(Y)-i),] #define the estimation window 
      Y.window=as.matrix(Y.window)
      X.window=X[(1+noos-i):(nrow(Y)-i),]
      
      winfit=fitADL(X.window,Y.window,p,h) #call the function to fit the AR(p) and generate h-step forecast
      #save.coef[(1+noos-i),2]=winfit$coef #save estimated coefficients
      save.pred[(1+noos-i),]=winfit$pred #save the forecast
      #cat("iteration",(1+noos-i),"\n") #display iteration number (useful for slower ML methods)
    }
    
    #Some useful post-prediction misc stuff:
    real=Y #get actual values
    plot(real,type="l")
    lines(c(rep(NA,length(real)-noos),save.pred),col="red") #padded with NA for blanks, plot predictions vs. actual
    
    rmse=sqrt(mean((tail(real,noos)-save.pred)^2)) #compute RMSE
    mae=mean(abs(tail(real,noos)-save.pred)) #compute MAE (Mean Absolute Error)
    errors=c("rmse"=rmse,"mae"=mae) #stack errors in a vector
    
    return(list("pred"=save.pred,"coef"=save.coef,"errors"=errors)) #return forecasts, history of estimated coefficients, and RMSE and MAE for the period.
  }
  
}
