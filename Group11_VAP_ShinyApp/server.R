pacman::p_load(shiny, tidyverse, readr, plotly, forecast, stats, zoo, shinyjs, ggstatsplot)

# import data
temp_data <-read_rds("data/rds/temperature.rds")

Temp_YM <- temp_data %>% 
  group_by(Year, Month, Region) %>% 
  reframe(Date = Date,
          AveMeanTemp = round(mean(MeanTemp, na.rm = TRUE),1),
          MaxMaxTemp = max(MaxTemp, na.rm = TRUE),
          MinMinTemp = min(MinTemp, na.rm = TRUE)) %>% 
  distinct() %>% 
  ungroup() %>% 
  filter(!is.na(AveMeanTemp))

Temp_YM_allR <- temp_data %>% 
  group_by(Year, Month) %>% 
  reframe(Date = Date,
          AveMeanTemp = round(mean(MeanTemp, na.rm = TRUE),1),
          MaxMaxTemp = max(MaxTemp, na.rm = TRUE),
          MinMinTemp = min(MinTemp, na.rm = TRUE)) %>% 
  distinct() %>%
  ungroup() %>% 
  filter(!is.na(AveMeanTemp))

rain_data <-read_rds("data/rds/rainfall.rds")

Rain_YM <- rain_data %>% 
  group_by(Region, Year, Month) %>% 
  reframe(Date = Date,
          TotalRain = round(sum(TotalRainfall, na.rm = TRUE),1),
          TotalRain30 = sum(TotalRainfall30, na.rm = TRUE),
          TotalRain60 = sum(TotalRainfall60, na.rm = TRUE),
          TotalRain120 = sum(TotalRainfall120, na.rm = TRUE)) %>% 
  distinct() %>%
  ungroup() %>% 
  filter(!is.na(TotalRain))

Rain_YM_allR <- rain_data %>% 
  group_by(Year, Month) %>% 
  reframe(Date = Date,
          TotalRain = round(sum(TotalRainfall, na.rm = TRUE),1),
          TotalRain30 = sum(TotalRainfall30, na.rm = TRUE),
          TotalRain60 = sum(TotalRainfall60, na.rm = TRUE),
          TotalRain120 = sum(TotalRainfall120, na.rm = TRUE)) %>% 
  distinct() %>%
  ungroup() %>% 
  filter(!is.na(TotalRain))

weather_Y <-read_rds("data/rds/weather_y.rds")

# Define server logic
function(input, output, session) {
  
  output$correlationPlot <- renderPlot({
    var <- sym(input$variable)
    
    ggscatterstats(
      data = weather_Y,
      x = "MeanTemp",
      y = "TotalRainfall",
      method = input$method,
      type = input$type,
      marginal = input$marginal
    ) + 
      facet_wrap(vars(!!var))
  })
  
  forecastPlotReady <- reactiveValues(ok = FALSE)

  observeEvent(input$showPlotButton, {
    shinyjs::disable("showPlotButton")
    Sys.sleep(2)  # Simulate some processing time
    forecastPlotReady$ok <- TRUE
  })
  
  output$plotForecast <- renderPlotly({
    if (forecastPlotReady$ok) {
      shinyjs::enable("showPlotButton")
      
      if (input$variable == "Temperature") {
          if (input$region != "All") {
            temp <- Temp_YM %>%
              filter(Region == input$region) %>%
              rename(Value = AveMeanTemp)
          } else {
            temp <- Temp_YM_allR %>%
              rename(Value = AveMeanTemp)
          }
          displayText = "Temp"
          displayUnit = "°C"
        } else {
          if (input$region != "All") {
            temp <- Rain_YM %>%
              filter(Region == input$region) %>%
              rename(Value = TotalRain)
          } else {
            temp <- Rain_YM_allR %>%
              rename(Value = TotalRain)
          }
          displayText = "Rainfall"
          displayUnit = "mm"
        }
      
      minDate = min(temp$Date)
      maxDate = max(temp$Date)
      ts_data <- ts(temp$Value,
                    start = c(year(minDate), month(minDate)),
                    end = c(year(maxDate), month(maxDate)), frequency = 12)
      
      actual_df <- data.frame(Date = time(ts_data), Actual = ts_data)
      actual_df$Period <- format(as.Date(actual_df$Date,
                                         origin = minDate), "%Y-%m")
      switch(input$model,
             "ARIMA" = { model = auto.arima(ts_data, p = 5, seasonal = TRUE)},
             "Holt-Winters" = { model <- HoltWinters(ts_data)},
             "Seasonal & Trend Decomposition" = {model <- stl(ts_data, s.window="periodic") }
      )
  
      forecast_values <- forecast(model, h = as.numeric(input$years) * 12, 
                                  level = c(as.numeric(input$confidence)))
  
      forecast_df <- data.frame(Date = time(forecast_values$mean), 
                                Forecast = forecast_values$mean, 
                                LowerV = forecast_values$lower, 
                                UpperV = forecast_values$upper)
  
      forecast_df$Period <- format(as.Date(forecast_df$Date, 
                                           origin = minDate), "%Y-%m")
  
      LowerV <- paste("X", input$confidence, ".", sep = "")
      HigherV <- paste("X", input$confidence, "..1", sep = "")
      names(forecast_df)[names(forecast_df) == HigherV] <- "UpperV"
      names(forecast_df)[names(forecast_df) == LowerV] <- "LowerV"
  
      #cannot go negative
      forecast_df$LowerV <- pmax(forecast_df$LowerV, 0)
     
      plot_ly() %>%
        add_lines(data = forecast_df, x = ~Date, y = ~Forecast, 
                  name = "Forecast", line = list(color = 'blue'), 
                  hoverinfo = "text", 
                  text = ~paste("Year-Month: ", Period, 
                                "<br>", displayText, ": ", 
                                round(Forecast, 1), displayUnit))%>%
        add_lines(data = actual_df, x = ~Date, y = ~Actual, 
                  name = "Actual", line = list(color = 'red'), 
                  hoverinfo = "text", 
                  text = ~paste("Year-Month: ", Period, 
                                "<br>", displayText, ": ", 
                                Actual, displayUnit)) %>%
        add_ribbons(data = forecast_df, x = ~Date, 
                    ymin = ~LowerV, ymax = ~UpperV, 
                    name = paste(input$confidence, "% CI"), 
                    fillcolor = 'lightblue',
                    opacity = 0.5,
                    hoverinfo = "text", 
                    text = ~paste("Year-Month: ", Period, 
                                  "<br>CI:", round(LowerV,1), displayUnit,
                                  "-", round(UpperV,1), displayUnit)) %>%
        layout(title = paste("Forecasting ", displayText,
                             " for the next ", input$years,
                             " years using ", input$model),
               xaxis = list(title = "Year"),
               yaxis = list(title = displayText))
      
    }
  })
}

