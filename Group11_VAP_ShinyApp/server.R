pacman::p_load(shiny, tidyverse, ggrepel, plotly, forecast, stats, zoo, shinyjs, ggstatsplot, sf, tmap, terra, viridis, sp, raster, gstat, automap)

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

### ---- Data prep for CDA ----

Temp_Annual <- temp_data %>%
  group_by(Station, Region, Year) %>%
  summarise(Avg_Mean_Temp = round(mean(MeanTemp, na.rm = TRUE),1),
            Avg_Max_Temp = round(mean(MaxTemp, na.rm = TRUE),1),
            Avg_Min_Temp = round(mean(MinTemp, na.rm = TRUE),1),
            Max_Temp = round(max(MaxTemp, na.rm = TRUE),1),
            Min_Temp = round(min(MinTemp, na.rm = TRUE),1)) %>%
  distinct() %>%
  ungroup() %>%
  filter(!is.na(Avg_Mean_Temp))

Temp_Monthly <- temp_data %>%
  group_by(Station, Region, Year, Month) %>%
  summarise(Avg_Mean_Temp = round(mean(MeanTemp, na.rm = TRUE),1),
            Avg_Max_Temp = round(mean(MaxTemp, na.rm = TRUE),1),
            Avg_Min_Temp = round(mean(MinTemp, na.rm = TRUE),1),
            Max_Temp = round(max(MaxTemp, na.rm = TRUE),1),
            Min_Temp = round(min(MinTemp, na.rm = TRUE),1)) %>%
  distinct() %>%
  ungroup() %>%
  filter(!is.na(Avg_Mean_Temp))

Rainfall_Annual <- rain_data %>%
  group_by(Station, Region, Year) %>% 
  reframe(Total_Rf = round(sum(TotalRainfall, na.rm = TRUE),1),
          Total_Rf_30 = round(sum(TotalRainfall30, na.rm = TRUE),1),
          Total_Rf_60 = round(sum(TotalRainfall60, na.rm = TRUE),1),
          Total_Rf_120 = round(sum(TotalRainfall120, na.rm = TRUE),1),
          Avg_Total_Rf = round(mean(TotalRainfall, na.rm = TRUE),1),
          Avg_Total_Rf30 = round(mean(TotalRainfall30, na.rm = TRUE),1),
          Avg_Total_Rf60 = round(mean(TotalRainfall60, na.rm = TRUE),1),
          Avg_Total_Rf120 = round(mean(TotalRainfall120, na.rm = TRUE),1),
          Min_Total_Rf = round(min(TotalRainfall, na.rm = TRUE),1),
          Max_Total_Rf = round(max(TotalRainfall, na.rm = TRUE),1)) %>% 
  distinct() %>% 
  ungroup() %>% 
  filter(!is.na(Total_Rf))

Rainfall_Monthly <- rain_data %>%
  group_by(Station, Region, Year, Month) %>% 
  reframe(Total_Rf = round(sum(TotalRainfall, na.rm = TRUE),1),
          Total_Rf_30 = round(sum(TotalRainfall30, na.rm = TRUE),1),
          Total_Rf_60 = round(sum(TotalRainfall60, na.rm = TRUE),1),
          Total_Rf_120 = round(sum(TotalRainfall120, na.rm = TRUE),1),
          Avg_Total_Rf = round(mean(TotalRainfall, na.rm = TRUE),1),
          Avg_Total_Rf30 = round(mean(TotalRainfall30, na.rm = TRUE),1),
          Avg_Total_Rf60 = round(mean(TotalRainfall60, na.rm = TRUE),1),
          Avg_Total_Rf120 = round(mean(TotalRainfall120, na.rm = TRUE),1),
          Min_Total_Rf = round(min(TotalRainfall, na.rm = TRUE),1),
          Max_Total_Rf = round(max(TotalRainfall, na.rm = TRUE),1)) %>% 
  distinct() %>% 
  ungroup() %>% 
  filter(!is.na(Total_Rf))

# Timeseries

# Prepare temperature data
MeanTemp_Year <- temp_data %>% 
  group_by(Year) %>% 
  summarise(MeanTemp_Year = round(mean(MeanTemp, na.rm = TRUE), 1))

temp_time <- left_join(temp_data, MeanTemp_Year, by = "Year")

# Prepare rainfall data
TotalRainfall_Year <- rain_data %>% 
  group_by(Year) %>% 
  summarise(MeanRainfall_Year = round(mean(TotalRainfall, na.rm = TRUE), 1))

rainfall_time <- left_join(rain_data, TotalRainfall_Year, by = "Year")

# Correlation

weather_YM <- merge(rain_data, temp_data, by=c("Station", "Region", "Year", "Month", "Date"))

weather_Y <- weather_YM %>%
  dplyr::select(-Month, -Date) %>%
  group_by(Station, Region, Year) %>%
  summarise(
    MeanTemp = round(mean(MeanTemp, na.rm = TRUE), 1),
    MaxTemp = round(mean(MaxTemp, na.rm = TRUE), 1),
    MinTemp = round(mean(MinTemp, na.rm = TRUE), 1),
    TotalRainfall = round(mean(TotalRainfall, na.rm = TRUE), 1),
    TotalRainfall30 = round(mean(TotalRainfall30, na.rm = TRUE), 1),
    TotalRainfall60 = round(mean(TotalRainfall60, na.rm = TRUE), 1),
    TotalRainfall1120 = round(mean(TotalRainfall120, na.rm = TRUE), 1)
  ) %>%
  ungroup()



# Define server logic
function(input, output, session) {
  
  # Timeseries
  
  observeEvent(input$showPlotButton, {
    
    # Temperature plot
    output$temp_cycle_plot <- renderPlot({
      
      # User input
      req(input$selected_years)
      
      # Filtering the dataframe for the selected years
      cycle_input <- temp_time %>%
        filter(Year %in% input$selected_years)
    
      # Define darker pastel colors
      palette <- c("gold1", "orange2", "darkorange", "darkorange1", "tomato1", "tomato3", "tomato4")
      
      # Plot with darker pastel colors
      ggplot(data = cycle_input, aes(x = Month, y = MeanTemp, group = Year, color = as.factor(Year))) +
        geom_hline(aes(yintercept = MeanTemp_Year), color = "black", alpha = 1.0, size = 0.4) +
        geom_line(alpha = 0.6) +
        geom_text(aes(x = 1, y = MeanTemp_Year - 0.05, label = paste0("Mean: ", MeanTemp_Year)),
                  hjust = -0.1, vjust = 0.5, color = "black", size = 3.5) +
        facet_wrap(~Year, scales = "free_y") + 
        labs(x = "Month", y = "Mean Temperature", title = "Temperature Change over Selected Years") +
        theme_bw() +
        theme(legend.position = "none",
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.title = element_text(size = 10),
              title = element_text(size =12),
              axis.text.y = element_text(size = 8),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              strip.background = element_blank(),
              strip.text = element_text(size = 12)) +
        scale_color_manual(values = palette)
    })
    
    # Rainfall plot
    output$rain_cycle_plot <- renderPlot({
      
      # User input
      req(input$selected_years)
      
      # Filtering the dataframe for the selected years
      cycle_input <- rainfall_time %>%
        filter(Year %in% input$selected_years)
      
      # Generate a color palette with a sufficient number of colors
      number_of_years <- length(unique(cycle_input$Year))
      palette <- colorRampPalette(c("steelblue1", "dodgerblue", "dodgerblue3", "royalblue3", "blue3", "blue4", "darkblue"))(number_of_years)
      
      # Plot with darker pastel colors
      ggplot(data = cycle_input, aes(x = Month, y = TotalRainfall, group = Year, color = as.factor(Year))) +
        geom_hline(aes(yintercept = MeanRainfall_Year), color = "black", alpha = 1.0, size = 0.4) +
        geom_line(alpha = 0.6) +
        geom_text(aes(x = 1, y = MeanRainfall_Year - 0.05, label = paste0("Mean: ", MeanRainfall_Year)),
                  hjust = -0.1, vjust = 0.5, color = "black", size = 3.5) +
        facet_wrap(~Year, scales = "free_y") +
        labs(x = "Month", y = "Total Rainfall", title = "Rainfall Change over Selected Years") +
        theme_bw() +
        theme(legend.position = "none",
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.title = element_text(size = 10),
              title = element_text(size =12),
              axis.text.y = element_text(size = 8),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              strip.background = element_blank(),
              strip.text = element_text(size = 12)) +
        scale_color_manual(values = palette)
    })
    
  })
  
  # Geospatial
  
  # Data loading should be handled outside of reactive context if they are static
  stations <- read.csv("data/aspatial/RainfallStation.csv")
  mpsz <- st_read(dsn = "data/geospatial", layer = "MPSZ-2019") %>% 
    st_transform(crs=3414)
  
  # Assuming temp_data and rain_data are already loaded into the R session
  
  output$geo_plot <- renderPlot({
    # Check which variable to analyze and select the corresponding data frame
    data_sf <- if (input$analysis_variable == "Temperature") {
      temp_data %>% 
        dplyr::select(Station, MeanTemp) %>% 
        left_join(stations, by = "Station") %>% 
        st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
        st_transform(crs = 3414)
    } else {
      rain_data %>% 
        dplyr::select(Station, TotalRainfall) %>% 
        left_join(stations, by = "Station") %>% 
        st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
        st_transform(crs = 3414)
    }
    
    # Prepare for geospatial analysis
    analysis_var <- if(input$analysis_variable == "Temperature") "MeanTemp" else "TotalRainfall"
    
    # Conduct geospatial analysis using gstat
    model <- vgm(psill = 0.5, model = input$model_option, range = input$range_param, nugget = 0.1)
    res <- gstat(formula = as.formula(paste(analysis_var, "~ 1")), 
                 data = data_sf,
                 nmax = input$n_neighbors,
                 model = model)
    
    # Predict the spatial distribution
    grid <- terra::rast(mpsz, nrows = 690, ncols = 1075)
    xy <- terra::xyFromCell(grid, 1:ncell(grid))
    coop <- st_as_sf(as.data.frame(xy), coords = c("x", "y"), crs = st_crs(mpsz))
    coop <- st_filter(coop, mpsz)
    resp <- predict(res, coop)
    
    resp$x <- st_coordinates(resp1)[,1]
    resp$y <- st_coordinates(resp1)[,2]
    resp$pred <- resp1$var1.pred
    
    pred <- terra::rasterize(resp, grid, field = "pred", fun = "mean")
    
    # Plot using tmap
    tmap_mode("plot")
    tm <- tm_shape(pred) +
      tm_raster(alpha = 0.6, palette = "viridis") +
      tm_layout(main.title = paste("Geospatial Analysis Result:", input$analysis_variable)) +
      tm_compass(type = "8star", size = 2) +
      tm_scale_bar() +
      tm_grid(alpha = 0.2)
    
    # Print the plot to the Shiny app
    print(tm)
  })
  
  
  # Correlation
  
  options(scipen = 999)
  
  plotcorrelation <- function(a, method = "auto", association_type = "np", marginal_type = "histogram") {
    if (!is.character(a) || !(a %in% names(weather_Y))) {
      stop("Please provide a valid column name as a string.")
    }
    
    var <- rlang::sym(a)
    
    output$correlationPlot <- renderPlot({
      ggscatterstats(
        data = weather_Y,
        x = "MeanTemp",  
        y = "TotalRainfall", 
        method = method,  # Smoothing method: "auto", "lm", "glm", "gam", "loess", or a function
        type = association_type,  # Association type: "p" (parametric), "np" (nonparametric), "r" (robust)
        marginal = marginal_type  # Marginal distribution type: "histogram", "boxplot", "density", "violin", "densigram"
      ) + 
        facet_wrap(vars(!!var)) 
    })
  }
  
  observeEvent(input$CorrPlot_Button, {
    plotcorrelation(input$variable, input$method, input$association_type, input$marginal_type)
  })
  
  # CDA
  
  plot_station_temp <- function(measurement, 
                                selected_stations = c("Admiralty", "Ang Mo Kio", "Boon Lay (East)"), 
                                metric = "Avg_Mean_Temp", 
                                p_type = "boxviolin", 
                                t_type = "nonparametric", 
                                pair_disp = "significant", 
                                conf = 0.95) {
    if (measurement == "Monthly") {
      data <- Temp_Monthly %>%
        filter(Station %in% selected_stations)}
    else {data <- Temp_Annual %>%
      filter(Station %in% selected_stations)}
    
    
    output$station_temp <- renderPlot({
      ggbetweenstats(
        data = data,
        x = Station,
        y = !!sym(metric),
        plot.type = p_type,
        type = t_type,
        pairwise.display = pair_disp)
    })
  }
  
  observeEvent(input$ByStation_Button, {
    plot_station_temp(input$measurement, 
                      input$station,
                      input$metric,
                      input$plot_type,
                      input$test_type,
                      input$pair_display,
                      input$conf_inv)
  })
  
  # Forecast
  
  forecastPlotReady <- reactiveValues(ok = FALSE)

  observeEvent(input$showPlotButton, {
    shinyjs::disable("Forecast_Button")
    Sys.sleep(2)  # Simulate some processing time
    forecastPlotReady$ok <- TRUE
  })
  
  output$plotForecast <- renderPlotly({
    if (forecastPlotReady$ok) {
      shinyjs::enable("Forecast_Button")
      
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

