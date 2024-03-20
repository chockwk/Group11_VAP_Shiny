pacman::p_load(shiny, tidyverse, dplyr, readr, plotly, forecast, stats, zoo, shinyjs, ggstatsplot, sf, tmap, terra, gstat, viridis, automap, sp, spacetime, raster)

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

# Geospatial

stations <- read.csv("data/aspatial/RainfallStation.csv")

mpsz <- st_read(dsn = "data/geospatial", layer = "MPSZ-2019") %>% 
  st_transform(crs=3414)

tpdata <- temp_data %>% 
  dplyr::select(Station, MeanTemp) %>% 
  left_join(stations)

tpdata_sf <- st_as_sf(tpdata, coords = c("Longitude", "Latitude"),
                      crs = 4326) %>% 
  st_transform(crs = 3414)

rfdata <- rain_data %>% 
  dplyr::select(Station, TotalRainfall) %>% 
  left_join(stations)

rfdata_sf <- st_as_sf(rfdata, coords = c("Longitude", "Latitude"), # xaxis then yaxis
                      crs = 4326) %>% 
  st_transform(crs = 3414)

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
  
  # Geospatial
  
  output$geo_plot <- renderPlot({
    
    # Create grid
    grid <- terra::rast(mpsz, nrows = 690, ncols = 1075)
    
    # Extract xy coordinates
    xy <- terra::xyFromCell(grid, 1:ncell(grid))
    
    coop <- st_as_sf(as.data.frame(xy), coords = c("x", "y"), crs = st_crs(mpsz))

    coop <- st_filter(coop, mpsz)
    
    # Check which variable to analyze
    if (input$analysis_variable == "Temperature") {
      data_sf <- tpdata_sf
      analysis_var <- "MeanTemp"
      title <- "Distribution of Mean Temperature"
    } else {
      data_sf <- rfdata_sf
      analysis_var <- "TotalRainfall"
      title <- "Distribution of Total Rainfall"
    }
    
    # Perform geostatistical analysis
    model <- switch(input$model_option, 
                    Sph = "Sph", 
                    Exp = "Exp", 
                    Gau = "Gau", 
                    Lin = "Lin")
    
    res <- gstat::gstat(formula = as.formula(paste(analysis_var, "~ 1")), 
                        locations = data_sf, 
                        nmax = input$n_neighbors,
                        set = list(idp = 0),
                        model = vgm(psill = 0.5, model = model, range = input$range_param, nugget = 0.1))
    
    resp <- predict(res, coop)
    
    resp$x <- st_coordinates(resp)[, 1]
    resp$y <- st_coordinates(resp)[, 2]
    resp$pred <- resp$var1.pred
    
    pred <- terra::rasterize(resp, grid, 
                             field = "pred", 
                             fun = "mean")
    
    # Plot the result
    tmap_options(check.and.fix = TRUE)
    tmap_mode("plot")
    tm_shape(pred) + 
      tm_raster(alpha = 0.6, palette = "viridis") +
      tm_layout(main.title = "Distribution of Mean Temperature",
                main.title.position = "center",
                main.title.size = 1.2,
                legend.height = 0.45, 
                legend.width = 0.35,
                frame = TRUE) +
      tm_compass(type = "8star", size = 2) +
      tm_scale_bar() +
      tm_grid(alpha = 0.2)
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
  
  observeEvent(input$showPlotButton, {
    plotcorrelation(input$variable, input$method, input$association_type, input$marginal_type)
  })
  
  
  # Forecast
  
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

