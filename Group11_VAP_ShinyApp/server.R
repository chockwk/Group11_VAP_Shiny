pacman::p_load(shiny, gstat, tidyverse, ggrepel, DT, plotly, 
               forecast, stats, shinyjs, ggstatsplot, gganimate, 
               ggthemes, sf, tmap, terra, viridis, sp, raster, datagovsgR, 
               tsibble, fable, forecast, tseries, urca, tidyr, ggHoriPlot)

# import data
temp_data <-read_rds("data/rds/temperature.rds")
rain_data <-read_rds("data/rds/rainfall.rds")

#### Forecast data ####
merged_data <- merge(temp_data, rain_data, 
                     by = c("Date", "Region", "Station")) 

data_ts <- merged_data %>%     
  group_by(Date) %>% 
  summarise(AveTemp = mean(MeanTemp, na.rm = TRUE),
            MaxTemp = max(MaxTemp, na.rm = TRUE),
            MinTemp = ifelse(all(MinTemp == 0), NA, min(MinTemp[MinTemp != 0], na.rm = TRUE)),
            Rainfall = mean(TotalRainfall)) %>% 
  mutate(YearMonth = yearmonth(Date)) %>% 
  ungroup()

data_EDA <- data_ts %>% 
  as_tsibble(index = Date) 

data_ts <- data_ts %>% 
  as_tsibble(index = (YearMonth)) 

data_region_ts <- merged_data %>%     
  group_by(Region, Date) %>% 
  summarise(AveTemp = mean(MeanTemp, na.rm = TRUE),
            MaxTemp = max(MaxTemp, na.rm = TRUE),
            MinTemp = ifelse(all(MinTemp == 0), NA, min(MinTemp[MinTemp != 0], na.rm = TRUE)),
            Rainfall = mean(TotalRainfall)) %>% 
  mutate(YearMonth = yearmonth(Date)) %>% 
  ungroup()

#### CDA ####

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

#### Timeseries data ####

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

#### Correlation data ####

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

#### Geospatial data ####

stations <- read.csv("data/aspatial/RainfallStation.csv")
mpsz <- st_read(dsn = "data/geospatial", layer = "MPSZ-2019") %>% 
  st_transform(crs=3414)

station_to_PA <- c(
  "Admiralty" = "WOODLANDS",
  "Ang Mo Kio" = "ANG MO KIO",
  "Boon Lay (East)" = "BOON LAY",
  "Changi" = "CHANGI",
  "Choa Chu Kang (South)" = "CHOA CHU KANG",
  "Clementi" = "CLEMENTI",
  "Clementi" = "QUEENSTOWN",
  "East Coast Parkway" = "BEDOK",
  "Jurong (West)" = "JURONG WEST",
  "Khatib" = "YISHUN",
  "Marina Barrage" = "MARINE PARADE",
  "Marina Barrage" = "MARINA EAST",
  "Marina Barrage" = "MARINA SOUTH",
  "Marina Barrage" = "DOWNTOWN CORE",
  "Newton" = "NEWTON",
  "Newton" = "NOVENA",
  "Newton" = "ORCHARD",
  "Newton" = "MUSUEM",
  "Newton" = "RIVER VALLEY",
  "Pasir Panjang" = "PASIR PANJANG",
  "Paya Lebar" = "PAYA LEBAR",
  "Seletar" = "SELETAR",
  "Sembawang" = "SEMBAWANG",
  "Tai Seng" = "HOUGANG",
  "Tengah" = "TENGAH",
  "Tuas South" = "TUAS"
)

weather_Y$PA <- station_to_PA[weather_Y$Station]
weather_Y <- weather_Y[, c("PA", setdiff(names(weather_Y), "PA"))]
mpszweather <- left_join(mpsz, weather_Y, by = c("PLN_AREA_N" = "PA"))

mpszweather <- mpszweather %>% 
  filter(!if_all(c(Station, MeanTemp, TotalRainfall), is.na))
head(mpszweather)



# Define server logic
function(input, output, session) {
  
  #### Dashboard Animation ####
  
  # Render temp_plot
  output$temp_plot <- renderUI({
    tags$div(style = "text-align: left;", 
             imageOutput("temp_animation")
    )
  })
  
  # Render rainfall_plot
  output$rainfall_plot <- renderUI({
    tags$div(style = "text-align: left;", 
             imageOutput("rainfall_animation")
    )
  })
  
  # Load and display temp_plot GIF
  output$temp_animation <- renderImage({
    list(src = "data/images/tempplot.gif",
         contentType = "image/gif",
         alt = "Temperature Plot")
  }, deleteFile = FALSE)
  
  # Load and display rainfall_plot GIF
  output$rainfall_animation <- renderImage({
    list(src = "data/images/rainplot.gif",
         contentType = "image/gif",
         alt = "Rainfall Plot")
  }, deleteFile = FALSE)
  
  #### Live Forecast ####
  
  # Current Time
  output$currentTime <- renderPrint({
    format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  })
  
  
  # Air Temperature
  output$airTemperature <- renderText({
    # Assuming weather_reading function retrieves weather readings
    date_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
    weather_data <- weather_reading(date = date_time)
    
    # Define a named vector to map station IDs to their corresponding names
    station_names <- c("S43" = "North", "S44" = "West", "S115" = "East", "S111" = "Central")
    
    # Extract station IDs and temperature values
    stations <- weather_data$air_temp$station_id
    temperatures <- weather_data$air_temp$value
    
    # Initialize an empty character vector to store formatted temperature readings
    formatted_temperatures <- character(length(stations))
    
    # Loop through each station and temperature value, and format them
    for (i in seq_along(stations)) {
      # Get the corresponding station name
      station_name <- ifelse(stations[i] %in% names(station_names), station_names[stations[i]], stations[i])
      formatted_temperatures[i] <- paste(station_name, ": ", temperatures[i], "°C", sep = "")
    }
    
    # Combine formatted temperatures into a single string with line breaks
    paste(formatted_temperatures, collapse = "\n")
  })
  
  # UV Index
  output$uvIndex <- renderPrint({
    formatted_datetime <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
    latest_uvi <- head(uvi(formatted_datetime), n = 1)
    print(latest_uvi$value)
  })
  
  # PSI
  output$psiData <- renderPrint({
    # Assuming psi function retrieves PSI data
    date_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
    psi_data <- psi(date = date_time)
    print(psi_data)
  })
  
  # Function to retrieve weather data
  getWeatherData <- reactive({
    current_time <- Sys.time()
    formatted_date <- format(current_time, "%Y-%m-%d")
    formatted_time <- format(current_time, "%H:%M:%S")
    formatted_datetime <- paste(formatted_date, formatted_time, sep = "T")
    
    # Wrap API call in tryCatch to handle errors
    tryCatch({
      weather_data <- weather_forecast(formatted_datetime)
      return(weather_data)
    }, error = function(e) {
      # Return NULL or a default value if API call fails
      return(NULL)
    })
  })
  
  output$weatherTable <- renderDataTable({
    weather_data <- getWeatherData()
    if (is.null(weather_data)) {
      return(data.frame(Area = NA, Forecast = NA))
    }
    weather_table <- data.frame(
      Area = weather_data$area,
      Forecast = weather_data$forecast
    )
    datatable(weather_table, options = list(pageLength = 10))
  })
  
  
  #### Timeseries ####
  
  observeEvent(input$showPlotButton, {
    
    # Temperature plot
    output$temp_cycleplot <- renderPlot({
      
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
    
    output$temp_horiplot <- renderPlot({
      
      # User input
      req(input$selected_years)
      
      # Filtering the dataframe for the selected years
      hori_input <- temp_time %>%
        filter(Year %in% input$selected_years)
      
      # Plot with darker pastel colors
      ggplot(data = hori_input) +
        geom_horizon(aes(x = Month, y = MeanTemp),
                     origin = "midpoint",
                     horizonscale = 6) +
        facet_grid(Station ~ .) +
        theme_few() +
        scale_fill_hcl(palette = 'RdBu') +
        theme(panel.spacing.y = unit(0, "lines"),
              strip.text.y = element_text(size =5, angle = 0, hjust = 0),
              legend.position = "none",
              axis.text.x = element_text(size = 7),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              panel.border = element_blank())
    })
    
    # Rainfall plot
    output$rain_cycleplot <- renderPlot({
      
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
    
    output$rain_horiplot <- renderPlot({
      
      # User input
      req(input$selected_years)
      
      # Filtering the dataframe for the selected years
      hori_input <- rainfall_time %>%
        filter(Year %in% input$selected_years)
      
      
      # Plot with darker pastel colors
      ggplot(data = hori_input) +
        geom_horizon(aes(x = Month, y = TotalRainfall),
                     origin = "midpoint",
                     horizonscale = 6) +
        facet_grid(Station ~ .) +
        theme_few() +
        scale_fill_hcl(palette = 'RdBu') +
        theme(panel.spacing.y = unit(0, "lines"),
              strip.text.y = element_text(size =5, angle = 0, hjust = 0),
              legend.position = "none",
              axis.text.x = element_text(size = 7),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              panel.border = element_blank())
    })
    
  })
  
  #### Geospatial ####
  
  output$temp_choromap <- renderPlot({
    
    # Plot the outline of mpsz
    outline <- tm_shape(mpsz) +
      tm_borders()
    
    # Plot mpszweather on top of the outline
    temp <- tm_shape(mpszweather) +
      tm_polygons(col = "MeanTemp", palette = "Oranges", style = input$style_param) +
      tm_layout(main.title = "Choropleth Map") 
    
    # Combine the outline and weather map
    tempoutline <- outline + temp
    
    # Display the map
    print(tempoutline)
    tmap_mode("plot")
    tmap_options(check.and.fix = TRUE)
  })
  
  output$temp_geoplot <- renderPlot({
    tpdata_sf <- temp_data %>% 
      dplyr::select(Station, MeanTemp) %>% 
      left_join(stations, by = "Station") %>% 
      st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
      st_transform(crs = 3414)

    grid <- terra::rast(mpsz, nrows = 690, ncols = 1075)
    xy <- terra::xyFromCell(grid, 1:ncell(grid))
    coop <- st_as_sf(as.data.frame(xy), coords = c("x", "y"), crs = st_crs(mpsz))
    coop <- st_filter(coop, mpsz)

    res <- gstat(formula = MeanTemp ~ 1,
                 locations = tpdata_sf,
                 nmax = input$n_neighbors,
                 set = list(idp = 0))
    
    resp <- predict(res, coop)
    
    resp$x <- st_coordinates(resp)[,1]
    resp$y <- st_coordinates(resp)[,2]
    resp$pred <- resp$var1.pred
    
    pred <- terra::rasterize(resp, grid, field = "pred", fun = "mean")
    
    tmap_mode("plot")
    tm <- tm_shape(pred) +
      tm_raster(alpha = 0.6, palette = "viridis") +
      tm_layout(main.title = "Isohyet Map with Interpolation") +
      tm_compass(type = "8star", size = 2) +
      tm_scale_bar() +
      tm_grid(alpha = 0.2)
    print(tm)
    
    v <- variogram(MeanTemp ~ 1, data = tpdata_sf)
    
    fv <- fit.variogram(object = v,
                        model = vgm(psill = 0.5, model = input$model_option, range = input$range_param, nugget = 0.1),
                        fit.method = input$fit_method)
  })
  
  
  output$rain_choromap <- renderPlot({
    
    # Plot the outline of mpsz
    outline <- tm_shape(mpsz) +
      tm_borders()
    
    # Plot mpszweather on top of the outline
    rain <- tm_shape(mpszweather) +
      tm_polygons(col = "TotalRainfall", palette = "Blues", style = input$style_param) +
      tm_layout(main.title = "Choropleth Map") 
    
    # Combine the outline and weather map
    rainoutline <- outline + rain
    
    # Display the map
    print(rainoutline)
    tmap_mode("plot")
    tmap_options(check.and.fix = TRUE)

  })
  
  output$rain_geoplot <- renderPlot({
    rfdata_sf <- rain_data %>% 
        dplyr::select(Station, TotalRainfall) %>% 
        left_join(stations, by = "Station") %>% 
        st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
        st_transform(crs = 3414)
    
    grid <- terra::rast(mpsz, nrows = 690, ncols = 1075)
    xy <- terra::xyFromCell(grid, 1:ncell(grid))
    coop <- st_as_sf(as.data.frame(xy), coords = c("x", "y"), crs = st_crs(mpsz))
    coop <- st_filter(coop, mpsz)
    
    res <- gstat(formula = TotalRainfall ~ 1,
                 locations = rfdata_sf,
                 nmax = input$n_neighbors,
                 set = list(idp = 0))
    
    resp <- predict(res, coop)
    
    resp$x <- st_coordinates(resp)[,1]
    resp$y <- st_coordinates(resp)[,2]
    resp$pred <- resp$var1.pred
    
    pred <- terra::rasterize(resp, grid, field = "pred", fun = "mean")
    
    tmap_mode("plot")
    tm <- tm_shape(pred) +
      tm_raster(alpha = 0.6, palette = "viridis") +
      tm_layout(main.title = "Isohyet Map with Interpolation") +
      tm_compass(type = "8star", size = 2) +
      tm_scale_bar() +
      tm_grid(alpha = 0.2)
    print(tm)
    
    v <- variogram(TotalRainfall ~ 1, data = rfdata_sf)
    
    fv <- fit.variogram(object = v,
                        model = vgm(psill = 0.5, model = input$model_option, range = input$range_param, nugget = 0.1),
                        fit.method = input$fit_method)
  })
  
  #### Correlation ####
  
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
  
  #### CDA - Temperature by Station ####
  
  plot_station_temp <- function(measurement, 
                                selected_stations = c("Admiralty", "Ang Mo Kio", "Boon Lay (East)"), 
                                metric = "Avg_Mean_Temp", 
                                p_type = "boxviolin", 
                                t_type = "nonparametric", 
                                pair_disp = "significant", 
                                conf = 0.95,
                                font_size = 5) {
    
    if (measurement == "Monthly") {
      data <- Temp_Monthly %>%
        filter(Station %in% selected_stations)
    } else {data <- Temp_Annual %>%
      filter(Station %in% selected_stations)
    }
    
    if (p_type == "Box") {
      v_width <- 0
      b_width <- 0.3
    } else if (p_type == "Violin") {
      v_width <- 0.5
      b_width <- 0
    } else {
      v_width <- 0.5
      b_width <- 0.3
    }
    
    output$station_temp <- renderPlot({
      
      ylab <- switch(input$s_temp_metric,
                     "Avg_Mean_Temp" = "Average of Mean Temperature",
                     "Avg_Max_Temp" = "Average of Max Temperature",
                     "Avg_Min_Temp" = "Average of Minimum Temperature",
                     "Max_Temp" = "Maximum Temperature",
                     "Min_Temp" = "Minimum Temperature")
      
      if (input$s_temp_measurement == "Monthly") {
        title <- paste("Monthly", ylab, "by Selected Stations")
      } else {
        title <- paste("Annual", ylab, "by Selected Stations")
      }
      
      ggbetweenstats(
        data = data,
        x = Station,
        y = !!sym(metric),
        type = t_type,
        pairwise.display = pair_disp,
        point.args = list(position = position_jitterdodge(dodge.width = 0.6), 
                          alpha = 0.4, size = 5, stroke = 0, na.rm = TRUE),
        boxplot.args = list(width = b_width, alpha = 0.2),
        violin.args = list(width = v_width, alpha = 0.2),
        ggsignif.args = list(textsize = font_size, tip_length = 0.01),
        xlab = "Station",
        ylab = ylab,
        title = title) +
        theme_classic() +
        theme(axis.text = element_text(size = 14),
              axis.title.x = element_text(size = 18,
                                          vjust = -0.3),
              axis.title.y = element_text(size = 18,
                                          vjust = +1),
              plot.title = element_text(size = 25), 
              legend.position="none")
    })
  }
  
  observeEvent(input$show_station_temp, {
    plot_station_temp(input$s_temp_measurement, 
                      input$s_temp_station,
                      input$s_temp_metric,
                      input$s_temp_plot_type,
                      input$s_temp_test_type,
                      input$s_temp_pair_display,
                      input$s_temp_conf_inv,
                      input$s_temp_psize)
  })
  
  #### CDA - Rainfall by Station ####
  
  plot_station_rf <- function(measurement, 
                              selected_stations = c("Admiralty", "Ang Mo Kio", "Boon Lay (East)"), 
                              metric = "Total_Rf", 
                              p_type = "boxviolin", 
                              t_type = "nonparametric", 
                              pair_disp = "significant", 
                              conf = 0.95,
                              font_size = 5) {
    
    if (measurement == "Monthly") {
      data <- Rainfall_Monthly %>%
        filter(Station %in% selected_stations)
    } else {data <- Rainfall_Annual %>%
      filter(Station %in% selected_stations)
    }
    
    if (p_type == "Box") {
      v_width <- 0
      b_width <- 0.3
    } else if (p_type == "Violin") {
      v_width <- 0.5
      b_width <- 0
    } else {
      v_width <- 0.5
      b_width <- 0.3
    }
    
    output$station_rf <- renderPlot({
      
      ylab <- switch(input$s_rf_metric,
                     "Total_Rf" = "Total Rainfall",
                     "Total_Rf_30" = "Total Rainfall (30 min)",
                     "Total_Rf_60" = "Total Rainfall (60 min)",
                     "Total_Rf_120" = "Total Rainfall (120 min)",
                     "Average of Total Rainfall" = "Avg_Total_Rf",
                     "Avg_Total_Rf30" = "Average of Total Rainfall (30 min)",
                     "Avg_Total_Rf60" = "Average of Total Rainfall (60 min)",
                     "Avg_Total_Rf120" = "Average of Total Rainfall (120 min)",
                     "Min_Total_Rf" = "Minimum of Total Rainfall",
                     "Max_Total_Rf" = "Maximum of Total Rainfall")
      
      if (input$s_rf_measurement == "Monthly") {
        title <- paste("Monthly", ylab, "by Selected Stations")
      } else {
        title <- paste("Annual", ylab, "by Selected Stations")
      }
      
      ggbetweenstats(
        data = data,
        x = Station,
        y = !!sym(metric),
        type = t_type,
        pairwise.display = pair_disp,
        point.args = list(position = position_jitterdodge(dodge.width = 0.6), 
                          alpha = 0.4, size = 5, stroke = 0, na.rm = TRUE),
        boxplot.args = list(width = b_width, alpha = 0.2),
        violin.args = list(width = v_width, alpha = 0.2),
        ggsignif.args = list(textsize = font_size, tip_length = 0.01),
        xlab = "Station",
        ylab = ylab,
        title = title) +
        theme_classic() +
        theme(axis.text = element_text(size = 14),
              axis.title.x = element_text(size = 18,
                                          vjust = -0.3),
              axis.title.y = element_text(size = 18,
                                          vjust = +1),
              plot.title = element_text(size = 25), 
              legend.position="none")
    })
  }
  
  observeEvent(input$show_station_rf, {
    plot_station_rf(input$s_rf_measurement, 
                    input$s_rf_station,
                    input$s_rf_metric,
                    input$s_rf_plot_type,
                    input$s_rf_test_type,
                    input$s_rf_pair_display,
                    input$s_rf_conf_inv,
                    input$r_rf_psize)
  })
  
  #### CDA - Temperature by Region ####
  
  plot_region_temp <- function(measurement, 
                               selected_regions = c("Central", "East", "North", "North-East", "West"), 
                               metric = "Avg_Mean_Temp", 
                               p_type = "boxviolin", 
                               t_type = "nonparametric", 
                               pair_disp = "significant", 
                               conf = 0.95,
                               font_size = 5) {
    
    if (measurement == "Monthly") {
      data <- Temp_Monthly %>%
        filter(Region %in% selected_regions)
    } else {data <- Temp_Annual %>%
      filter(Region %in% selected_regions)
    }
    
    if (p_type == "Box") {
      v_width <- 0
      b_width <- 0.3
    } else if (p_type == "Violin") {
      v_width <- 0.5
      b_width <- 0
    } else {
      v_width <- 0.5
      b_width <- 0.3
    }
    
    output$region_temp <- renderPlot({
      
      ylab <- switch(input$r_temp_metric,
                     "Avg_Mean_Temp" = "Average of Mean Temperature",
                     "Avg_Max_Temp" = "Average of Max Temperature",
                     "Avg_Min_Temp" = "Average of Minimum Temperature",
                     "Max_Temp" = "Maximum Temperature",
                     "Min_Temp" = "Minimum Temperature")
      
      if (input$r_temp_measurement == "Monthly") {
        title <- paste("Monthly", ylab, "by Selected Regions")
      } else {
        title <- paste("Annual", ylab, "by Selected Regions")
      }
      
      ggbetweenstats(
        data = data,
        x = Region,
        y = !!sym(metric),
        type = t_type,
        pairwise.display = pair_disp,
        point.args = list(position = position_jitterdodge(dodge.width = 0.6), 
                          alpha = 0.4, size = 5, stroke = 0, na.rm = TRUE),
        boxplot.args = list(width = b_width, alpha = 0.2),
        violin.args = list(width = v_width, alpha = 0.2),
        ggsignif.args = list(textsize = font_size, tip_length = 0.01),
        xlab = "Region",
        ylab = ylab,
        title = title) +
        theme_classic() +
        theme(axis.text = element_text(size = 14),
              axis.title.x = element_text(size = 18,
                                          vjust = -0.3),
              axis.title.y = element_text(size = 18,
                                          vjust = +1),
              plot.title = element_text(size = 25), 
              legend.position="none")
    })
  }
  
  observeEvent(input$show_region_temp, {
    plot_region_temp(input$r_temp_measurement, 
                     input$r_temp_region,
                     input$r_temp_metric,
                     input$r_temp_plot_type,
                     input$r_temp_test_type,
                     input$r_temp_pair_display,
                     input$r_temp_conf_inv,
                     input$r_temp_psize)
  })
  
  #### CDA - Rainfall by Region ####
  
  plot_region_rf <- function(measurement, 
                             selected_regions = c("Central", "East", "North", "North-East", "West"), 
                             metric = "Total_Rf", 
                             p_type = "boxviolin", 
                             t_type = "nonparametric", 
                             pair_disp = "significant", 
                             conf = 0.95,
                             font_size = 5) {
    
    if (measurement == "Monthly") {
      data <- Rainfall_Monthly %>%
        filter(Region %in% selected_regions)
    } else {data <- Rainfall_Annual %>%
      filter(Region %in% selected_regions)
    }
    
    if (p_type == "Box") {
      v_width <- 0
      b_width <- 0.3
    } else if (p_type == "Violin") {
      v_width <- 0.5
      b_width <- 0
    } else {
      v_width <- 0.5
      b_width <- 0.3
    }
    
    output$region_rf <- renderPlot({
      
      ylab <- switch(input$r_rf_metric,
                     "Total_Rf" = "Total Rainfall",
                     "Total_Rf_30" = "Total Rainfall (30 min)",
                     "Total_Rf_60" = "Total Rainfall (60 min)",
                     "Total_Rf_120" = "Total Rainfall (120 min)",
                     "Average of Total Rainfall" = "Avg_Total_Rf",
                     "Avg_Total_Rf30" = "Average of Total Rainfall (30 min)",
                     "Avg_Total_Rf60" = "Average of Total Rainfall (60 min)",
                     "Avg_Total_Rf120" = "Average of Total Rainfall (120 min)",
                     "Min_Total_Rf" = "Minimum of Total Rainfall",
                     "Max_Total_Rf" = "Maximum of Total Rainfall")
      
      if (input$r_rf_measurement == "Monthly") {
        title <- paste("Monthly", ylab, "by Selected Regions")
      } else {
        title <- paste("Annual", ylab, "by Selected Regions")
      }
      
      ggbetweenstats(
        data = data,
        x = Region,
        y = !!sym(metric),
        plot.type = p_type,
        type = t_type,
        pairwise.display = pair_disp,
        point.args = list(position = position_jitterdodge(dodge.width = 0.6), 
                          alpha = 0.4, size = 5, stroke = 0, na.rm = TRUE),
        boxplot.args = list(width = b_width, alpha = 0.2),
        violin.args = list(width = v_width, alpha = 0.2),
        ggsignif.args = list(textsize = font_size, tip_length = 0.01),
        xlab = "Region",
        ylab = ylab,
        title = title) +
        theme_classic() +
        theme(axis.text = element_text(size = 14),
              axis.title.x = element_text(size = 18,
                                          vjust = -0.3),
              axis.title.y = element_text(size = 18,
                                          vjust = +1),
              plot.title = element_text(size = 25), 
              legend.position="none")
    })
  }
  
  observeEvent(input$show_region_rf, {
    plot_region_rf(input$r_rf_measurement, 
                   input$r_rf_region,
                   input$r_rf_metric,
                   input$r_rf_plot_type,
                   input$r_rf_test_type,
                   input$r_rf_pair_display,
                   input$r_rf_conf_inv,
                   input$r_rf_psize)
  })
  
  #### Forecast Models #####

  outputPlotETS <- reactive ({
   if (input$iETS_Region != "All") {
      data <- data_region_ts %>% 
        filter(Region == input$iETS_Region) %>% 
        as_tsibble(index = (YearMonth))
    } else {
      data <-data_ts
    }

    if (input$iETS_Variable == "Rainfall"){
      data <- data %>% rename(Value = Rainfall)
      displayText = "Rainfall"
      displayUnit = "mm"
    } else {
      if (input$iETS_Variable == "MeanTemp") {
        data <- data %>% rename(Value = AveTemp)
      } else if (input$iETS_Variable == "MaxTemp") {
        data <- data %>% rename(Value = MaxTemp)
      } else {
        data <- data %>% rename(Value = MinTemp)
      }
      displayText = "Temperature"
      displayUnit = "°C"
    }
 
    ets_model <- data %>%
      model(ETS(Value ~ error(input$iETS_Error) + 
                  trend(input$iETS_Trend) + season(input$iETS_Season), 
                opt_crit = input$iETS_OptCrit))
    
    #can get the IC and OC values
    aic <- glance(ets_model)$AIC
    bic <- glance(ets_model)$BIC
    aicc <- glance(ets_model)$AICc
    
    switch(input$iETS_OptCrit,
           "lik" = opt_crit_value <-glance(ets_model)$log_lik,
           "sigma" = opt_crit_value <-glance(ets_model)$sigma2,
           "mae" = opt_crit_value <-glance(ets_model)$MAE,
           "mse" = opt_crit_value <-glance(ets_model)$MSE,
           "amse" = opt_crit_value <-glance(ets_model)$AMSE
             )
    
    forecast_values <- forecast(ets_model, h = input$iETS_Years * 12)  
    
    # quantiles <- forecast_values %>%
    #   group_by(.model) %>%
    #   summarise(lower_80 = quantile(Value, 0.1),
    #             upper_80 = quantile(Value, 0.9),
    #             lower_95 = quantile(Value, 0.025),
    #             upper_95 = quantile(Value, 0.975))
    # 
    # p <- ggplot() +
    #   geom_line(data = data, 
    #             aes(x = Date, y = Value, color = "Observed"), 
    #             linetype = "solid") +
    #   geom_line(data = forecast_values, 
    #             aes(x = as.Date(YearMonth), y = .mean, color = "Forecast"), 
    #             linetype = "dashed") +
    #   geom_point(data = forecast_values, 
    #              aes(x = as.Date(YearMonth), y = .mean, 
    #                  text = paste("YearMonth:", YearMonth, 
    #                               "<br>Forecasted:", round(.mean, 1), displayUnit)),
    #              size = 1, color = "red") +
    #   geom_point(data = data, 
    #              aes(x = Date, y = Value, 
    #                  text = paste("YearMonth:", YearMonth, 
    #                               "<br>Observed:", round(Value, 1), displayUnit)),
    #              size = 1, color = "blue") +
    #   geom_ribbon(data = quantiles, 
    #               aes(x = as.Date(YearMonth), 
    #                   ymin = lower_80, 
    #                   ymax = upper_80), fill = "black", alpha = 0.5) +
    #   geom_ribbon(data = quantiles, 
    #               aes(x = as.Date(YearMonth), 
    #                   ymin = lower_95, 
    #                   ymax = upper_95), fill = "gray", alpha = 0.5) +
    #   xlab("Year") +
    #   ylab(displayText) +
    #   ggtitle(paste("Forecast ", displayText)) +
    #   scale_color_manual(values = c("Observed" = "blue", "Forecast" = "red")) +
    #   theme_minimal()
    # 
    # list(plot = ggplotly(p, tooltip = "text"), 
    #      AIC = aic, BIC = bic, AICc = aicc,
    #      optValue = opt_crit_value)
    
    p <- forecast_values %>% autoplot(data) +
      ylab(displayText) + 
      xlab("Year") +
      theme_minimal()
    
    list(plot = p, AIC = aic, BIC = bic, AICc = aicc,
         optValue = opt_crit_value)

  })
  
  output$plot_ETS <- renderPlot({
    if( input$button_ETS_plot > 0) {
      plot_and_ic <- isolate(outputPlotETS())
      plot_and_ic$plot
    }
  })
  
  output$text_ETS_IC1 <- renderUI({
    if( input$button_ETS_plot > 0) {
      plot_and_ic <- isolate(outputPlotETS())
      ic_value <- paste("AIC:", round(plot_and_ic$AIC,1))
      div <- tags$div(class = "bordered-text", ic_value)
      div
    } 
  })
  
  output$text_ETS_IC2 <- renderUI({
    if( input$button_ETS_plot > 0) {
      plot_and_ic <- isolate(outputPlotETS())
      ic_value <- paste("BIC:", round(plot_and_ic$BIC,1))
      div <- tags$div(class = "bordered-text", ic_value)
      div
    } 
  })
  
  output$text_ETS_IC3 <- renderUI({
    if( input$button_ETS_plot > 0) {
      plot_and_ic <- isolate(outputPlotETS())
      ic_value <- paste("AICc:", round(plot_and_ic$AICc,1))
      div <- tags$div(class = "bordered-text", ic_value)
      div
    } 
  })
  
  output$text_ETS_OptValue <- renderUI({
    if( input$button_ETS_plot > 0) {
      plot_and_ic <- isolate(outputPlotETS())
      ic_value <- paste(input$iETS_OptCrit, ":", round(plot_and_ic$optValue,1))
      div <- tags$div(class = "bordered-text", ic_value)
      div
    } 
  })
  
  
  outputPlotARIMA <- reactive ({
    if (input$iARIMA_Region != "All") {
      data <- data_region_ts %>% 
        filter(Region == input$iARIMA_Region) %>% 
        as_tsibble(index = (YearMonth))
    } else {
      data <-data_ts
    }

    if (input$iARIMA_Variable == "Rainfall"){
      data <- data %>% rename(Value = Rainfall)
      displayText = "Rainfall"
      displayUnit = "mm"
    } else {
      if (input$iARIMA_Variable == "MeanTemp") {
        data <- data %>% rename(Value = AveTemp)
      } else if (input$iARIMA_Variable == "MaxTemp") {
        data <- data %>% rename(Value = MaxTemp)
      } else {
        data <- data %>% rename(Value = MinTemp)
      }
      displayText = "Temperature"
      displayUnit = "°C"
    }
    
    aic <- 0
    ets_model <- data %>%
      model(ARIMA(Value ~ pdq(as.numeric(input$iARIMA_p),
                              as.numeric(input$iARIMA_d),
                              as.numeric(input$iARIMA_q))))

    aic <- glance(ets_model)$AIC
    bic <- glance(ets_model)$BIC
    aicc <- glance(ets_model)$AICc
    
    forecast_values <- forecast(ets_model, h = input$iARIMA_Years * 12)  
    
    quantiles <- forecast_values %>%
      group_by(.model) %>%
      summarise(lower_80 = quantile(Value, 0.1),
                upper_80 = quantile(Value, 0.9),
                lower_95 = quantile(Value, 0.025),
                upper_95 = quantile(Value, 0.975))
    
    # p <- ggplot() +
    #   geom_line(data = data, 
    #             aes(x = Date, y = Value, color = "Observed"), 
    #             linetype = "solid") +
    #   geom_line(data = forecast_values, 
    #             aes(x = as.Date(YearMonth), y = .mean, color = "Forecast"), 
    #             linetype = "dashed") +
    #   geom_point(data = forecast_values, 
    #              aes(x = as.Date(YearMonth), y = .mean, 
    #                  text = paste("YearMonth:", YearMonth, 
    #                               "<br>Forecasted:", round(.mean, 1), displayUnit)),
    #              size = 1, color = "red") +
    #   geom_point(data = data, 
    #              aes(x = Date, y = Value, 
    #                  text = paste("YearMonth:", YearMonth, 
    #                               "<br>Observed:", round(Value, 1), displayUnit)),
    #              size = 1, color = "blue") +
    #   geom_ribbon(data = quantiles, 
    #               aes(x = as.Date(YearMonth), 
    #                   ymin = lower_80, 
    #                   ymax = upper_80), fill = "black", alpha = 0.5) +
    #   geom_ribbon(data = quantiles, 
    #               aes(x = as.Date(YearMonth), 
    #                   ymin = lower_95, 
    #                   ymax = upper_95), fill = "gray", alpha = 0.5) +
    #   xlab("Year") +
    #   ylab(displayText) +
    #   ggtitle(paste("Forecast ", displayText)) +
    #   scale_color_manual(values = c("Observed" = "blue", "Forecast" = "red")) +
    #   theme_minimal()
    # 
    #list(plot = ggplotly(p, tooltip = "text"), AIC = aic, BIC = bic, AICc = aicc)
    
    p <- forecast_values %>% autoplot(data) + 
      ylab(displayText) + 
      xlab("Year") +
      theme_minimal()
      
    list(plot = p, AIC = aic, BIC = bic, AICc = aicc)
  })
  
  output$plot_ARIMA <- renderPlot({
    if( input$button_ARIMA_plot > 0) {
      plot_and_ic <- isolate(outputPlotARIMA())
      plot_and_ic$plot        
    }
  })
  
  output$text_ARIMA_IC1 <- renderUI({
    if( input$button_ARIMA_plot > 0) {
      plot_and_ic <- isolate(outputPlotARIMA())
      ic_value <- paste("AIC:", round(plot_and_ic$AIC,1))
      tags$div(class = "bordered-text", ic_value)
    } 
  })
  
  output$text_ARIMA_IC2 <- renderUI({
    if( input$button_ARIMA_plot > 0) {
      plot_and_ic <- isolate(outputPlotARIMA())
      ic_value <- paste("BIC:", round(plot_and_ic$BIC,1))
      tags$div(class = "bordered-text", ic_value)
    }
  })
  
  output$text_ARIMA_IC3 <- renderUI({
    if( input$button_ARIMA_plot > 0) {
      plot_and_ic <- isolate(outputPlotARIMA())
      ic_value <- paste("AICc:", round(plot_and_ic$AICc,1))
      tags$div(class = "bordered-text", ic_value)
     } 
  })
  
  #### Pre-forecast checks ####

  outputTextStationary <- reactive ({

    if (input$iCheck_STest == "ADF") {
      if (input$iCheck_SVariable == "Rainfall"){
        result <- adf.test(data_ts$Rainfall)
      } else if (input$iCheck_SVariable == "MeanTemp") {
        result <- adf.test(data_ts$AveMean)
      } else if (input$iCheck_SVariable == "MaxTemp") {
        result <- adf.test(data_ts$MaxTemp)
      } else {
        result <- adf.test(data_ts$MinTemp)
      }
      resultText <- paste("<b>Conducting ", input$iCheck_STest, " test</b>",
                          "<br>Null hypothesis: The time series is not stationary",
                          "<br>Alternative hypothesis: The time series is stationary",
                          "<br>",
                          "<br><b>Result</b>",
                          "<br>Statistic:", round(result$statistic,3),
                          "<br>p-value:", result$p.value,
                          "<br>significant level:", input$iCheck_SAlpha,
                          "<br>Conclusion:", ifelse(result$p.value < input$iCheck_SAlpha, 
                                                "Reject null hypothesis (stationary)", 
                                                "Fail to reject null hypothesis (non-stationary)"))
      
    } else {
      if (input$iCheck_SVariable == "Rainfall"){
        result <- ur.kpss(data_ts$Rainfall)
      } else if (input$iCheck_SVariable == "MeanTemp") {
        result <- ur.kpss(data_ts$AveMean)
      } else if (input$iCheck_SVariable == "MaxTemp") {
        result <- ur.kpss(data_ts$MaxTemp)
      } else {
        result <- ur.kpss(data_ts$MinTemp)
      }
      
      CI <- paste0(as.numeric(input$iCheck_SAlpha) * 100, "pct")
      criticalValue <- result@cval[, CI]
      resultText <- paste("<br></b>Conducting ", input$iCheck_STest, " test</b>",
                          "<br>Null hypothesis: The time series is stationary",
                          "<br>Alternative hypothesis: The time series is not stationary",
                          "<br>",
                          "<br><b>Result:</b>",
                          "<br>Statistic:", round(result@teststat, 3),
                          "<br>lag:", result@lag,
                          "<br>significant level:", input$iCheck_SAlpha,
                          "<br>critical value:", criticalValue,
                          "<br>Conclusion:", ifelse(result@teststat < criticalValue, 
                                                "Reject null hypothesis (Not stationary)", 
                                                "Fail to reject null hypothesis (Stationary)"))
    }
  })
  
  output$text_Stationary <- renderText({
   if (input$button_Stationary_check > 0) {
      isolate(outputTextStationary())
    }
    else{
      print("Please click the button.")
    }
  })
  
  outputPlotDecomposition <- reactive ({
    if (input$iCheck_DPlot == "D"){
      if (input$iCheck_DVariable == "Rainfall"){
        data_ts.ts = ts(data_ts$Rainfall, frequency = 12)
      } else if (input$iCheck_DVariable == "MeanTemp") {
        data_ts.ts = ts(data_ts$AveTemp, frequency = 12)
      } else if (input$iCheck_DVariable == "MaxTemp") {
        data_ts.ts = ts(data_ts$MaxTemp, frequency = 12)
      } else {
        data_ts.ts = ts(data_ts$MinTemp, frequency = 12)
      }
      
      decomposed <- decompose(data_ts.ts)
      autoplot(decomposed) +
        ggtitle(paste("Decomposition of the", input$iCheck_DVariable, "time-series data")) +
        xlab("Year")      
    } 
    else {
      if (input$iCheck_DVariable == "Rainfall"){
        data_ts.ts = data_ts$Rainfall
      } else if (input$iCheck_DVariable == "MeanTemp") {
        data_ts.ts = data_ts$AveTemp
      } else if (input$iCheck_DVariable == "MaxTemp") {
        data_ts.ts = data_ts$MaxTemp
      } else {
        data_ts.ts = data_ts$MinTemp
      }
      ggtsdisplay(difference(data_ts.ts, 12),
                  plot_type='partial', lag = 36) +
        labs(title= paste("Seasonally differenced of", input$iCheck_DVariable), y="")
    }
  })
  
  output$plot_Decomposition <- renderPlot({
    if (input$button_Decomposition_check > 0) {
      isolate(outputPlotDecomposition())
    }
  })
}

