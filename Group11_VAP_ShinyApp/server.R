pacman::p_load(shiny, tidyverse, ggrepel, DT, plotly, forecast, stats, zoo, shinyjs, ggstatsplot, gganimate, ggthemes,
               sf, tmap, terra, viridis, sp, raster, gstat, automap, ggrepel)

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
  
  # Dashboard Animation
  
  output$anim_plot <- renderUI({
    # Render the animation and save it as a gif
    anim_file <- tempfile(fileext = ".gif")
    p <- ggplot(temp_time, aes(x = Month, y = MeanTemp)) +
      geom_point(aes(color = MeanTemp), alpha = 0.5, size = 4, show.legend = FALSE) +
      scale_color_gradient(low = "darkorange", high = "darkred") +
      geom_boxplot(aes(y = MeanTemp_Year), width = 0.8, color = "darkgoldenrod1") +
      scale_size(range = c(2, 12)) +
      labs(title = 'Mean Temperature, 1986-2023 \nYear: {frame_time}', 
           x = 'Month', 
           y = 'Mean Temperature (°C)') +
      transition_time(as.integer(Year)) + 
      ease_aes('linear') +
      theme(legend.position = "right",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      guides(color = guide_legend(title = "Average Temperature", override.aes = list(color = "grey", linetype = "dashed"))) +
      theme_hc()
    
    anim_save("animation.gif", animation = p)
    
    # Serve the gif file
    tags$img(src = anim_file, style = "width: 600px; height: 400px;") 
  })
  
  output$animation <- renderImage({
    # Return a list containing the image file path and content type
    list(src = "animation.gif", contentType = "image/gif")
  }, deleteFile = FALSE) # Set to TRUE if the file is temporary
  
  # Live Forecast
  
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
  
  output$closestTimestamp <- renderText({
    weather_data <- getWeatherData()
    if (is.null(weather_data)) {
      return("No data available")
    }
    paste("Closest timestamp:", weather_data$closest_timestamp)
  })
  
  output$forecastValid <- renderText({
    weather_data <- getWeatherData()
    if (is.null(weather_data)) {
      return("No data available")
    }
    paste("Forecast valid to:", weather_data$forecast_valid)
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
  
  # CDA - Temperature by Station
  
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
  
  # CDA - Rainfall by Station
  
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
  
  # CDA - Temperature by Region
  
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
  
  # CDA - Rainfall by Region
  
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
  
  # Forecast
  
  forecastPlotReady <- reactiveValues(ok = FALSE)

  observeEvent(input$Forecast_Button, {
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

