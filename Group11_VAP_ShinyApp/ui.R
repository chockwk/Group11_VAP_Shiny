# Load required packages
pacman::p_load(bslib, shiny, shinydashboard, shinyWidgets, plotly)

# Define UI for application
fluidPage(
  titlePanel("The Heat is On!"),
  theme = bslib::bs_theme(bootswatch = "morph"),
  setBackgroundImage(
#    src = "https://storage.googleapis.com/pod_public/1300/95971.jpg"
    src = "https://wallpapers.com/images/hd/cute-aesthetic-cloudy-sky-vyyff3zydu6k5b9l.jpg"
  ),

  tags$head(
    tags$style(
      HTML("        
          .selectize-dropdown-content {
            font-size: 16px; 
          }
          .selectize-control.single .selectize-input {
            font-size: 16px; 
            padding: 5px; /* Adjust the padding as needed */
            margin-top: 0; /* Adjust the margin-top as needed */
          }
          .selectize-input {
            height: 30px; 
          }
          
          .bordered-text {
            border: 1px solid black;
            padding: 10px;
            background-color: lightblue;
          }
        ")
    )
  ),

  navlistPanel(
    id = "tabset",
    widths = c(3,9),
    "Overview",
    tabPanel("Dashboard", 
             tags$h1("Welcome to Singapore’s Race Against Climate Change"),
             tags$h2("Understanding Our Climate, Shaping Our Future"),
             tags$p(HTML("As the sun rises over the Lion City, we embark on an urgent journey to address a challenge that looms over our nation: climate change. Singapore, our home, our pride, stands at the forefront of a silent battle against the escalating impacts of a warming planet. This platform is born out of a necessity to harness the power of data, to visualize and understand the creeping changes to our environment, and to spearhead proactive strategies for resilience and sustainability.")),
             tags$h3("Why This Matters"),
             tags$p(HTML("Singapore, a vibrant city-state renowned for its lush greenery amidst metropolitan splendor, faces existential threats from the consequences of global warming. Intensified rainfall, and increasing temperatures are not just predictions; they are the imminent realities we seek to comprehend and mitigate through informed action.")),
             tags$h3("Project Objectives"),
             tags$p(HTML("Our mission is multi-faceted, aiming not only to highlight the past and present but also to cast a light on our future:")),
             tags$ul(
               tags$li("Visualize: Translate complex climate data into clear, compelling visualizations, making the science accessible and engaging to all Singaporeans."),
               tags$li("Predict: Harness historical data to model future climate scenarios, equipping everyone with the tools to plan and act with foresight."),
               tags$li("Empower: Create a dynamic public platform for every citizen to interact with, explore, and contribute to our collective understanding of climate trends.")
             ),
             tags$h3("The Reality in Numbers"),
             tags$p(HTML("In the last decade, our mean temperatures have edged upward, with an average increase of 0.25°C, subtly yet significantly altering our city's climate balance. Urban heat has surged, with certain regions experiencing temperatures up to 7°C higher than surrounding areas, a direct challenge to our liveability.")),
             
             # Display the temp_plot
             uiOutput("temp_plot"),
             
             tags$p(HTML("Sea levels around Singapore have risen at a rate of 1.2 to 1.7 mm per year since the 1970s, outpacing the global average and putting our coastal integrity at stake. The repercussions are dire for Singapore as a low-lying state; its vulnerability to the effects of climate change cannot be undermined.")),
             
             # Display the rainfall_plot
             uiOutput("rainfall_plot"),
             
             tags$p(HTML("This is a call to engage, to learn, and to contribute. As stewards of our nation's future, we invite you to delve into the data, understand the trends, and join the dialogue. Together, let's navigate the path towards a resilient and sustainable Singapore."))
    ),
    
    tabPanel("Live Weather Forecast", 
             titlePanel("Live Weather Forecast"),
             mainPanel(
               
               # Inform Time and Forecast Validity
               p("Current Date and Time:"),
               verbatimTextOutput("currentTime"),
               hr(),
               p("Air Temperature:"),
               verbatimTextOutput("airTemperature"),
               hr(),
               p("UV Index:"),
               verbatimTextOutput("uvIndex"),
               hr(),
               p("Current PSI Data:"),
               verbatimTextOutput("psiData"),
               p("Two-Hour Rain Forecast:"),
               DT::dataTableOutput("weatherTable")
             )
    ),
    
    "Exploratory Data Analysis",
    tabPanel("Time Series Analysis", 
             sidebarLayout(
               sidebarPanel(
                 selectInput("selected_years", "Select Years (Up to 7)", 
                             choices = seq(1990, 2023, by = 2), 
                             multiple = TRUE),
                 actionButton("showPlotButton", "Plot")
               ),
               mainPanel(
                 navset_card_underline(
                   nav_panel("Temperature", plotOutput("temp_cycleplot"), plotOutput("temp_horiplot")),
                   nav_panel("Rainfall", plotOutput("rain_cycleplot"), plotOutput("rain_horiplot"))
                 )
               )
             )
    ),
    
    tabPanel("Geospatial Analysis",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 selectInput("style_param", "Style Parameter",
                             choices = c("jenks", "pretty", "bclust", "equal", "fisher", "kmeans", "quantile"),
                             selected = "quantile"
                 ),
                 sliderInput("n_neighbors", "Number of Neighbors",
                             min = 0, max = 20, 
                             value = 5
                 ),
                 selectInput("model_option", "Model Options",
                             choices = c("Sph", "Exp", "Gau", "Lin")
                 ),
                 sliderInput("range_param", "Range",
                             min = 1000, max = 5000, step = 1000, 
                             value = 2000
                 ),
                 selectInput("fit_method", "Fit Methods",
                             choices = c(1, 2, 3, 4),
                             selected = 1
                 ),
               actionButton("GS_Button", "Plot")
             ),
             mainPanel(
               width = 9,
               navset_card_underline(
                 nav_panel("Temperature", plotOutput("temp_choromap"), plotOutput("temp_geoplot")),
                 nav_panel("Rainfall", plotOutput("rain_choromap"), plotOutput("rain_geoplot"))
               )
             )
           )
    ),
    
    tabPanel("Correlation Analysis", 
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 selectInput("variable", "Grouping Variable",
                             choices = c("Station", "Region")
                 ),
                 selectInput("method", "Smoothing Method", 
                             choices = c("auto", "lm", "glm", "gam", "loess")
                 ),
                 selectInput("association_type", "Association Type", 
                             choices = c("parametric" = "p", "nonparametric" = "np", "robust" = "r")
                 ),
                 selectInput("marginal_type", "Marginal Distribution Type", 
                             choices = c("histogram", "boxplot", "density", "violin", "densigram")
                 ),
                 actionButton("CorrPlot_Button", "Plot")
               ),
               mainPanel(
                 width = 9,
                 plotOutput("correlationPlot")
               )
             )
    ),
                

    "Confirmatory Data Analysis",
    tabPanel("By Station",
             # Main panel for displaying outputs
             mainPanel(
               width = 12,
               navset_card_underline(
                 # Panel for Temperature
                 nav_panel("Temperature", 
                           sidebarLayout(
                             sidebarPanel(
                               width = 3,
                               tags$style(type='text/css', ".selectize-dropdown-content {max-height: 800px; }"),
                               selectizeInput("s_temp_station", "Select 5 Stations", c("Admiralty",
                                                                                       "Ang Mo Kio",
                                                                                       "Boon Lay (East)",
                                                                                       "Changi",
                                                                                       "Choa Chu Kang (South)",
                                                                                       "Clementi",
                                                                                       "East Coast Parkway",
                                                                                       "Jurong (West)",
                                                                                       "Khatib",
                                                                                       "Marina Barrage",
                                                                                       "Newton",
                                                                                       "Pasir Panjang",
                                                                                       "Paya Lebar",
                                                                                       "Seletar",
                                                                                       "Sembawang",
                                                                                       "Tai Seng",
                                                                                       "Tengah",
                                                                                       "Tuas South"),
                                              multiple = TRUE,
                                              options = list(maxItems = 5)),
                               
                               selectInput("s_temp_measurement", "Measurement", c("Monthly", "Annual"), 
                                           selected = "Monthly"),
                               
                               selectInput("s_temp_metric", "Metric", c("Average of Mean Temperature" = "Avg_Mean_Temp",
                                                                        "Average of Max Temperature" = "Avg_Max_Temp",
                                                                        "Average of Minimum Temperature" = "Avg_Min_Temp",
                                                                        "Maximum Temperature" = "Max_Temp",
                                                                        "Minimum Temperature" = "Min_Temp"), 
                                           selected = "Average of Mean Temperature"),
                               
                               selectInput("s_temp_plot_type", "Plot Type", c("Box Violin", 
                                                                              "Box", 
                                                                              "Violin"), 
                                           selected = "Box Violin"),
                               
                               selectInput("s_temp_test_type", "Test Type", c("Non-parametric" = "nonparametric", 
                                                                              "Parametric" = "parametric", 
                                                                              "Robust" = "robust", 
                                                                              "Bayes" = "bayes"),
                                           selected = "Non-parametric"),
                               
                               selectInput("s_temp_pair_display", "Pair Display", c("Significant" = "significant",
                                                                                    "Non-Significant" = "non-significant",
                                                                                    "Everything" = "everything",
                                                                                    "All" = "all"),
                                           selected = "Significant"),
                               
                               radioButtons("s_temp_conf_inv", "Confidence Level", c("90%" = 0.9,
                                                                                     "95%" = 0.95,
                                                                                     "99%" = 0.99),
                                            selected = 0.95),
                               
                               sliderInput("s_temp_psize", "Font Size for p-value", 3, 10,
                                           value = 5),
                               
                               actionButton("show_station_temp", "Plot")
                             ),  #sideabrpanel
                             
                             plotOutput("station_temp",
                                        width = "1125px", height = "900px")
                           ) # sidebarlayout
                 ), # nav_panel
                 
                 # Panel for Rainfall
                 nav_panel("Rainfall", 
                           sidebarLayout(
                             sidebarPanel(
                               width = 3,
                               tags$style(type='text/css', ".selectize-dropdown-content {max-height: 800px; }"),
                               selectizeInput("s_rf_station", "Select 5 Stations", c("Admiralty",
                                                                                     "Ang Mo Kio",
                                                                                     "Boon Lay (East)",
                                                                                     "Changi",
                                                                                     "Choa Chu Kang (South)",
                                                                                     "Clementi",
                                                                                     "East Coast Parkway",
                                                                                     "Jurong (West)",
                                                                                     "Khatib",
                                                                                     "Marina Barrage",
                                                                                     "Newton",
                                                                                     "Pasir Panjang",
                                                                                     "Paya Lebar",
                                                                                     "Seletar",
                                                                                     "Sembawang",
                                                                                     "Tai Seng",
                                                                                     "Tengah",
                                                                                     "Tuas South"),
                                              multiple = TRUE,
                                              options = list(maxItems = 5)),
                               
                               selectInput("s_rf_measurement", "Measurement", c("Monthly", "Annual"), 
                                           selected = "Monthly"),
                               
                               selectInput("s_rf_metric", "Metric", c("Total Rainfall" = "Total_Rf", 
                                                                      "Total Rainfall (30 min)" = "Total_Rf_30",
                                                                      "Total Rainfall (60 min)" = "Total_Rf_60",
                                                                      "Total Rainfall (120 min)" = "Total_Rf_120",
                                                                      "Average of Total Rainfall" = "Avg_Total_Rf",
                                                                      "Average of Total Rainfall (30 min)" = "Avg_Total_Rf30",
                                                                      "Average of Total Rainfall (60 min)" = "Avg_Total_Rf60",
                                                                      "Average of Total Rainfall (120 min)" = "Avg_Total_Rf120",
                                                                      "Minimum of Total Rainfall" = "Min_Total_Rf",
                                                                      "Maximum of Total Rainfall" = "Max_Total_Rf"), 
                                           selected = "Total Rainfall"),
                               
                               selectInput("s_rf_plot_type", "Plot Type", c("Box Violin", 
                                                                            "Box", 
                                                                            "Violin"), 
                                           selected = "Box Violin"),
                               
                               selectInput("s_rf_test_type", "Test Type", c("Non-parametric" = "nonparametric", 
                                                                            "Parametric" = "parametric",
                                                                            "Robust" = "robust", 
                                                                            "Bayes" = "bayes"),
                                           selected = "Non-parametric"),
                               
                               selectInput("s_rf_pair_display", "Pair Display", c("Significant" = "significant",
                                                                                  "Non-Significant" = "non-significant",
                                                                                  "Everything" = "everything",
                                                                                  "All" = "all"),
                                           selected = "Significant"),
                               
                               radioButtons("s_rf_conf_inv", "Confidence Level", c("90%" = 0.9,
                                                                                   "95%" = 0.95,
                                                                                   "99%" = 0.99),
                                            selected = 0.95),
                               
                               sliderInput("s_rf_psize", "Font Size for p-value", 3, 10,
                                           value = 5),
                               
                               actionButton("show_station_rf", "Plot")
                               
                             ),  #sideabrpanel
                             
                             plotOutput("station_rf",
                                        width = "1125px", height = "900px")
                           ) # sidebarlayout
                 ) # nav_panel
               ) # navset_card_underline
             ) # mainpanel
    ), # tabpanel
    
    tabPanel("By Region", 
             mainPanel(
               width = 12,
               navset_card_underline(
                 # Panel for Temperature
                 nav_panel("Temperature", 
                           sidebarLayout(
                             sidebarPanel(
                               width = 3,
                               
                               selectizeInput("r_temp_region", "Select Regions", c("Central", "East", "North", "North-East", "West"),
                                              multiple = TRUE,
                                              selected = c("Central", "East", "North", "North-East", "West")),
                               
                               selectInput("r_temp_measurement", "Measurement", c("Monthly", "Annual"), 
                                           selected = "Monthly"),
                               
                               selectInput("r_temp_metric", "Metric", c("Average of Mean Temperature" = "Avg_Mean_Temp",
                                                                        "Average of Max Temperature" = "Avg_Max_Temp",
                                                                        "Average of Minimum Temperature" = "Avg_Min_Temp",
                                                                        "Maximum Temperature" = "Max_Temp",
                                                                        "Minimum Temperature" = "Min_Temp"), 
                                           selected = "Average of Mean Temperature"),
                               
                               selectInput("r_temp_plot_type", "Plot Type", c("Box Violin", 
                                                                              "Box", 
                                                                              "Violin"), 
                                           selected = "Box Violin"),
                               
                               selectInput("r_temp_test_type", "Test Type", c("Non-parametric" = "nonparametric", 
                                                                              "Parametric" = "parametric", 
                                                                              "Robust" = "robust", 
                                                                              "Bayes" = "bayes"),
                                           selected = "Non-parametric"),
                               
                               selectInput("r_temp_pair_display", "Pair Display", c("Significant" = "significant",
                                                                                    "Non-Significant" = "non-significant",
                                                                                    "Everything" = "everything",
                                                                                    "All" = "all"),
                                           selected = "Significant"),
                               
                               radioButtons("r_temp_conf_inv", "Confidence Level", c("90%" = 0.9,
                                                                                     "95%" = 0.95,
                                                                                     "99%" = 0.99),
                                            selected = 0.95),
                               
                               sliderInput("r_temp_psize", "Font Size for p-value", 3, 10,
                                           value = 5),
                               
                               actionButton("show_region_temp", "Plot")
                             ),  #sideabrpanel
                             
                             plotOutput("region_temp",
                                        width = "1125px", height = "900px")
                           ) # sidebarlayout
                 ), # nav_panel
                 
                 # Panel for Rainfall
                 nav_panel("Rainfall", 
                           sidebarLayout(
                             sidebarPanel(
                               width = 3,
                               
                               selectizeInput("r_rf_region", "Select Regions", c("Central", "East", "North", "North-East", "West"),
                                              multiple = TRUE,
                                              selected = c("Central", "East", "North", "North-East", "West")),
                               
                               selectInput("r_rf_measurement", "Measurement", c("Monthly", "Annual"), 
                                           selected = "Monthly"),
                               
                               selectInput("r_rf_metric", "Metric", c("Total Rainfall" = "Total_Rf", 
                                                                      "Total Rainfall (30 min)" = "Total_Rf_30",
                                                                      "Total Rainfall (60 min)" = "Total_Rf_60",
                                                                      "Total Rainfall (120 min)" = "Total_Rf_120",
                                                                      "Average of Total Rainfall" = "Avg_Total_Rf",
                                                                      "Average of Total Rainfall (30 min)" = "Avg_Total_Rf30",
                                                                      "Average of Total Rainfall (60 min)" = "Avg_Total_Rf60",
                                                                      "Average of Total Rainfall (120 min)" = "Avg_Total_Rf120",
                                                                      "Minimum of Total Rainfall" = "Min_Total_Rf",
                                                                      "Maximum of Total Rainfall" = "Max_Total_Rf"), 
                                           selected = "Total Rainfall"),
                               
                               selectInput("r_rf_plot_type", "Plot Type", c("Box Violin", 
                                                                            "Box", 
                                                                            "Violin"), 
                                           selected = "Box Violin"),
                               
                               selectInput("r_rf_test_type", "Test Type", c("Non-parametric" = "nonparametric", 
                                                                            "Parametric" = "parametric",
                                                                            "Robust" = "robust", 
                                                                            "Bayes" = "bayes"),
                                           selected = "Non-parametric"),
                               
                               selectInput("r_rf_pair_display", "Pair Display", c("Significant" = "significant",
                                                                                  "Non-Significant" = "non-significant",
                                                                                  "Everything" = "everything",
                                                                                  "All" = "all"),
                                           selected = "Significant"),
                               
                               radioButtons("r_rf_conf_inv", "confidence Level", c("90%" = 0.9,
                                                                                   "95%" = 0.95,
                                                                                   "99%" = 0.99),
                                            selected = 0.95),
                               
                               sliderInput("r_rf_psize", "Font Size for p-value", 3, 10,
                                           value = 5),
                               
                               actionButton("show_region_rf", "Plot")
                               
                             ),  #sideabrpanel
                             
                             plotOutput("region_rf",
                                        width = "1125px", height = "900px")
                           ) # sidebarlayout
                 ) # nav_panel
               ) # navset_card_underline
             ) # mainpanel
    ), # tabpanel

    "Forecasting",
    tabPanel("Pre-forecast Checks",
             mainPanel(
               width = 12,
               navset_card_underline(
                 nav_panel("Stationary Check",
                           sidebarLayout(
                             sidebarPanel(
                               width = 4,
                               HTML("<h6><b>Please select the parameters</b></h6>"),
                               selectInput("iCheck_SVariable", "Select a variable:",
                                           c("Rainfall" = "Rainfall",
                                             "Mean Temperature" = "MeanTemp",
                                             "Max Temperature" = "MaxTemp",
                                             "Min Temperature" = "MinTemp"),
                                           selected = "Rainfall"),
                               selectInput("iCheck_STest", "Select a test:",
                                           c("ADF" = "ADF",
                                             "KPSS" = "KPSS")),  
                               HTML("<font size = 2>Augmented Dickey-Fuller(ADF)</font>"),
                               HTML("<font size = 2>Kwiatkowski–Phillips–Schmidt–Shin(KPSS)</font>"),
                               radioButtons("iCheck_SAlpha", "Select the significance level:",
                                           c("5%" = 0.05,
                                             "10%" = 0.10)),   
                               actionButton("button_Stationary_check", "Check")
                             ),
                             mainPanel(
                                htmlOutput("text_Stationary")
                             )
                           )
                 ),
                 nav_panel("Decomposition Check",
                           sidebarLayout(
                             sidebarPanel(
                               width = 4,
                               HTML("<h6><b>Please select the parameters</b></h6>"),
                               selectInput("iCheck_DVariable", "Select a variable:",
                                           c("Rainfall" = "Rainfall",
                                             "Mean Temperature" = "MeanTemp",
                                             "Max Temperature" = "MaxTemp",
                                             "Min Temperature" = "MinTemp"),
                                           selected = "Rainfall"),
                               selectInput("iCheck_DPlot", "Select plot:",
                                           c("STL Decomposition" = "D",
                                             "PACF" = "PACF"),
                                           selected = "Decomposition"),
                               actionButton("button_Decomposition_check", "Check")
                             ),
                             mainPanel(
                               plotOutput("plot_Decomposition")
                             )
                           )
                )
               )
             )
    ),
    tabPanel("Forecast Models", 
               mainPanel(
                 width = 12,
                 navset_card_underline(
                   nav_panel("ETS Model",
                             sidebarLayout(
                               sidebarPanel(
                                 width = 4,
                                 HTML("<h6><b>Please select the parameters</b></h6>"),
                                 selectInput("iETS_Variable", "Select a variable:",
                                             c("Rainfall" = "Rainfall",
                                               "Mean Temperature" = "MeanTemp",
                                               "Max Temperature" = "MaxTemp",
                                               "Min Temperature" = "MinTemp"),
                                             selected = "Rainfall"),  
                                 selectInput("iETS_Region", "Select a region:",
                                             c("All" = "All",
                                               "North" = "North",
                                               "North-East" = "North-East",
                                               "Central" = "Central",
                                               "East" = "East",
                                               "West" = "West"),
                                             selected = "All"),
                                 selectInput("iETS_Error", "Select Error Term:",
                                             c("Additive" = "A",
                                               "Multiplicative" = "M"),
                                             selected = "Additive"),
                                 selectInput("iETS_Trend", "Select Trend Term:",
                                             c("None" = "N",
                                               "Additive" = "A",
                                               "Damped variants" = "Ad"),
                                             selected = "None"),                                
                                 selectInput("iETS_Season", "Select Season Term:",
                                             c("None" = "N",
                                               "Additive" = "A",
                                               "Multiplicative" = "M"),
                                             selected = "None"),
                                 selectInput("iETS_OptCrit", "Select Optimise Criterion:",
                                             c("log-likelihood" = "lik",
                                               "sigma of residuals" = "sigma",
                                               "MeanAbsoluteError" = "mae",
                                               "MeanSquareError" = "mse",
                                               "Ave MSE" = "amse"),
                                             selected = "sigma"),
                                 sliderInput("iETS_Months", "Number of months to forecast:", 
                                             3, 12, value = 5),
                                 
                                 actionButton("button_ETS_plot", "Forecast")
                               ),
                               mainPanel(

                                 fluidRow(
                                   HTML("<b>Instructions: </b><br>Please use the Pre-forecast Checks tab."),
                                   HTML("<br>You can examine whether the variable time series selected is stationary or not."),
                                   HTML("<br>You can also check its ACF plot of residuals."),
                                   HTML("<br>It takes some time to plot the graph. Your patience is appreciated."),                                  
                                   column(4, uiOutput("text_ETS_IC1")),
                                   column(4, uiOutput("text_ETS_IC2")),
                                   column(4, uiOutput("text_ETS_IC3"))
                                 ),
                                 fluidRow(column(4, uiOutput("text_ETS_OptValue"))),
                                 plotOutput("plot_ETS")
                               )
                             )
                   ),
                   nav_panel("ARIMA Model",
                             sidebarLayout(
                               sidebarPanel(
                                 width = 4,
                                 HTML("<h6><b>Please select the parameters</b></h6>"),
                                 selectInput("iARIMA_Variable", "Select a variable:",
                                             c("Rainfall" = "Rainfall",
                                               "Mean Temperature" = "MeanTemp",
                                               "Max Temperature" = "MaxTemp",
                                               "Min Temperature" = "MinTemp"),
                                             selected = "Rainfall"),  
                                 selectInput("iARIMA_Region", "Select a region:",
                                             c("All" = "All",
                                               "North" = "North",
                                               "North-East" = "North-East",
                                               "Central" = "Central",
                                               "East" = "East",
                                               "West" = "West"),
                                             selected = "All"),
                                 HTML("<font size = 3><b>Seasonal term (pdq)</b></font>"),
                                 selectInput("iARIMA_p", "Select p:",
                                             c("1" = 1,
                                               "2" = 2,
                                               "3" = 3),
                                             selected = "1"),
                                 selectInput("iARIMA_d", "Select d:",
                                             c("0" = 0,
                                               "1" = 1,
                                               "2" = 2),
                                             selected = "0"), 
                                 selectInput("iARIMA_q", "Select q:",
                                             c("0" = 0,
                                               "1" = 1,
                                               "2" = 2),
                                             selected = "0"), 
                                 sliderInput("iARIMA_Months", "Number of months to forecast:", 
                                             3, 12, value = 5),
                                 
                                 actionButton("button_ARIMA_plot", "Forecast")
                               ),
                               mainPanel(
                                 HTML("<b>Instructions: </b><br>Please use the Pre-forecast Checks tab."),
                                 HTML("<br>You can examine whether the variable time series selected is stationary or not."),
                                 HTML("<br>You can also check its ACF plot of residuals."),
                                 HTML("<br>It takes some time to plot the graph. Your patience is appreciated."),                                  
                                 HTML("<br>If it shows 'No model', means the seasonal combination produces no model."),                                  
                                 fluidRow(
                                   column(4, uiOutput("text_ARIMA_IC1")),
                                   column(4, uiOutput("text_ARIMA_IC2")),
                                   column(4, uiOutput("text_ARIMA_IC3"))
                                 ),
                                 plotOutput("plot_ARIMA")
                                 )
                             )
                   )
                 )
               )
    )
  )
)


