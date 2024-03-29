# Load required packages
pacman::p_load(bslib, shiny, shinydashboard, shinyWidgets, plotly, ggstatsplot, DT)

sidebar <- dashboardSidebar(
  width = 100,
  sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", icon = icon("th"), tabName = "widgets", badgeLabel = "new", badgeColor = "green")
      ),
  dashboardBody()
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            h2("Dashboard tab content")
    ),
    
    tabItem(tabName = "widgets",
            h2("Widgets tab content")
    )
  )
)

dashboardPage(
  dashboardHeader(title = "Simple tabs"),
  sidebar,
  body
)

# Define UI for application
fluidPage(
  # Application title
  titlePanel("The Heat is On!"),
  # Theme
  theme = bslib::bs_theme(bootswatch = "morph"),
  # Wallpaper
  setBackgroundImage(
#    src = "https://storage.googleapis.com/pod_public/1300/95971.jpg"
    src = "https://wallpapers.com/images/hd/cute-aesthetic-cloudy-sky-vyyff3zydu6k5b9l.jpg"
  ),
  
  navlistPanel(
    id = "tabset",
    widths = c(2,10),
    
    "Overview",
    tabPanel("Dashboard", 
             # Application title

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
             
             tags$p(HTML("This is a call to engage, to learn, and to contribute. As stewards of our nation's future, we invite you to delve into the data, understand the trends, and join the dialogue. Together, let's navigate the path towards a resilient and sustainable Singapore.")),

             
    ),
    
    tabPanel("Live Weather Forecast", 
             titlePanel("Live Weather Forecast"),
             mainPanel(
               
               # Inform Time and Forecast Validity
               textOutput('closestTimestamp'),
               textOutput('forecastValid'),
               
               # Weather Forecast
               DT::dataTableOutput("weatherTable")
             )
    ),
    
    "Exploratory Data Analysis",
    tabPanel("Time Series Analysis", 
             sidebarLayout(
               sidebarPanel(
                 selectInput("selected_years", "Select Years:", 
                             choices = 1990:2023, 
                             multiple = TRUE),
                 actionButton("showPlotButton", "Plot")
               ),
               mainPanel(
                 navset_card_underline(
                   nav_panel("Temperature", plotOutput("temp_cycle_plot")),
                   nav_panel("Rainfall", plotOutput("rain_cycle_plot"))
                 )
               )
             )
    ),
    
    tabPanel("Geospatial Analysis",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 selectInput("analysis_variable", "Variable:",
                             choices = c("Temperature", "Rainfall")
                 ),
                 sliderInput("n_neighbors", "Number of Neighbors:",
                             min = 0, max = 20, value = 5
                 ),
                 selectInput("model_option", "Model Options:",
                             choices = c("Sph", "Exp", "Gau", "Lin")
                 ),
                 sliderInput("range_param", "Range:",
                             min = 1000, max = 5000, step = 1000, value = 5000
                 ),
                 selectInput("fit_method", "Fit Methods:",
                             choices = c(1, 2, 3, 4),
                             selected = 1
                 ),
               actionButton("GS_Button", "Plot")
             ),
             mainPanel(
               width = 9,
               plotOutput("geo_plot")
             )
           )
    ),
    
    tabPanel("Correlation", 
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 selectInput("variable", "Choose a grouping variable:",
                             choices = c("Station", "Region")
                 ),
                 selectInput("method", "Smoothing Method:", 
                             choices = c("auto", "lm", "glm", "gam", "loess")
                 ),
                 selectInput("association_type", "Association Type:", 
                             choices = c("parametric" = "p", "nonparametric" = "np", "robust" = "r")
                 ),
                 selectInput("marginal_type", "Marginal Distribution Type:", 
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
    tabPanel("Time Series Forecasting", 
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 selectInput(inputId = "variable",
                             label = "Select variable to forecast",
                             choices = c("Temperature", "Rainfall"),
                             selected = "Temperature"
                 ),
                 selectInput(inputId = "model",
                             label = "Select a model",
                             choices = c("ARIMA", "Holt-Winters", "Seasonal & Trend Decomposition"),
                             selected = "ARIMA"
                 ),
                 selectInput(inputId = "region",
                             label = "Select a region",
                             choices = c("All", "North", "North-East", "Central", "East", "West"),
                             selected = "All"
                 ),
                 sliderInput(inputId = "years",
                             label = "Years to forecast",
                             min = 5,
                             max = 20,
                             value = 10
                 ),
                 radioButtons(inputId = "confidence",
                              label = "Select the confidence level (%):",
                              choices = c("90", "95", "99"),
                              selected = "90"
                 ),
                 actionButton("Forecast_Button", "Forecast")
               ),
               mainPanel(
                 width = 9,
                 plotlyOutput("plotForecast")
               )
             )
          )
    )

)

