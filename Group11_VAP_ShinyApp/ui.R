# Load required packages
pacman::p_load(bslib, shiny, shinydashboard, shinyWidgets, plotly, ggstatsplot)

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
    widths = c(2,6),
    
    "Overview",
    tabPanel("Introduction", "Panel one contents"),
    tabPanel("Dashboard", "Panel two contents"),
    
    "Exploratory Data Analysis",
    tabPanel("Time Series Analysis", 
             "Panel 6 contents"
             
             
    ),
    
    tabPanel("Geospatial Analysis",
             sidebarLayout(
               sidebarPanel(
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
               actionButton("showPlotButton", "Plot")
             ),
             mainPanel(
               plotOutput("geo_plot")
             )
           )
    ),
    
    tabPanel("Correlation", 
             sidebarLayout(
               sidebarPanel(
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
                 actionButton("showPlotButton", "Plot")
               ),
               mainPanel(
                 plotOutput("correlationPlot")
               )
             )
    ),
                

    "Confirmatory Data Analysis",
    tabPanel("By Station",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("station", "Select 3 Stations", c("Admiralty",
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
                                options = list(maxItems = 3)),
                 selectInput("measurement", "Measurement", c("Monthly", "Annual"), 
                             selected = "Monthly"),
                 selectInput("metric", "Metric", c("Average of Mean Temperature" = "Avg_Mean_Temp",
                                                   "Average of Max Temperature" = "Avg_Max_Temp",
                                                   "Average of Minimum Temperature" = "Avg_Min_Temp",
                                                   "Maximum Temperature" = "Max_Temp",
                                                   "Minimum Temperature" = "Min_Temp"),
                             selected = "Average of Mean Temperature"),
                 selectInput("plot_type", "Plot Type", c("Box Violin" = "boxviolin", 
                                                         "Box" = "box", 
                                                         "Violin" = "violin"), 
                             selected = "Box Violin"),
                 selectInput("test_type", "Test Type", c("Non-parametric" = "nonparametric", 
                                                         "Parametric" = "parametric", 
                                                         "Robust" = "robust", 
                                                         "Bayes" = "bayes"),
                             selected = "Non-parametric"),
                 selectInput("pair_display", "Pair Display", c("Significant" = "significant",
                                                               "Non-Significant" = "non-significant",
                                                               "Everything" = "everything",
                                                               "All" = "all"),
                             selected = "Significant"),
                 radioButtons("conf_inv", "Confidnence Interval", c("95%" = "0.95",
                                                                    "99%" = "0.99"),
                              selected = "95%"),
                 actionButton("showPlotButton", "Plot")
               ),  #sideabrpanel
               
               # After sidebarPanel
               # Main panel for displaying outputs
               mainPanel(
                 width = 8,
                 fluid = FALSE,
                 navset_card_underline(
                   # Panel for Temperature
                   nav_panel("Temperature", plotOutput("temp_station")),
                 
                   # Panel for Rainfall
                   nav_panel("Rainfall", plotOutput("rf_station"))
                   ) # navset_card_underline
                 ) #mainpanel
             ) # sidebarLayout
    ), # tabpanel
    
    tabPanel("By Region", 
             sidebarLayout(
               sidebarPanel(
                 selectInput("measurement", "Measurement", c("Monthly", "Annual"), 
                             selected = "Monthly"),
                 selectInput("metric", "Metric", c("Average of Mean Temperature" = "Avg_Mean_Temp",
                                                   "Average of Max Temperature" = "Avg_Max_Temp",
                                                   "Average of Minimum Temperature" = "Avg_Min_Temp",
                                                   "Maximum Temperature" = "Max_Temp",
                                                   "Minimum Temperature" = "Min_Temp"),
                             selected = "Average of Mean Temperature"),
                 selectInput("plot_type", "Plot Type", c("Box Violin" = "boxviolin", 
                                                         "Box" = "box", 
                                                         "Violin" = "violin"), 
                             selected = "Box Violin"),
                 selectInput("test_type", "Test Type", c("Non-parametric" = "nonparametric", 
                                                         "Parametric" = "parametric", 
                                                         "Robust" = "robust", 
                                                         "Bayes" = "bayes"),
                             selected = "Non-parametric"),
                 selectInput("pair_display", "Pair Display", c("Significant" = "significant",
                                                               "Non-Significant" = "non-significant",
                                                               "Everything" = "everything",
                                                               "All" = "all"),
                             selected = "Significant"),
                 radioButtons("conf_inv", "Confidnence Interval", c("95%" = "0.95",
                                                                    "99%" = "0.99"),
                              selected = "95%"),
                 actionButton("showPlotButton", "Plot")
               ), #sidebarPanel
               
               # Main panel for displaying outputs
               mainPanel(
                 navset_card_underline(
                   # Panel for Temperature
                   nav_panel("Temperature", plotOutput("temp_station")),
                   # Panel for Rainfall
                   nav_panel("Rainfall", plotOutput("rf_station"))
                   ) # navset_card_underline
                 ) # mainpanel
             ) # sidebarLayout
    ), # tabpanel

    "Forecasting",
    tabPanel("Time Series Forecasting", 
             sidebarLayout(
               sidebarPanel(
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
                 actionButton("showPlotButton", "Forecast")
               ),
               mainPanel(
                 plotlyOutput("plotForecast")
               )
             )
          )
    )

)

