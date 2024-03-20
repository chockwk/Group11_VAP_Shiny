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
    
    "Overview",
    tabPanel("Introduction", "Panel one contents"),
    tabPanel("Dashboard", "Panel two contents"),
    
    "Exploratory Data Analysis",
    tabPanel("Time Series Analysis", "Panel 6 contents"),
    
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
    tabPanel("Distribution", "Panel 8 contents"),
    tabPanel("Statistical Testing", "Panel 9 contents"),

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

