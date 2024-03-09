# Load required packages
pacman::p_load(bslib, shiny, shinydashboard)

sidebar <- dashboardSidebar(
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
  
  navlistPanel(
    id = "tabset",
    "Overview",
    tabPanel("Introduction", "Panel one contents"),
    tabPanel("Dashboard", "Panel two contents"),
    "Data Preparation",
    tabPanel("Data Extraction", "Panel three contents"),
    tabPanel("Data Cleaning", "Panel four contents"),
    tabPanel("Data Health", "Panel five contents"),
    "Exploratory Data Analysis",
    tabPanel("Time Series Analysis", "Panel 6 contents"),
    tabPanel("Geospatial Analysis", "Panel 7 contents"),
    "Confirmatory Data Analysis",
    tabPanel("Distribution", "Panel 8 contents"),
    tabPanel("Statistical Testing", "Panel 9 contents"),
    "Forecasting",
    tabPanel("Next 10 Years", "Panel 10 contents"),
    tabPanel("Next 20 Years", "Panel 11 contents")
  )
  
)

