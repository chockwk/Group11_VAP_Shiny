pacman::p_load(tidyverse, ggplot2, plotly, lubridate, ggrepel, reshape2, scales, ggthemes)

weather <- readRDS("data/rds/weather.rds")
# Define UI for application that draws a histogram
fluidPage(
    # Application title
    titlePanel("The Heat is On!"),
    sidebarLayout(
      sidebarPanel("Exploratory Data Analysis"),
      mainPanel("Choropleth maps")
    )
)