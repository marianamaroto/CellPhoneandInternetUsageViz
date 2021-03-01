# Mariana Maroto
# Data Visualization Class Spring 2021
# Homework 1

# load libraries
library(ggplot2)
library(xts)
library(dygraphs)
library(plotly)
library(reshape2)

# read dataset
data <- read.csv('cell_phones_per_100_people.csv', stringsAsFactors = FALSE)

# rearrange dataset
data <- melt(data, id=c("country"))
data$year <- gsub("X","", data$variable)
data$variable <- NULL
data$year <- as.Date(data$year, format = "%Y")

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  titlePanel("Phone Adoption by Country"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(inputId = "country",
                  label = "Select Country:",choices = unique(data$country), selected = 'Costa Rica'),
      h5('Horizontal Blue Line shows year in which there is more phones than people in selected Country.'),
      h6('Data from: https://www.gapminder.org/data/. Please visit link for more info.')
    ),
    
    mainPanel(
      dygraphOutput(outputId = "timeseries")
      
    )
  )
)


# Define server 
server <- function(input, output) {
  
  output$timeseries <- renderDygraph({
    
    # select country
    plot_data <- data[ which(data$country == input$country),]
    
    # create time series object
    phones_timeSeries <- xts(x = plot_data$value,
                             order.by = plot_data$year)
    
    # create a basic interactive element
    interact_time <- dygraph(phones_timeSeries, xlab = 'Year', ylab = 'Cell Phones per 100 People', main = paste('Phone Adoption in ',input$country)) %>%
      dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#a3d98f") %>%
      dyRangeSelector() %>%
      dyCrosshair(direction = "vertical") %>%
      dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE) %>% 
      dyLimit(100, '100 Phones per 100 People', strokePattern = "solid", color = "blue")
    interact_time
    
  })
  
}

shinyApp(ui, server)