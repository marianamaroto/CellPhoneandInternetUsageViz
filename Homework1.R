# Mariana Maroto
# Data Visualization Class Spring 2021
# Homework 1

# load libraries
library(ggplot2)
library(xts)
library(dygraphs)
library(plotly)
library(reshape2)
library(circlize)
library(gganimate)
library(hrbrthemes)
library(dplyr)
library(tidyverse)


#####################################################################
#####################################################################
# Part 1: Phone Adoption Data Cleaning 

# read dataset
data <- read.csv('cell_phones_per_100_people.csv', stringsAsFactors = FALSE)

# rearrange dataset
data <- melt(data, id=c("country"))
data$year <- gsub("X","", data$variable)
data$variable <- NULL
data$year <- as.Date(data$year, format = "%Y")

#################################################################
# Plot 1: Interactive

# select country
plot_data <- data[ which(data$country =='Costa Rica'),]

# create time series object
phones_timeSeries <- xts(x = plot_data$value,
                            order.by = plot_data$year)

# create a basic interactive element
interact_time <- dygraph(phones_timeSeries, xlab = 'Year', ylab = 'Cell Phones per 100 People', main = 'Phone Adoption in Costa Rica') %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#a3d98f") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)
interact_time

#################################################################
# Plot 2: Animated

# Keep only 3 names
plot2_data <- data %>% 
  filter(country %in% c("United States", "Costa Rica", "France", 'China')) 

# Plot
plot2_data %>%
  ggplot( aes(x=year, y=value, group=country, color=country)) +
  geom_line() +
  geom_point() +
  ggtitle("Phone Adoption") +
  theme_ipsum() +
  ylab("Cell Phones per 100 People") +
  transition_reveal(year)

# Save at gif:
anim_save("Phone Adoption.gif")

#####################################################################
####################################################################
# Part 2: Internet Usage

# read dataset
dt <- read.csv('share-of-individuals-using-the-internet.csv', stringsAsFactors = FALSE)
groups <- read.csv('country_group.csv', stringsAsFactors = FALSE)
data <- inner_join(dt, groups, by = "Entity")

# keep only max year
require(data.table)
data <- as.data.table(data)
data <- data[data[, .I[Year == max(Year)], by=Entity]$V1]

# rename columns
colnames(data) <- c('Country','Code','Year','InternetUsageShare','Region')

# Select 4 smallest Regions to Visualize Better
data <- data[data$Region %in% c('North America','Arab States','South/Latin America','Middle east'),]

# Set a number of 'empty bar' to add at the end of each Region
empty_bar <- 3
to_add <- data.frame( matrix(NA, empty_bar*length(unique(data$Region)), ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$Region <- rep(unique(data$Region), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(Region)
data$id <- seq(1, nrow(data))

# Get the name and the y position of each label
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- data %>% 
  group_by(Region) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

# Make the plot
p <- ggplot(data, aes(x=as.factor(id), y=value, fill=Region)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  geom_bar(aes(x=as.factor(id), y=InternetUsageShare, fill=Region), stat="identity", alpha=0.5) +
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(data$id),4), y = c(20, 40, 60, 80), label = c("20", "40", "60", "80") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  geom_bar(aes(x=as.factor(id), y=InternetUsageShare, fill=Region), stat="identity", alpha=0.5) +
  ylim(-100,120) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=InternetUsageShare+0.1, label=Country, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -0.5, xend = end, yend = -0.5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -0.18, label=Region), hjust=c(1,1,0,0), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)

p + labs(title = 'Share of Population Connected to the Internet') +
  theme(plot.title = element_text(lineheight = 0.9))

