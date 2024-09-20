library(tidyverse)
library(tidymodels)
library(dplyr)
library(vroom)
library(DataExplorer)
library(GGally)
library(skimr)
library(patchwork)
library(corrplot)  
library(ggfortify)
library(ggplot2)

#Read in Data----------------------------------------------------------
trainData <- vroom("train.csv")
testData <- vroom("test.csv")
summary(trainData)


#Workflow Function----------------------------------------------------------
predict_export <- function(workFlowName, fileName) {
  
  x <- predict(workFlowName, new_data=testData) %>% 
    bind_cols(., testData) %>% 
    select(datetime, .pred) %>% 
    rename(count=.pred) %>% #
    mutate(count=pmax(0, count)) %>% 
    mutate(datetime=as.character(format(datetime))) 
  
  vroom_write(x, file=fileName, delim=",")
}

#Data Modeling----------------------------------------------------------
##Discrete Data
  plot_bar(bikesharedata)

##Continuous Data
plot_histogram(bikesharedata)

##Barplot of Weather
weather.plot <- ggplot(data = bikesharedata, aes(x = weather), fill = ) +
  geom_bar(mapping = aes(x = weather),
           fill = c('aliceblue', 'slategray1', 'dodgerblue3'),
           color = c('azure3', 'slategray3', 'deepskyblue4')) +
  labs(x = "Weather",
       y = "Count",
       title = "Count of Weather") +
  theme_minimal()


##Barplot of Weather in each Season
weather.season.plot <- ggplot(data = bikesharedata, aes(x = season, fill = weather)) +
  geom_bar(color = "black", size = 0.3) +
  labs(
    x = "Weather",
    y = "Count",
    title = "Weather in Each Season",
    fill = "Season") +
  scale_fill_manual(values = c(
    "Clear/Partly Cloudy" = "aliceblue",
    "Misty/Cloudy" = "slategray2",
    "Snow/Rain" = "dodgerblue3")) +
  theme_minimal()

##Density plot of Temperature in each Season
temp.season.plot <- ggplot(data = bikesharedata, aes(x = temp, fill = season)) +
  geom_density(adjust = 2,
               alpha = 0.5) +
  scale_fill_manual(values = c("orangered", "hotpink", "aquamarine", "dodgerblue")) +
  theme_minimal() +
  labs(
    x = "Temperature",
    y = "Density",
    fill = "Season",
    title = "Density of Temperature per Season"
  )


##Weather and Wind speed
weather.wind.plot <- ggplot(data = bikesharedata, aes( x = windspeed, y = count, color = weather))+
  labs(
    x = "Windspeed",
    y = "Mass",
    color = "Weather",
    title = "Wind Speed and Weather"
  ) +
  geom_point() +
  scale_color_manual(values = c("lightblue1", "dodgerblue", "dodgerblue4"))+
  theme_minimal()

##Show all Plots together
(weather.plot + weather.season.plot) / (temp.season.plot + weather.wind.plot)


