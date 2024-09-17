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

#Read in Data
trainData <- vroom("train.csv")
testData <- vroom("test.csv")
summary(trainData)
-----------------------------------------------
  #Cleaning
  ##Remove the casual and registered variables
  ##Change count to log(count) 
  
  trainData <- trainData %>%
  select(-c(casual, registered)) %>% 
  mutate(count = log(count))

summary(trainData)
-----------------------------------------------
  #Feature Engineering
  ##Recode weather 4 to a 3 and make it into a factor
  ##Extract the hour variable from the timestamp
  ##Make season a factor
  ##1 other step of choice
bikerecipe <- recipe(formula = count~., data = trainData) %>%
  step_mutate(weather = ifelse(weather == 4, 3, weather)) %>%
  step_mutate(weather = factor(weather, levels = c(1,2,3), labels = c("Clear", "Misty", "Rain"))) %>% 
  step_time(datetime, features = c("hour")) %>%
  step_rename(time_of_day = datetime_hour) %>%
  step_mutate(season=factor(season, levels = c(1,2,3,4), labels = c("Spring", "Summer", "Fall", "Winter"))) %>%
  step_date(datetime, features = "dow") %>%
  step_rename(day_of_week = datetime_dow) %>% 
  step_rm(datetime)


bike_data_prepped <- prep(bikerecipe)

juice(bike_data_prepped)





----------------------------------------------------------
  #Data Modeling
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

--------------------------------------------------------------
  #Data Analysis - Linear Regression
  ## Setup and Fit the Linear Regression Model
my_linear_model <- linear_reg() |> 
  set_engine("lm") |> 
  set_mode("regression") |> 
  fit(formula = count ~ ., data = bikesharedata)

my_model <- linear_reg() |> 
  set_engine("lm") |> 
  set_mode("regression")

#Workflow
bike_workflow <- workflow() %>% 
  add_recipe(bikerecipe) %>% 
  add_model(my_model) %>% 
  fit(data = trainData)

## Generate Predictions Using Linear Model
bike_predictions <- predict(bike_workflow,
                            new_data=testData) # Use fit to predict (test.csv file)
bike_predictions ## Look at the output


## Format the Predictions for Submission to Kaggle
kaggle_submission <- bike_predictions %>%
  bind_cols(., testData) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  #mutate(count = exp(count)) %>%  ######BackTrack from Log(count) to count
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

## Write out the file
vroom_write(x=kaggle_submission, file="RecipePreds.csv", delim=",")
--------------------------------------------
  ##Data Analysis - Pois Regression model 
  library(poissonreg)


my_pois_model <- poisson_reg() %>% #Type of model
  set_engine("glm") %>% # GLM = generalized linear model
  set_mode("regression") %>%
  fit(formula=count~., data=bikesharedata2)

## Generate Predictions Using Linear Model
bike_predictions <- predict(my_pois_model,
                            new_data=testData) # Use fit to predict
bike_predictions ## Look at the output

## Format the Predictions for Submission to Kaggle
pois_kaggle_submission <- bike_predictions %>%
  bind_cols(., testData) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction va
  rename(count=.pred) %>% #rename pred to count 
  mutate(datetime=as.character(format(datetime))) 

## Write out the file
vroom_write(x=pois_kaggle_submission, file="./PoissonPreds.csv", delim = ",")