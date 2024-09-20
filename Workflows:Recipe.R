library(tidyverse)
library(tidymodels)
library(vroom)

# Read in Data
trainData <- vroom("train.csv")
testData <- vroom("test.csv")

# Workflow Function
predict_export <- function(workFlowName, fileName) {
  x <- predict(workFlowName, new_data = testData) %>%
    bind_cols(testData) %>%
    select(datetime, .pred) %>%
    rename(count = .pred) %>%
    mutate(count = pmax(1, count), 
           datetime = as.character(format(datetime)))
  
  vroom_write(x, file = fileName, delim = ",")
}

# Cleaning
trainData <- trainData %>%
  select(-c(casual, registered)) 

# Creating a Recipe
bikerecipe <- recipe(count ~ ., data = trainData) %>%
  step_zv(all_predictors()) %>% 
  step_time(datetime, features = c("hour")) %>% 
  step_date(datetime, features = "dow") %>% 
  step_mutate(as.integer(datetime_dow)) %>% 
  step_rm(datetime, datetime_dow) %>% 
  step_corr(all_predictors(), threshold = 0.5) %>% 
  step_mutate(weather=ifelse(weather==4 , 3, weather)) %>% 
  step_mutate(weather=factor(weather, levels = c(1,2,3), labels = c("Clear", "Misty", "Rain"))) %>% 
  step_mutate(season=factor(season, levels = c(1,2,3,4), labels = c("Spring", "Summer", "Fall", "Winter"))) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_nominal_predictors())

# Prepping the Recipe
bike_data_prepped <- prep(bikerecipe)



# Juicing shows the changes in the recipe
juice(bike_data_prepped)

# Create a Model
my_model <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

# Create a Workflow
bike_workflow <- workflow() %>%
  add_recipe(bikerecipe) %>%
  add_model(my_model) %>%
  fit(data = trainData)

# Export predictions using the function
predict_export(bike_workflow, "Workflow_Preds27.csv")




