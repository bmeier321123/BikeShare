library(stacks)
library(tidymodels)
library(vroom)
library(glmnet)
library(rpart)
library(rlang)
library(ranger)
library(dials)
library(car)
library(lubridate)
library(dbarts)
library(xgboost)
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

#Read in Data----------------------------
trainData <- vroom("train.csv")
testData <- vroom("test.csv")

# Workflow Function
predict_export <- function(workFlowName, fileName) {
  x <- predict(workFlowName, new_data = testData) %>%
    bind_cols(testData) %>%
    select(datetime, .pred) %>%
    rename(count = .pred) %>%
    mutate(count = exp(count), 
           datetime = as.character(format(datetime)))
  
  vroom_write(x, file = fileName, delim = ",")
}

trainData <- trainData %>%
  select(-c(casual, registered)) %>%
  mutate(count=log(count))

# Assuming trainData is your dataset
trainData <- trainData %>%
  mutate(datetime = as.POSIXct(datetime)) 

# Create the recipe
my_recipe <- recipe(count ~ ., data = trainData, rankdeficient = "NA") %>%
  step_zv(all_predictors()) %>% 
  # Extract time features from datetime
  step_time(datetime, features = c("hour")) %>% 
  step_mutate(datetime_hour=factor(datetime_hour)) %>% 
  step_date(datetime, features = c("year", "dow")) %>%
  # Remove the original datetime variable
  step_rm(datetime) %>%
  step_rm(windspeed) %>% 
  # Convert categorical variables to factors
  step_mutate(season = factor(season, levels = c(1, 2, 3, 4), 
                              labels = c("Spring", "Summer", "Fall", "Winter")),
              holiday = factor(holiday, labels = c("No", "Yes")),
              workingday = factor(workingday, labels = c("No", "Yes"))) %>%
  step_mutate(weather=ifelse(weather==4 , 3, weather)) %>% 
  step_mutate(weather=factor(weather, levels = c(1,2,3), labels = c("Clear", "Misty", "Rain"))) %>%
  step_mutate(as.factor(weather)) %>% 
  step_mutate(as.factor(season)) %>% 
  step_mutate(as.factor(holiday)) %>% 
  step_mutate(as.factor(workingday)) %>% 
  # One-hot encode categorical variables
  step_dummy(all_nominal_predictors()) %>%
  # Normalize numeric predictors
  step_normalize(all_predictors())

# Preview the recipe
glimpse(bake(prep(my_recipe), trainData))

folds <- vfold_cv(trainData, v = 5, repeats=2)

## Create a control grid
untunedModel <- control_stack_grid() #If tuning over a grid
tunedModel <- control_stack_resamples() #If not tuning a model

## Penalized regression model
preg_model <- linear_reg(penalty=tune(),
                         mixture=tune()) %>% 
  set_engine("glmnet") 

preg_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(preg_model)

preg_tuning_grid <- grid_regular(penalty(),
                                 mixture(),
                                 levels = 5) 
preg_models <- preg_wf %>%
  tune_grid(resamples=folds,
            grid=preg_tuning_grid,
            metrics=metric_set(rmse),
            control = untunedModel) 

#Linear Regression

my_linear_model <- linear_reg() |> 
  set_engine("lm") |> 
  set_mode("regression") 

lin_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(my_linear_model) 

lin_reg_model <-fit_resamples(
  lin_wf,
  resamples = folds,
  metrics = metric_set(rmse),
  control = tunedModel
)



#Random Forests
ran_for_mod <- rand_forest(
  mtry = tune(),
  min_n = tune(),
  trees = 500) %>%
  set_engine("ranger") %>%
  set_mode("regression")

ran_for_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(ran_for_mod)

grid_of_tuning_params_for <- grid_regular(
  parameters(mtry(range = c(1, ncol(trainData) - 1)), min_n()),
  levels = 5
)

randfor_model <- ran_for_wf %>%
  tune_grid(resamples = folds,
            grid = grid_of_tuning_params_for,
            metrics = metric_set(rmse),
            control = untunedModel)

#Boost
boost_mod <- boost_tree(
  mtry = tune(),
  min_n = tune(),
  trees = 500) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

boost_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(boost_mod)

grid_of_tuning_params_b <- grid_regular(
  parameters(mtry(range = c(1, ncol(trainData) - 1)), min_n()),
  levels = 5
)

boostf_model <- boost_wf %>%
  tune_grid(resamples = folds,
            grid = grid_of_tuning_params_b,
            metrics = metric_set(rmse),
            control = untunedModel)

#Bart
bart_mod <- bart(
  trees = 500) %>%
  set_engine("dbarts") %>%
  set_mode("regression")

bart_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(bart_mod)

bart_model <-fit_resamples(
  bart_wf,
  resamples = folds,
  metrics = metric_set(rmse),
  control = tunedModel)

# Specify with models to include
my_stack <- stacks() %>%
  add_candidates(preg_models) %>%
  add_candidates(bart_model) %>%
  add_candidates(randfor_model) %>%
  add_candidates(boostf_model) 

## Fit the stacked model
stack_mod <- my_stack %>%
  blend_predictions() %>% # LASSO penalized regression meta-learner
  fit_members() ## Fit the members to the dataset

## If you want to build your own metalearner you'll have to do so manually
## using
stackData <- as_tibble(my_stack)

## Use the stacked data to get a prediction
predict_export(stack_mod, "Full_Stacking_2.csv")

