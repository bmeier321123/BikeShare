library(tidymodels)
library(vroom)
library(glmnet)
library(rpart)
library(rlang)
library(ranger)
library(dials)
library(tidymodels)
library(dials)


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

## Create a workflow with model & recipe
trainData <- trainData %>%
  select(-c(casual, registered)) %>%
  mutate(count=log(count))

my_recipe <- recipe(count ~ ., data = trainData) %>%
  step_zv(all_predictors()) %>% 
  step_time(datetime, features = c("hour")) %>% 
  step_date(datetime, features = "dow") %>% 
  step_mutate(datetime_hour=factor(datetime_hour)) %>%
  step_rm(datetime) %>% 
  step_mutate(weather=ifelse(weather==4 , 3, weather)) %>% 
  step_mutate(weather=factor(weather, levels = c(1,2,3), labels = c("Clear", "Misty", "Rain"))) %>% 
  step_mutate(season=factor(season, levels = c(1,2,3,4), labels = c("Spring", "Summer", "Fall", "Winter"))) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_predictors())
glimpse(bake(prep(my_recipe), trainData))


ran_for_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(ran_for_mod)

# Specify the Random Forest model
ran_for_mod <- rand_forest(
  mtry = tune(),
  min_n = tune(),
  trees = 500
) %>%
  set_engine("ranger") %>%
  set_mode("regression")

# Create the grid for tuning
grid_of_tuning_params <- grid_regular(
  parameters(mtry(range = c(1, ncol(trainData) - 1)), min_n()),
  levels = 5
)

# Set up K-fold CV
folds <- vfold_cv(trainData, v = 5, repeats = 2)

# Tune the model
CV_results <- ran_for_wf %>%
  tune_grid(
    resamples = folds,
    grid = grid_of_tuning_params,
    metrics = metric_set(rmse, mae, rsq)
  )

## Finalize workflow and predict
bestTune <- CV_results %>%
  select_best(metric = "rmse")

## Set up grid of tuning values
randfor_wf <-ran_for_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data=trainData)

## Finalize workflow and predict
randfor_wf %>%
  predict(new_data = testData)

predict_export(randfor_wf, "RandomForest_1.csv")

