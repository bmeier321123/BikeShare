library(tidymodels)
library(vroom)
library(glmnet)
library(poissonreg) #if you want to do penalized, poisson regression

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

# Cleaning
trainData <- trainData %>%
  select(-c(casual, registered)) %>%
  mutate(count=log(count))

## Create a recipe
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

## Penalized regression model
preg_model <- linear_reg(penalty=tune(),
                         mixture=tune()) %>% #Set model and tuning6
  set_engine("glmnet") # Function to fit in R7

## Set Workflow
preg_wf <- workflow() %>%
add_recipe(my_recipe) %>%
add_model(preg_model)

## Grid of values to tune over
grid_of_tuning_params <- grid_regular(penalty(),
                                      mixture(),
                                      levels = 5) ## L^2 total tuning possibilities

## Split data for CV
folds <- vfold_cv(trainData, v = 10, repeats=1)

## Run the CV
CV_results <- preg_wf %>%
tune_grid(resamples=folds,
          grid=grid_of_tuning_params,
          metrics=metric_set(rmse, mae, rsq)) #Or leave metrics NULL

## Plot Results (example)
collect_metrics(CV_results) %>% # Gathers metrics into DF
  filter(.metric=="rmse") %>%
  ggplot(data=., aes(x=penalty, y=mean, color=factor(mixture))) + geom_line()

## Find Best Tuning Parameters
bestTune <- CV_results %>%
  select_best(metric = "rmse")

## Finalize the Workflow & fit it
final_wf <-preg_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data=trainData)

## Predict
final_wf %>%
  predict(new_data = testData)

#Export
predict_export(final_wf, "Tuning_4.csv")

