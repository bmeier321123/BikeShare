#Penalized Regression-------------------------------
library(vroom)
library(glmnet)
library(tidymodels)
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
    mutate(count = pmax(1, count), 
           datetime = as.character(format(datetime)))
  
  vroom_write(x, file = fileName, delim = ",")
}
# Cleaning
trainData <- trainData %>%
  select(-c(casual, registered)) 

## Create a recipe
my_recipe <- recipe(count ~ ., data = trainData) %>%
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

## Penalized regression model
preg_model <- linear_reg(penalty = 1, mixture = 0) %>% 
  set_engine("glmnet") 

preg_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(preg_model) %>%
  fit(data=trainData)

predict_export(preg_wf, "Pen_Reg8.csv")

##do 5 different combinations
  

