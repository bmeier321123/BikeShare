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

#Read in Data-------------------------------------
trainData <- vroom("train.csv")
testData <- vroom("test.csv")

#Workflow Function-------------------------------------
predict_export <- function(workFlowName, fileName) {
  
  x <- predict(workFlowName, new_data=testData) %>% 
    bind_cols(., testData) %>% 
    select(datetime, .pred) %>% 
    rename(count=.pred) %>% #
    mutate(count=pmax(0, count)) %>% 
    mutate(datetime=as.character(format(datetime))) 
  
  vroom_write(x, file=fileName, delim=",")
}
  
  
#Data Analysis - Linear Regression-------------------------------------
## Setup and Fit the Linear Regression Model
my_linear_model <- linear_reg() |> 
  set_engine("lm") |> 
  set_mode("regression") |> 
  fit(formula = count ~ ., data = bikesharedata)


## Generate Predictions Using Linear Model
predict_export(my_linear_model, "Linear_Model_Preds.csv")