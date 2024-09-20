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

#Workflow Function
predict_export <- function(workFlowName, fileName) {
  
  x <- predict(workFlowName, new_data=testData) %>% 
    bind_cols(., testData) %>% 
    select(datetime, .pred) %>% 
    rename(count=.pred) %>% #
    mutate(count=pmax(0, count)) %>% 
    mutate(datetime=as.character(format(datetime))) 
  
  vroom_write(x, file=fileName, delim=",")
}

trainData <- trainData %>%
  select(-c(casual, registered)) 

  
##Data Analysis - Pois Regression model 
library(poissonreg)
  
  
my_pois_model <- poisson_reg() %>% #Type of model
  set_engine("glm") %>% # GLM = generalized linear model
  set_mode("regression") %>%
  fit(formula=count~., data=trainData)

  
#Write out file
predict_export(my_pois_model, "Poisson_Preds2.csv")
