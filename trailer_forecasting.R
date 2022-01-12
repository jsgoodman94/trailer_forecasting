#forecasting
library(forecast)
library(prophet)

#ML
library(glmnet)
library(earth)
library(kernlab)
library(kknn)
library(randomForest)
library(ranger)
library(xgboost)
library(Cubist)

#deep learning
library(reticulate)

#time series
library(tidymodels)
library(rules)
library(modeltime)

#core libraries
library(tidyverse)
library(lubridate)
library(timetk)

# extras
library(DataExplorer)
library(fs)
library(anytime)

setwd("E:/Drive/Coding/R/trailer_forecasting")
data <- read.csv('trailer_forecasting.csv')

data %>% glimpse()

data_formatted <- data %>% 
  rename(Date = ï..Date) %>% 
  group_by(Date) %>% 
  summarize(pallets_shipped = sum(Pallets.Shipped)) %>% 
  mutate(Date = anydate(Date)) %>% 
  mutate(trailers_needed = round(pallets_shipped / 26)) %>% 
  select(-pallets_shipped)

data_formatted

data_formatted %>%  tk_summary_diagnostics(.date_var = Date)

data_prepared <- 
data_formatted %>% pad_by_time(.pad_value = 0,
                               .date_var  = Date,
                               .by        = "day")


splits <- 
  data_prepared %>% 
  time_series_split(
    date_var = Date,
    #last 8 weeks
    assess   = "4 week",
    cumulative = TRUE
  )

splits %>% 
  #split by training and testing sets in .key column
  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan(.date_var = Date,trailers_needed,
                           .interactive = FALSE)



model_prophet_fit <- prophet_reg() %>% 
  set_engine("prophet") %>% 
  fit(trailers_needed ~ Date, data = training(splits))

# modeltime process ----

#create table for multiple models

model_tbl <- modeltime_table(
  model_prophet_fit
)

#calibrate with new data 
calibration_tbl <- model_tbl %>% 
  modeltime_calibrate(
    new_data = testing(splits)
  )

# visualize forecast

calibration_tbl %>% 
  modeltime_forecast(actual_data = data_prepared) %>% 
  plot_modeltime_forecast(.interactive = FALSE)

calibration_tbl %>% 
modeltime_accuracy()

