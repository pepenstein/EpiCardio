library(tidymodels)
library(modeltime)
library(timetk)
library(lubridate)
library(glmnet)
both <- both %>% 
  mutate(Year2 = seq.Date(as.Date("2010-01-01", "%Y-%m-%d"),
                          as.Date("2020-01-01", "%Y-%m-%d"),
                          by = "year"))

time <- both %>% 
  select(Year2, N)
colnames(time) <- c("Year", "Deaths")

time %>% plot_time_series(Year, Deaths)

splits <- time_series_split(
  time, assess = "5 years",
  cumulative = TRUE
)

splits %>% 
  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan(Year, Deaths)


model_arima <- arima_reg() %>% 
  set_engine("auto_arima") %>% 
  fit(Deaths ~ Year, training(splits))

model_prophet <- prophet_reg(seasonality_yearly = TRUE) %>% 
  set_engine("prophet") %>% 
  fit(Deaths ~ Year, training(splits))

model_glmnet <- linear_reg(penalty = 0.01) %>% 
  set_engine("glmnet") %>% 
  fit(
    Deaths ~ year(Year)
    + as.numeric(Year),
      training(splits))

model_tbl <- modeltime_table(
  model_arima,
  model_prophet,
  model_glmnet
)

calib_tbl <- model_tbl %>% 
  modeltime_calibrate(testing(splits))

calib_tbl %>% modeltime_accuracy()

calib_tbl %>% 
  modeltime_forecast(
    new_data = testing(splits), 
    actual_data = time) %>% 
  plot_modeltime_forecast()


future_forecast_tbl <- calib_tbl %>% 
  modeltime_refit(time) %>% 
  modeltime_forecast(
    h = "5 years",
    actual_data = time
  )
future_forecast_tbl %>% 
  plot_modeltime_forecast()

colnames(future_forecast_tbl) <- c("ID", "Model", "Key", "Year", "Deaths", "Lower", "Upper")

write.csv(future_forecast_tbl, "forecast.csv")
