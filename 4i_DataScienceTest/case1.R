library(dslabs)
library(tidyverse)
library(ggplot2)
library(forecast)
library(readr)
library(fpp2)
library(TTR)
library(caret)

# Importing data to R environment
cwd <- getwd()
file <- "/data/TFP.csv"
path <- file.path(cwd, file)

tfp_df <- as.data.frame(read_csv(path, col_names = TRUE))

# Data Frame Summary/Information
head(tfp_df)
dim(tfp_df)
ls(tfp_df)
summary(tfp_df)
view(tfp_df)
glimpse(tfp_df)

#Renaming columns
colnames(tfp_df) <- c("country", "year", "tfpna")

tfp_df %>% ggplot + 
  geom_line(aes(x = year, y = tfpna, color = country), lwd = 1.1) +
  ggtitle("Total Factor Productivity over Years") +
  ylab("TFP") + 
  xlab("Years")

tfp_USA <- tfp_df %>% filter(country == "USA")
tfp_CAN <- tfp_df %>% filter(country == "CAN")
tfp_MEX <- tfp_df %>% filter(country =="MEX")

# Creating Time Series Variable for each country
USA_TSobj <- ts(tfp_USA)
CAN_TSobj <- ts(tfp_CAN)
MEX_TSobj <- ts(tfp_MEX)

# Apply forecast for each time series
forecast_USA <- forecast(USA_TSobj[,3], h = 10)
forecast_CAN <- forecast(CAN_TSobj[,3], h = 10)
forecast_MEX <- forecast(MEX_TSobj[,3], h = 10)
summary(forecast_USA)

# Create Dataframe for forecasted values
tfp_USA_forecasted <- data.frame(c("USA"), c(2012:2021), c(forecast_USA$mean))
tfp_CAN_forecasted <- data.frame(c("CAN"), c(2012:2021), c(forecast_CAN$mean))
tfp_MEX_forecasted <- data.frame(c("MEX"), c(2012:2021), c(forecast_MEX$mean))

#Changinf column names for each forecasted dataframe
colnames(tfp_USA_forecasted) <- c("country", "year", "tfpna")
colnames(tfp_CAN_forecasted) <- c("country", "year", "tfpna")
colnames(tfp_MEX_forecasted) <- c("country", "year", "tfpna")

# Binding Dataframes
tfp_forecast <- rbind(rbind(tfp_df, tfp_USA_forecasted),rbind(tfp_CAN_forecasted,tfp_MEX_forecasted))

#Plotting Results
tfp_forecast %>% ggplot +
  geom_line(aes(x = year, y = tfpna, color = country), lwd = 1.1) +
  geom_vline(xintercept = 2011) +
  ggtitle("Total Factor Productivity over Years") +
  ylab("TFP") + 
  xlab("Years")

summary(forecast_USA)

# ARIMA autocorrelations descriptions
tmp_arima_USA <- auto.arima(USA_TSobj[,3])
tmp_arima_CAN <- auto.arima(CAN_TSobj[,3])
tmp_arima_MEX <- auto.arima(MEX_TSobj[,3])

# TBATS autocorrelations descriptions
# T: Trigonometric terms for seasonality
# B: Box-Cox transformations for heterogeneity
# A: ARMA errors for short-term dynamics
# T: Trend
# S: Seasonal (including multiple and non-integer periods)
USA_TSobj

tmp_tbats_USA <- tbats(USA_TSobj[,3])
tmp_tbats_CAN <- tbats(CAN_TSobj[,3])
tmp_tbats_MEX <- tbats(MEX_TSobj[,3])

# Forecast by autocorrelations
test_method_USA <- forecast::forecast(tmp_tbats_USA)
test_method_CAN <- forecast::forecast(tmp_tbats_CAN)
test_method_MEX <- forecast::forecast(tmp_tbats_MEX)


# TEST FORECASTING METHODS
# Apply forecast Method for each time series
test_method_USA <- holt(USA_TSobj[,3], h = 10)
test_method_CAN <- holt(CAN_TSobj[,3], h = 10)
test_method_MEX <- holt(MEX_TSobj[,3], h = 10)

# Create Dataframe for forecasted values
tfp_USA_tm <- data.frame(c("USA"), c(2012:2021), c(test_method_USA$mean))
tfp_CAN_tm <- data.frame(c("CAN"), c(2012:2021), c(test_method_CAN$mean))
tfp_MEX_tm <- data.frame(c("MEX"), c(2012:2021), c(test_method_MEX$mean))

#Changing column names for each forecasted dataframe
colnames(tfp_USA_tm) <- c("country", "year", "tfpna")
colnames(tfp_CAN_tm) <- c("country", "year", "tfpna")
colnames(tfp_MEX_tm) <- c("country", "year", "tfpna")


# Binding Dataframes
tfp_tm <- rbind(rbind(tfp_df, tfp_USA_tm),rbind(tfp_CAN_tm,tfp_MEX_tm))

#Plotting Results
tfp_tm%>% ggplot +
  geom_line(aes(x = year, y = tfpna, color = country), lwd = 1.1) +
  geom_vline(xintercept = 2011) +
  ggtitle("Total Factor Productivity over Years") +
  ylab("TFP") + 
  xlab("Years")

#Splitting tfp_df into tranining and testing set to evaluate error measurement

trainIndex <- createDataPartition(tfp_USA$tfpna, p = 0.75, list = FALSE)

tfp_train <- tfp_USA[trainIndex,]
tfp_test <- tfp_USA[-trainIndex,]

ts_train <- ts(tfp_train)
ts_test <- ts(tfp_test)

tmp_tbats_train <- tbats(ts_train[,3])
train_result <- forecast::forecast(tmp_tbats_train, h = 14)
summary(train_result)

df_tbats = as.data.frame(train_result)

dat_test <- df_tbats$`Point Forecast`

mape <- mean(abs((tfp_test$tfpna - dat_test)/tfp_test$tfpna))*100
mape
tfp_test$tfpna
dat_test
