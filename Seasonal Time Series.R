library(tseries)
library(tidyverse)
library(ggplot2)
library(stats)
library(TSA)
library(forecast)

df = read_csv("C:/Users/Aston/Downloads/Academic Projects/Time Series Project/HotelEmployees.csv")
df <- df %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))
str(df)
head(df)
tail(df)

plot(df, main='Original Data', type='l')

data = df$Employees
data_12 = ts(data, frequency = 12)
decompose_data = decompose(data_12, 'multiplicative')
plot(decompose_data, type='l', lwd=1, col='blue')

num_rows <- nrow(df)
train_data <- df[1:(num_rows-12),]
tail(train_data)
test_data <- tail(df, 12)

data = train_data$Employees
acf(data, lag.max = 200, main='ACF of Original Plot')
pacf(data, lag.max = 100, main='PACF of Original Plot')
#spikes at regular intervals

adf_test = adf.test(data)
print(adf_test)
print("Since p-value=0.9769 > 0.05, the TS is non-stationary")

employ_diff = diff(data)
employ_diff <- ts(employ_diff, frequency=12)
plot(employ_diff, type='l', xlab='Date', ylab='Value', main='Line graph for first-differencing of Employee count')

adf_test = adf.test(employ_diff)
print(adf_test)
print("Since p-value=0.01 < 0.05, the TS is stationary")

acf(employ_diff, lag.max = 100, main='ACF of Original Plot')
pacf(employ_diff, lag.max = 50, main='PACF of Original Plot')

#
sarima110100 <- arima(employ_diff, order = c(1,1,0), seasonal = list(order = c(1,0,0),period=12))
sarima110200 <- arima(employ_diff, order = c(1,1,0), seasonal = list(order = c(2,0,0),period=12))
sarima110300 <- arima(employ_diff, order = c(1,1,0), seasonal = list(order = c(3,0,0),period=12))

#
sarima210100 <- arima(employ_diff, order = c(2,1,0), seasonal = list(order = c(1,0,0),period=12))
sarima210200 <- arima(employ_diff, order = c(2,1,0), seasonal = list(order = c(2,0,0),period=12))
sarima210300 <- arima(employ_diff, order = c(2,1,0), seasonal = list(order = c(3,0,0),period=12))

#
sarima310100 <- arima(employ_diff, order = c(3,1,0), seasonal = list(order = c(1,0,0),period=12))
sarima310200 <- arima(employ_diff, order = c(3,1,0), seasonal = list(order = c(2,0,0),period=12))
sarima310300 <- arima(employ_diff, order = c(3,1,0), seasonal = list(order = c(3,0,0),period=12))

#
sarima410100 <- arima(employ_diff, order = c(4,1,0), seasonal = list(order = c(1,0,0),period=12))
sarima410200 <- arima(employ_diff, order = c(4,1,0), seasonal = list(order = c(2,0,0),period=12))
sarima410300 <- arima(employ_diff, order = c(4,1,0), seasonal = list(order = c(3,0,0),period=12))

#
sarima510100 <- arima(employ_diff, order = c(5,1,0), seasonal = list(order = c(1,0,0),period=12))
sarima510200 <- arima(employ_diff, order = c(5,1,0), seasonal = list(order = c(2,0,0),period=12))
sarima510300 <- arima(employ_diff, order = c(5,1,0), seasonal = list(order = c(3,0,0),period=12))

#
sarima610100 <- arima(employ_diff, order = c(6,1,0), seasonal = list(order = c(1,0,0),period=12))
sarima610200 <- arima(employ_diff, order = c(6,1,0), seasonal = list(order = c(2,0,0),period=12))
sarima610300 <- arima(employ_diff, order = c(6,1,0), seasonal = list(order = c(3,0,0),period=12))

#
sarima710100 <- arima(employ_diff, order = c(7,1,0), seasonal = list(order = c(1,0,0),period=12))
sarima710200 <- arima(employ_diff, order = c(7,1,0), seasonal = list(order = c(2,0,0),period=12))
sarima710300 <- arima(employ_diff, order = c(7,1,0), seasonal = list(order = c(3,0,0),period=12))

sarima110100
sarima110200
sarima110300
sarima210100
sarima210200
sarima210300
sarima310100
sarima310200
sarima310300
sarima410100
sarima410200
sarima410300
sarima510100
sarima510200
sarima510300
sarima610100
sarima610200
sarima610300
sarima710100
sarima710200
sarima710300

BIC_values <- c(BIC(sarima110100), BIC(sarima110200), BIC(sarima110300),
                BIC(sarima210100), BIC(sarima210200), BIC(sarima210300),
                BIC(sarima310100), BIC(sarima310200), BIC(sarima310300),
                BIC(sarima410100), BIC(sarima410200), BIC(sarima410300),
                BIC(sarima510100), BIC(sarima510200), BIC(sarima510300),
                BIC(sarima610100), BIC(sarima610200), BIC(sarima610300),
                BIC(sarima710100), BIC(sarima710200), BIC(sarima710300))
BIC_values

min_BIC <- min(BIC_values)
min_BIC

min_BIC_index <- which.min(BIC_values)
min_BIC_index

print("From the above BIC values, SARIMA(5,1,0)x(3,0,0) has the lowest value")
sarima_model <- arima(employ_diff, order = c(5, 1, 0), seasonal = list(order = c(3, 0, 0), period = 12))
residuals = residuals(sarima510300)
acf(residuals, main="ACF of residuals")
pacf(residuals, main="PACF of residuals")
tsdiag(sarima510300)

predicted_values <- predict(sarima510300, n.ahead = 12)  # Predicting next 12 months
print(predicted_values)

original_forecasts <- diffinv(predicted_values$pred, lag = 1, differences = 1, xi = 1957)
original_forecasts <- original_forecasts[original_forecasts != 0]
original_forecasts <- original_forecasts[-1]
original_forecasts

plot(test_data$Date, test_data$Employees, main = 'Original Data vs Forecast for 2018', type = 'l', col = 'blue', xlab = 'Date', ylab = 'Employees')

# Adding forecasted values to the plot (if 'original_forecasts' is aligned with 'test_data$Date')
lines(test_data$Date, original_forecasts, col = 'red', type = 'l')

# Adding legend
legend('bottomright', legend = c('Original Data', 'Forecasts'), col = c('blue', 'red'), lty = 1)



