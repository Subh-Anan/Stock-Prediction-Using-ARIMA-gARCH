library(quantmod)
library(PerformanceAnalytics)
library(rugarch)

# Download historical stock prices of nasdaq
getSymbols("^IXIC", from = "2010-01-01", to = "2021-02-18")
getSymbols("^GSPC", from = "2010-01-01", to = "2021-02-18")

# Calculate daily returns
returns_1 <- dailyReturn(IXIC)
returns_2<- dailyReturn(GSPC)
summary(returns_1)
summary(returns_2)
# Plot returns
plot(returns_1, main = "NASDAQ COMP Daily Returns")
plot(returns_2, main = "S&P 500 Daily Returns")

# Plot ACF and PACF of returns
acf(returns_1, main = "ACF of NASDAQ Daily Returns")
pacf(returns_1, main = "PACF of NASDAQ Daily Returns")
acf(returns_2, main = "ACF of S&P 500 Daily Returns")
pacf(returns_2, main = "PACF of S&P 500 Daily Returns")

# Perform ADF test on the time series
adf_test <- adf.test(returns_1)
adf_test <- adf.test(returns_2)

# Print test results
cat("ADF Test Results:\n")
cat(paste("Test statistic:", adf_test$statistic, "\n"))
cat(paste("p-value:", adf_test$p.value, "\n"))
cat(paste("Critical values:", adf_test$parameter, "\n"))
cat(paste("Null hypothesis:", adf_test$method, "\n"))

# Check if the time series is stationary
if(adf_test$p.value < 0.05){
  cat("The time series is stationary.\n")
} else {
  cat("The time series is not stationary.\n")
}

# Estimate GARCH(1,1) model
garch_1 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), mean.model = list(armaOrder = c(0,0), include.mean = TRUE), distribution.model = "norm")
fit_1 <- ugarchfit(garch, returns_1)
garch_2 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), mean.model = list(armaOrder = c(0,0), include.mean = TRUE), distribution.model = "norm")
fit_2 <- ugarchfit(garch, returns_2)
fit_2

# Print coefficient estimates
coef(fit)
summary(fit)

#PREDICTING FUTURE VALUES

# Download nasdaq stock prices from Yahoo Finance
getSymbols("^IXIC", src = "yahoo", from = "2015-01-01", to = "2022-01-28", auto.assign = TRUE)
nasdaq <- data.frame(Date = as.Date(index(IXIC)), Close = as.numeric(IXIC$IXIC.Close))
nasdaq <- nasdaq[, c("Date", "Close")]
names(nasdaq)[2] <- "Price"
nasdaq <- na.omit(nasdaq)

getSymbols("^GSPC", src = "yahoo", from = "2015-01-01", to = "2022-01-28", auto.assign = TRUE)
index1 <- data.frame(Date = as.Date(index(GSPC)), Close = as.numeric(GSPC$GSPC.Close))
index1 <- index1[, c("Date", "Close")]
names(index1)[2] <- "Price"
index1 <- na.omit(index1)

# Create a time series object from the data
nasdaq_ts <- ts(nasdaq$Price, start = c(2015, 1), frequency = 252)
index1_ts <- ts(index1$Price, start = c(2015, 1), frequency = 252)

# Fit an ARIMA model to the time series data
arima_model_1 <- arima(nasdaq_ts, order = c(1,1,1))
summary(arima_model_1)
arima_model_1
arima_model_2 <- arima(index1_ts, order = c(1,1,1))
summary(arima_model_2)
arima_model_2

# Forecast future values
future_values_1 <- predict(arima_model_1, n.ahead = 50)
future_values_1
future_values_2 <- predict(arima_model_2, n.ahead = 50)
future_values_2

# Plot the present and future values
plot(nasdaq_ts, type = "l", col = "black", ylab="NASDAQ Returns")
lines(ts(future_values$pred, start = c(2022, 1), frequency = 252), col = "red")

plot(index1_ts, type = "l", col = "black",ylab="S&P 500 Returns" )
lines(ts(future_values$pred, start = c(2022, 1), frequency = 252), col = "red")