library(car)
library(RODBC)
library(MASS)
library(gplots)
library(plyr)
library(lattice)
library(visreg)
library(Hmisc)
library(ppcor)
library(TSA)
library(graphics)
library(astsa)
library(tseries)
library(forecast)

data = ts(read.csv("Project Data.csv", header = FALSE))
print(data)

plot.ts(data)
acf(data)
#The plot of the time series data has a clear thread-like structure suggesting a stochastic trend, this is reflected in the acf of the data
#showing a very slow decay

diff1 = diff(data)
plot.ts(diff1)
acf(diff1)

#The data still appears to not be stationary with an increasing trend. We attempted detrending but that appeared to result in the time series being white noise
#The slowly decaying acf seems suggestive that there is still a stochastic process in the data
#We will apply second order differencing

diff2 = diff(diff1)
plot.ts(diff2)
acf(diff2)
pacf(diff2)
eacf(diff2)

#After differencing the data appears to be stationary.
#Potential to be ARI(1,2), IMA(2,1), ARIMA(1,2,2) and ARIMA(2,2,2)

data_ari12 = arima(data, order = c(1,2,0), method = "ML", include.mean = F)
data_ima21 = arima(data, order = c(0, 2, 1), method = "ML", include.mean = F)
data_arima122 = arima(data, order = c(1, 2, 2), method = "ML", include.mean = F)
data_arima222 = arima(data, order = c(2,2,2), method = "ML", include.mean = F)
data_IMA22 = arima(data, order = c(0,2,2), method = "ML", include.mean = F)
aic_bic_values = data.frame(
  Model = c("ARIMA(1,2,2)", "IMA(2,1)", "ARIMA(1,2,2)", "ARIMA(2,2,2)"),
  AIC = c(AIC(data_ari12), AIC(data_ima21), AIC(data_arima122), AIC(data_arima222)),
  BIC = c(BIC(data_ari12), BIC(data_ima21), BIC(data_arima122), BIC(data_arima222))
)


print(aic_bic_values)

#ARIMA(2,2,2) appears to have the lowest AIC, but not the lowest BIC. The lowest BIC goes to IMA21

#Residual Diagnostics

tsdiag(data_ari12)
shapiro.test(rstandard(data_ari12))
tsdiag(data_ima21)
shapiro.test(rstandard(data_ima21))
tsdiag(data_arima122)
shapiro.test(rstandard(data_arima122))
tsdiag(data_arima222)
shapiro.test(rstandard(data_arima222))

#All models appear to pass the shapiro wilk test, so they are all normally distributed in terms of the standardized residuals.
#None of them appear to have significant acf in the residuals.
#However only ARIMA(2,2,2) appear to satisfy the Ljung-Box test
#Based on the residual diagnostics, and the AIC/BIC ARIMA(2,2,2) appears to be the best fit for the data.

print(data_arima222)

fit2_pr = predict(data_arima222, n.ahead = 10)

forecast = fit2_pr$pred
se = fit2_pr$se

l = forecast - 1.96 * se 
u = forecast + 1.96 * se 

forecast_results = data.frame(Forecast = forecast, SE = se, Lower = l, Upper = u)
print(forecast_results)

par(mfrow = c(1, 1))
t = 1:length(data)
plot(
  t, data, type = "o", xlim = c(1, length(data) + 10), ylim = range(c(data, l, u)),
  xlab = "Time", ylab = "Observed Values", main = "ARIMA Forecast"
)

forecast_time = (length(data) + 1):(length(data) + length(forecast))
lines(forecast_time, forecast, col = "red", type = "o", pch = 16)

lines(forecast_time, u, col = "blue", lty = "dashed")
lines(forecast_time, l, col = "blue", lty = "dashed")

legend(
  "topleft",
  legend = c("Observed", "Forecast", "95% CI"),
  col = c("black", "red", "blue"), lty = c(1, 1, 2), pch = c(1, 16, NA),
  bg = "white"
)

