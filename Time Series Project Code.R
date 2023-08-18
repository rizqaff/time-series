library(readxl)
library(TSA)
library(tseries)
library(forecast)
library(ggplot2)
library(normtest)
library(sarima)
library(astsa)

### import data ###
library(readxl)
curah_jabar <- read_excel("curah_hujan.xlsx")
View(curah_jabar)

### pendefinisian data runtun waktu ###
curah.jabar <- ts(curah_jabar$curah,
                  start = 2009,
                  frequency = 12)
print(curah.jabar)
head(curah.jabar)
tail(curah.jabar)

### plot runtun waktu ###
plot(curah.jabar,
     main = "Plot Runtun Waktu",
     xlab = "Lag",
     lwd = 2)

### cek stasioneritas ###
adf.test(curah.jabar)

### cek differencing seasonal dan non-seasonal ###
ndiffs(curah.jabar)
nsdiffs(curah.jabar)

### dekomposisi ###
dekomposisi <- decompose(curah.jabar, type="multiplicative")
plot(dekomposisi)

### penentuan model menggunakan acf, pacf, dan eacf ###
ggtsdisplay(curah.jabar)
tsdisplay(curah.jabar)
eacf(curah.jabar)

### penentuan model menggunakan auto arima ###
auto.arima(curah.jabar)

model1 <- Arima(curah.jabar, order = c(1,0,0), seasonal = c(1,0,0))
model1

model2 <- Arima(curah.jabar, order = c(1,0,1), seasonal = c(1,0,0))
model2

model3 <- Arima(curah.jabar, order = c(1,0,0), seasonal = c(1,0,1))
model3

model4 <- Arima(curah.jabar, order = c(1,0,0), seasonal = c(2,0,0))
model4


### model fitting ###
cbind(model1, model2, model3, model4)

### Estimasi Parameter ###
model <- sarima(curah.jabar, 1,0,0,1,0,1,12)
model

### Diagnosis Model ###
## Analisis Resdiual ##
fit <- Arima(curah.jabar, order = c(1,0,0), seasonal = c(1,0,1), include.drift=TRUE)
fit

# Cek Kestasioneran Residual #
adf.test(residuals(fit))

# Cek Independensi #
checkresiduals(fit)

# Cek Normalitas #
jb.norm.test(residuals(fit))

qqnorm(model1$residuals)
qqline(model1$residuals)

model <- sarima(curah.jabar, 1,0,0,1,0,1,12)

### overfitting model ###
overfit1<-sarima(curah.jabar, 2,0,0,1,0,1,12)
overfit2<-sarima(curah.jabar, 1,0,1,1,0,1,12)
overfit3<-sarima(curah.jabar, 1,0,0,2,0,1,12)
overfit4<-sarima(curah.jabar, 1,0,0,1,0,2,12)

overfit1
overfit2
overfit3
overfit4

### Cross Validation ###
actual.Point <- window(curah.jabar, start = c(2019,8))
train <- window(curah.jabar, end = c(2019,7))
cv <- Arima(train, order = c(1,0,0), seasonal = c(1,0,1))
forecasted <- forecast(cv,  h = 5, level = 0.95)
par(mfrow = c(2,1))
plot(forecasted)
plot(curah.jabar)
cbind(actual.Point, forecasted)
CVcompare <- cbind(coef(model3), coef(cv))
CVcompare <- `colnames<-`(CVcompare, c("Model Awal", "Model Training"))
CVcompare

### Peramalan ###
ramalan <- forecast(fit,  h = 5, level = 0.95)
ramalan
plot(ramalan)
