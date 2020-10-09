############# ANALYSIS OF AUSTRALIAN GAS PRODUCTION #########

### Invoking of the necessary libraries ####
install.packages('forecast')
library(forecast)
install.packages('tseries')
library(tseries)
install.packages('ggplot2')
library(ggplot2)
install.packages('dygraphs')
library(dygraphs)
install.packages('xts')
library(xts)
install.packages('fts')
library(fts)
install.packages('TSA')
library(TSA)
install.packages('Metrics')
library(Metrics)
### As the dataset to be used for the analysis is present in the ####
### forecast library, the data can be called directly using ###
### the data name 'gas'. For our analysis, we import it and
### name it 'gasprod' ###
gasprod = gas

### Viewing the data ####
print(gasprod)

### Checking the class of the imported ####
class(gasprod)

### Inspection of the time series data ####
summary(gasprod)
anyNA(gasprod)
findfrequency(gasprod)
ts.plot(gasprod,gpars = list(xlab = "Year",ylab = "Gas Production",
                             main = "Australian Gas Production (1956-1995)",
                             col = c("Blue")),lwd = 2)
stepplot = dygraph(gasprod,main = "Australian Gas Production (1956-1995)",
                   xlab = "Year",ylab = "Gas Production") %>%dyOptions(stepPlot= TRUE,pointSize = 0,fillGraph = TRUE,
                                         fillAlpha = 0.2)
stepplot

### Inspection of individual elements by decomposition of time series ####
dc.gasprod = stl(gasprod,s.window = "periodic")
plot(dc.gasprod)
monthplot(gasprod,main = "Month Plot for Australian Gas Production")
seasonplot(gasprod,year.labels = TRUE,
           main = "Month Plot for Australian Gas Production")

### De-Seasonalizing the time series from the decomposed time series####
ds.gasprod = (dc.gasprod$time.series[,2]+dc.gasprod$time.series[,3])
ts.plot(ds.gasprod,gpars = list(xlab = "Year",ylab = "Gas Production",
                             main = "Australian Gas Production (Deseasonlized)",
                             col = c("Red")),lwd = 2)
ts.plot(ds.gasprod,gasprod,gpars = list(xlab = "Year",ylab = "Gas Production",
                               main = "Australian Gas Production (Deseasonlized
                               vs. Original)",
                               col = c("Red","Blue")),lwd = 2)

### Splitting the time series into training and testing samples (Original) ####
train.gasprod = window(gasprod,start=c(1970,1), end=c(1993,12), freq=12)
ts.plot(train.gasprod)
test.gasprod = window(gasprod,start=c(1994,1),end=c(1995,8), freq=12)
ts.plot(test.gasprod)
autoplot(train.gasprod, series="Train") + autolayer(test.gasprod, series="Test") + 
  ggtitle("Gas Produciton Traning and Test data") + 
  xlab("Year") + ylab("Production") + 
  guides(colour=guide_legend(title="Forecast"))

### Splitting the time series into training and testing samples(Deseasonalize) ####
train.ds.gasprod = window(ds.gasprod,start=c(1970,1), end=c(1993,12), freq=12)
ts.plot(train.ds.gasprod)
test.ds.gasprod = window(ds.gasprod,start=c(1994,1),end=c(1995,8), freq=12)
ts.plot(test.ds.gasprod)
autoplot(train.ds.gasprod, series="Train") + autolayer(test.ds.gasprod, series="Test") + 
  ggtitle("Gas Produciton Traning and Test data(Deseasonalized)") + 
  xlab("Year") + ylab("Production") + 
  guides(colour=guide_legend(title="Forecast"))

### Checking the periodicity of the Time Series ####
periodicity(gasprod)
findfrequency(gasprod)
periodicity(train.gasprod)
findfrequency(train.gasprod)
periodicity(test.gasprod)
findfrequency(test.gasprod)

### Usage of Different methods to create models using Deseasonlazied and ####
### Seasonlized Data ###

#### Naive Method ####
gasprod.rw = stl(train.gasprod,s.window = 'p')
gasprod.rw = forecast(de.gasprod.rw,method = "rwdrift",h = 20)
ts.plot(test.gasprod,gasprod.rw$mean,gpars = list(col = c("Red","Blue"),
        main = "Random Walk with Drift(Original vs. Forecasted)"))
legend("topleft", legend = c("Actual","Forecasted"),col = c("Red","Blue"),lty = 1,
       box.lwd = 0.1,cex = 0.75)
vec.rw = cbind(test.gasprod,gasprod.rw$mean)
MAPE.rw = mean(abs(vec.rw[,1]-vec.rw[,2])/vec.rw[,1])
print(MAPE.rw)


#### Simple Exponential Smoothing (Original) ####
gasprod.ses = ses(train.gasprod,start = c(1970,1),end = c(1993,12),frequency = 12,h = 20)
summary(gasprod.ses)
print(gasprod.ses$mean)
ts.plot(test.gasprod,gasprod.ses$mean,gpars = list(col = c("Red","Blue"),
                                                  main = "Simple Exponential Smoothing(Original vs. Forecasted)"))
legend("topleft", legend = c("Actual","Forecasted"),col = c("Red","Blue"),lty = 1,
       box.lwd = 0.1,cex = 0.75)
vec.ses = cbind(test.gasprod+gasprod.ses$mean)
MAPE.ses = mape(test.gasprod,gasprod.ses$mean)
print(MAPE.ses)

#### Simple Exponential Smoothing (Deseasonalized) ####
gasprod.dses = ses(train.ds.gasprod,start = c(1970,1),end = c(1993,12),frequency = 12,h = 20)
summary(gasprod.dses)
print(gasprod.dses$mean)
ts.plot(test.ds.gasprod,gasprod.dses$mean,gpars = list(col = c("Red","Blue"),
                                                  main = "Simple Exponential Smoothing(Original vs. Forecasted)"))
legend("topleft", legend = c("Actual","Forecasted"),col = c("Red","Blue"),lty = 1,
       box.lwd = 0.1,cex = 0.75)
MAPE.dses = mape(test.ds.gasprod,gasprod.dses$mean)
print(MAPE.dses)

#### Double Exponential Method (Holt Model) (Originial) ####
gasprod.holt = holt(train.gasprod ,start=c(1970,1),end=c(1993,12), freq=12,h=20)
summary(gasprod.holt)
gasprod.holt$mean
ts.plot(test.gasprod,gasprod.holt$mean,gpars = list(col = c("Red","Blue"),
                                                  main = "Holt's Method(Original vs. Forecasted)"))
legend("topleft", legend = c("Actual","Forecasted"),col = c("Red","Blue"),lty = 1,
       box.lwd = 0.1,cex = 0.75)
MAPE.holt = mape(test.gasprod,gasprod.holt$mean)
print(MAPE.holt)

#### Double Exponential Method (Holt Model) (Deseasonlaize) ####
gasprod.dholt = holt(train.ds.gasprod,start = c(1970,1),end = c(1993,12),freq = 12,h = 20)
summary(gasprod.dholt)
gasprod.dholt$mean
ts.plot(test.ds.gasprod,gasprod.dholt$mean,gpars = list(col = c("Red","Blue"),
                                                  main = "Holt's Method(Original vs. Forecasted)"))
legend("topleft", legend = c("Actual","Forecasted"),col = c("Red","Blue"),lty = 1,
       box.lwd = 0.1,cex = 0.75)
MAPE.dholt = mape(test.ds.gasprod,gasprod.dholt$mean)
print(MAPE.dholt)

#### Holt Winter's method (Original) ####
gasprod.hw = hw(train.gasprod,start = c(1970,1),end = c(1993,12),freq = 12,h = 20)
summary(gasprod.hw)
gasprod.hw$mean
ts.plot(test.gasprod,gasprod.hw$mean,gpars = list(col = c("Red","Blue"),
                                                  main = "Holt Winter's(Original vs. Forecasted)"))
legend("topleft", legend = c("Actual","Forecasted"),col = c("Red","Blue"),lty = 1,
       box.lwd = 0.1,cex = 0.75)
MAPE.hw = mape(test.gasprod,gasprod.hw$mean)
print(MAPE.hw)

#### Holt Winter's method (Deseaonalize) ####
gasprod.dhw = hw(train.ds.gasprod,start = c(1970,1),end = c(1993,12),freq = 12,h = 20)
summary(gasprod.dhw)
gasprod.dhw$mean
ts.plot(test.ds.gasprod,gasprod.dhw$mean,gpars = list(col = c("Red","Blue"),
                                                  main = "Holt Winter's(Original vs. Forecasted)"))
legend("topleft", legend = c("Actual","Forecasted"),col = c("Red","Blue"),lty = 1,
       box.lwd = 0.1,cex = 0.75)
MAPE.dhw = mape(test.ds.gasprod,gasprod.dhw$mean)
print(MAPE.dhw)




### USING REGRESSION MODELS ON THE TIME SERIES ####
 
#### Checking for stationarity using visualization ####
ts.plot(gasprod,gpars = list(xlab = "Year",ylab = "Gas Production",
                             main = "Australian Gas Production (1960-1995)",
                             col = c("Blue")),lwd = 2)
#### Plotting ACF and PACF plots ####
acf(gasprod, lag.max = 50)
pacf(gasprod, lag.max = 50)
#### Checking for  stationarity using Augmented Dicky- Fuller Test ####
adf = adf.test(gasprod,alternative = "stationary")
adf
print(adf$alternative)
if(adf$p.value < 0.05){
  print("The series is Stationary & Null Hypothesis is rejected")
} else{
  print("The Series is not Stationary & Null Hypothesis is accepted")
}

#### Stationarizing the series for performing Manual Arima ####
diff.gasprod = diff(gasprod, differences = 1)
ts.plot(diff.gasprod,col = c("Blue"),lwd = 1,main = "Differenced Series") 
abline(a=1,b=0,col = c("Red"),lwd =4)
acf(diff.gasprod, lag.max = 50)
pacf(diff.gasprod, lag.max = 50)
adf.d = adf.test(diff.gasprod,alternative = "stationary")
adf.d
print(adf.d$alternative)
if(adf.d$p.value < 0.05){
  print("The series is Stationary & Null Hypothesis is rejected")
} else{
  print("The Series is not Stationary & Null Hypothesis is accepted")
}

### Splitting of time series into Training and Testing Data ####
diff.train.gasprod = window(diff.gasprod,start = c(1970,1),end = c(1993,12),frequency = 12)
diff.test.gasprod = window(diff.gasprod,start = c(1994,1),end = c(1995,8),frequency = 12)
plot.ts(diff.train.gasprod,main = "Differenced Train")
plot.ts(diff.test.gasprod,main = "Differenced Test")

### Finding the P and Q values ####
acf(diff.train.gasprod)
pacf(diff.train.gasprod)

### Building the Manual ARIMA model ####
marima.gasprod = Arima(diff.train.gasprod,order = c(2,1,2))
marima.gasprod
forc.marima.gasprod = forecast(marima.gasprod,h = 20)
ts.plot(diff.test.gasprod,forc.marima.gasprod$mean,gpars = list(col = c("Red","Blue"),
                                                  main = "Manual Arima(Original vs. Forecasted)"))
legend("topleft", legend = c("Actual","Forecasted"),col = c("Red","Blue"),lty = 1,
       box.lwd = 0.1,cex = 0.50)
vec.marima = cbind(diff.test.gasprod,forc.marima.gasprod$mean)
MAPE.marima = mean(abs(vec.marima[,1]-vec.marima[,2])/vec.marima[,1])
print(MAPE.marima)

###USING AUTO ARIMA (Original) ####
aarima.gasprod = auto.arima(train.gasprod,seasonal = TRUE)
aarima.gasprod
forc.aarima = forecast(aarima.gasprod,h = 20)
ts.plot(test.gasprod,forc.aarima$mean,gpars = list(col = c("Red","Blue"),
                                                  main = "Auto Arima(Original vs. Forecasted)"))
legend("topleft", legend = c("Actual","Forecasted"),col = c("Red","Blue"),lty = 1,
       box.lwd = 0.1,cex = 0.75)
vec.aarima = cbind(test.gasprod,forc.aarima$mean)
MAPE.aarima = mean(abs(vec.aarima[,1]-vec.aarima[,2])/vec.aarima[,1])
print(MAPE.aarima)

### Ljung box test ####
Box.test(aarima.gasprod$residuals,type = "Ljung-Box")
hist(aarima.gasprod$residuals,main = "Histogram of Auto ARIMA residuals",
     xlab = "Residuals")

### Making the future forecastusing the best model ####
future = forecast(aarima.gasprod,h = 32)
plot(future)
future$mean


### Making the future forecast using the model created from gasprod ####
aarima.fgasprod = auto.arima(gasprod,seasonal = TRUE)
fgasprod.forc = forecast(aarima.fgasprod,h = 12)
plot(fgasprod.forc)
fgasprod.forc$mean

