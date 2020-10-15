#first read the comma splitted values containing the dataset
data <- read.csv("weather_data_24hr_HI.csv", header = T)
#filter only maxtempC out which is relevant to our timeseries
data <- data[,3]
#remove NA values
data <- data[-4265]
data
#Let's partition the data into testing and training data
training <- data[1:2990]
testing <- data[2991:4264]
#Create a timeseries using the maxtempC data
timeseries <- ts(training, frequency = 365, start = c(2008,183))
timeseriestest <- ts(testing,  frequency = 365, start = c(2016, 252))
timeseries
#plot the timeseries
plot.ts(timeseries)
install.packages("TTR")
library("TTR")
#Simple exponential Smoothing
timeseriesforecasts <- HoltWinters(timeseries, beta=FALSE, gamma=FALSE, l.start = 31)
timeseriesforecasts
timeseriesforecasts$fitted
plot(timeseriesforecasts)
#calculate the sum of squared errors for our forecasts
a<-(timeseriesforecasts$SSE)
#calculate the mean of the SSE
MSE<- a/length(training)
#calculate the RMS error
sqrt(MSE)
testmodel<-HoltWinters(timeseriestest, alpha = 0.7159879, beta = FALSE, gamma = FALSE)
#calculate the sum of squared errors for our forecasts
a<-testmodel$SSE
#calculate the mean of the SSE
MSE<-a/length(testing)
sqrt(MSE)

#Holts exponential Smoothing
timeseriesforecasts <- HoltWinters(timeseries, gamma=FALSE, l.start = 27)
timeseriesforecasts$fitted
plot(timeseriesforecasts)
a<-(timeseriesforecasts$SSE)
a/length(data)
#Holts Winter Exponential Smoothing
timeseriesforecasts <- HoltWinters(timeseries, l.start = 27)
timeseriesforecasts$fitted
plot(timeseriesforecasts)
a<-(timeseriesforecasts$SSE)
a/length(data)

#ARIMA model
#first difference the time series to obtain stationary data
data <- read.csv("weather_data_24hr_master1.csv", header = T)
data <- data[,4]
data <- data[-4265]
x_mat = data.matrix(data[,c(4,5,6)])
ts <- ts(data, frequency = 365, start = c(2008,183))
?difference

fit<-auto.arima(tsd)
accuracy(fit)

plot(tsd)
training <- data[1:2990]
testing <- data[2991:4264]
tstrain <- ts(training, frequency = 365, start = c(2008,183))
tstest <- ts(testing, frequency = 365, start = c(2008,183))
install.packages("tseries")
plot(tstrain)
plot(tstest)
library(tseries)
adf.test(tstrain)
adf.test(tstest)
acf(tstrain, lag.max=20)
pacf(tstrain, lag.max=20)
install.packages("forecast")
library(forecast)
fit<-auto.arima(ts)
fit
accuracy(fit)
refit<-Arima(tstest,fit)
fit <- arima(ts, order = c(5,2,5),c(1,1,1), xreg = x_mat)
(sum(residuals(fit)^2))/4264
?Arch
?arch
fit <- garch(tstrain, c(1,6))
sqrt((sum(residuals(fit)^2, na.rm = T))/2990)
fit






data <- read.csv("weather_data_24hr_master1.csv", header = T)
data <- data[,c(4)]
data<- data[-4265]
mts<-ts(data)
plot(mts)

ts <- ts(data, frequency = 365, start = c(2008,183))
auto.arima(ts)
cts <- tsclean(ts, replace.missing = T)
plot(cts)
lines(ts)
tsclean(ts)
acf(cts, lag.max = 20)
pacf(cts, lag.max = 20)
fit<-arima(ts,order =c(4,0,0))
accuracy(fit)
fit
refit<-Arima(tstest, model = fit)
accuracy(refit)


training <- data[1:2990]
testing <- data[2991:4264]

tstrain <- ts(training, frequency = 365, start = c(2008,183))
tstraind <- diff(tstrain, differences = 1)
tstest <- ts(testing, frequency = 365)
tstestd <- diff(tstrain, differences = 1)
acf(tstrain, lag.max = 20)
pacf(tstrain, lag.max = 20)
fit<-arfima(ts,c(4,0,0))
accuracy(fit)
fit
refit<-Arima(tstest, model = fit)
accuracy(refit)


fit<-auto.arima(tstrain)
fit<-Arima(ts,c(4,0,0))
accuracy(fit)
fit
refit<-Arima(tstest, model = fit)
accuracy(refit)
tserror <- ts(residuals(fit),frequency = 365)
gwn <- rnorm(2990,0, 0.8)
gwn
residual<-sqrt(sum((tserror-gwn)^2,na.rm = T)/2990)
residual
tserrortest <- ts(residuals(refit),frequency = 365)
gwntest <- rnorm(1274,0,1)
residual<-sum(abs(tserrortest-gwntest),na.rm = T)/1274
residual
head(tserror)
size(tserror)
plot(tserror)
plot(density(tserror), type="l", col="red" )
par(new=TRUE)
plot(density(rnorm(x,0,0.8)), type="l", col="green" )

plot(density(tserror), type="l", col="red" )
lines(density(rnorm(2990,0,0.8)), type="l", col="green" )
plot(rnorm(0,1,1))







a<-garchFit(formula = ~ garch(30,30), data = tserror)
sqrt(sum(residuals(a)^2)/2990)


library(rugarch)

garch11        <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,3)), 
                             mean.model = list(armaOrder = c(4, 4), include.mean = TRUE), 
                             distribution.model = "norm")

garchfit       <- ugarchfit(spec = garch11, data = ts, solver = "hybrid")
garchfit
sum(residuals(garchfit)^2)/4264

install.packages("arfima")
library(arfima)
arfima(ts,order = c(4,0,0))

``

