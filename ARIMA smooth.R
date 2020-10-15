#first read the comma splitted values containing the dataset
data <- read.csv("weather.csv", header = T)
#filter only maxtempC out which is relevant to our timeseries
datamax <- data[,1]
#remove NA values
datamin <- data[,2]
data <- data[-4265]
#looking at the first few values of our vector
head(data)
#sequential data partition into training (70%) and testing data (30%)
trainingmax <- datamax[1:2990]
testingmax <- datamax[2991:4387]
trainingmin <- datamin[1:2990]
testingmin <- datamin[2991:4387]
testingmax<-c(testingmax, c(27.8, 30.2, 32, 31.8, 31.5, 32.2, 31.8, 28.4, 30.8, 31, 28, 26.8, 30, 31))
testingmin<-c(testingmin, c(25.3, 23.4, 25.5, 27, 27, 27.2, 25.5, 26, 25.5, 25.5, 25.5, 25, 25, 25.5))
#convert the data into a time series
ts <- ts(data, frequency = 365, start = c(2008,183))
#smooth the time series using the triangular moving average
tst2 <- ts(rollmean(rollmean(ts,2),2),frequency = 365, start = c(2008,183))
#plotting the original time series and the smoothed time series
plot(ts, col = 'green')
lines(tst2, col = 'blue')
#carrying out the augmented dickey-fuller test on our time series for stationarity
adf.test(ts)
#carrying out the kpss test to further confirm level stationarity of the data
kpss.test(ts, null = "Level")
#converting the training data into a time series
tstrain <- ts(training, frequency = 365, start = c(2008,183))
#smoothing the training time series using triangular moving average
ts1 <- ts(rollmean(rollmean(tstrain,2),2))
#converting the testing data into a time series
tstest <- ts(testing, frequency = 365, start = c(2016,252))
#smoothing the testing time series using triangular moving average
ts2 <- ts(rollmean(rollmean(tstest,2),2))
#we fit the training data to the arma(2,2) model
fitmax<-arima(trainingmax, c(2,0,2))
fitmin<-arima(trainingmin, c(2,0,2))
fit
plot(residuals(fit))
#finding out the RMS error of the arma(2,2) model
accuracy(fit)
#applying our previously derived model on the testing data
refitmax<-Arima(testingmax, model = fitmax)
#calculating the accuracy of our model on the testing data
refitmin<-Arima(testingmin, model = fitmin)
accuracy(refit)
c<-fitted(refitmax)
df<-data.frame(c,testingmax)
#plotting the residuals of our model
plot(residuals(fit))
predict(refitmax, n.ahead = 2)
predict(refitmin, n.ahead = 2)

