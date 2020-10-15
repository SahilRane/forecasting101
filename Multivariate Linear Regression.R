#First we need to read the data from the csv file with headers
data <- read.csv("weather_data_24hr_HI.csv", header = T)
#remove null row
data <- data[-4265,]
#remove columns based on feature selection
data <- data[,c(3,4,6,8,26,28)]
# we need to normalize the data (max-min normalization)
data$maxtempC <- (data$maxtempC-min(data$maxtempC))/(max(data$maxtempC)-min(data$maxtempC))
data$avgtempC <- (data$avgtempC-min(data$avgtempC))/(max(data$avgtempC)-min(data$avgtempC))
data$mintempC <- (data$mintempC-min(data$mintempC))/(max(data$mintempC)-min(data$mintempC))
data$WindChillC <- (data$WindChillC-min(data$WindChillC))/(max(data$WindChillC)-min(data$WindChillC))# Data Partition
data$DewPointC <- (data$DewPointC-min(data$DewPointC))/(max(data$DewPointC)-min(data$DewPointC))
# Partitioning the Data into testing and training data
# If we repeat the learning, we get the same result
set.seed(222)
ind <- sample(2,nrow(data), replace = T, prob =c(0.7,0.3))
training <- data[ind==1,]
testing <- data[ind==2,]
# Training the Linear regression model
MLR <- lm(HeatIndexC~., data = training)
summary(MLR)
# Prediction on training
output <- predict(MLR, training[,-1])
df <- data.frame(output, training[,1])
df
sum((df[,1]-df[,2])^2)/nrow(training)
# Prediction on testing
output <- predict(MLR, testing[,-1])
df <- data.frame(output, testing[,1])
df
sum((df[,1]-df[,2])^2)/nrow(testing)
