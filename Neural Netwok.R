#First we need to read the data from the csv file with headers
data <- read.csv("weather_data_24hr_HI.csv", header = T)
#remove null row
data <- data[-4265,]
#remove columns based on feature selection
data <- data[,c(3,4,6,8,27,29)]
# we need to normalize the data (max-min normalization)
data$maxtempC <- (data$maxtempC-min(data$maxtempC))/(max(data$maxtempC)-min(data$maxtempC))
data$avgtempC <- (data$avgtempC-min(data$avgtempC))/(max(data$avgtempC)-min(data$avgtempC))
data$mintempC <- (data$mintempC-min(data$mintempC))/(max(data$mintempC)-min(data$mintempC))
data$WindChillC <- (data$WindChillC-min(data$WindChillC))/(max(data$WindChillC)-min(data$WindChillC))# Data Partition
data$DewPointC <- (data$DewPointC-min(data$DewPointC))/(max(data$DewPointC)-min(data$DewPointC))
#Data partition to divide our data into training anad testing data (70% training 30% testing)
#we set seed in order to be able to repeat the learning
set.seed(222)
ind <- sample(2,nrow(data), replace = T, prob =c(0.7,0.3))
training <- data[ind==1,]
testing <- data[ind==2,]
#install the neural network packages in R
install.packages("neuralnet")
library(neuralnet)
#We create a neural network n trained on the training data
#This neural network has the error function as the sum of squared error
#It has the activation function as the sigmoid function
#It has 2 neurons
n <- neuralnet(HeatIndexC~.,
               data = training,
               hidden = 2,
               stepmax = 9999999,
               err.fct ='sse',
               act.fct = 'logistic',
               linear.output = T)
n
#We plot our trained neural network
plot(n)
# We calculate the RMS error for our neural network on the training and testing dataset
output <-compute(n, training)
p1 <- output$net.result
sqrt(sum((training$HeatIndexC-p1)^2)/nrow(training))
max((training$HeatIndexC-p1))
output <- compute(n, testing)
p2 <- output$net.result
sqrt(sum((testing$HeatIndexC-p2)^2)/nrow(testing))





