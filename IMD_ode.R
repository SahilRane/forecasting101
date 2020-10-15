# Let us get the data
# we need to make sure that you are in the right directory where data resides
data <- read.csv("weather_data_24hr_HI.csv", header = T)
# Data structure is called data-frame
# Let us look at the data structure
str(data)
data = data[-4265,]
# we need to normalize the data (max-min normalization)
hist(data$pressureMB)
data$maxlag1 <- (data$maxlag1-min(data$maxlag1))/(max(data$maxlag1)-min(data$maxlag1))
hist(data$maxlag1)
plot(density(data$maxlag1))
data$minlag1 <- (data$minlag1-min(data$minlag1))/(max(data$minlag1)-min(data$minlag1))
hist(data$minlag1)
plot(density(data$minlag1))
data$avglag1 <- (data$avglag1-min(data$avglag1))/(max(data$avglag1)-min(data$avglag1))
hist(data$avglag1)
plot(density(data$avglag1))
data$DAY <- (data$DAY-min(data$DAY))/(max(data$DAY)-min(data$DAY))
data$MONTH <- (data$MONTH-min(data$MONTH))/(max(data$MONTH)-min(data$MONTH))
data$YEAR <- (data$YEAR-min(data$YEAR))/(max(data$YEAR)-min(data$YEAR))
data$totalprecipMM <- (data$totalprecipMM-min(data$totalprecipMM))/(max(data$totalprecipMM)-min(data$totalprecipMM))
data$windspeedKmph <- (data$windspeedKmph-min(data$windspeedKmph))/(max(data$windspeedKmph)-min(data$windspeedKmph))
data$sunhour <- (data$sunhour-min(data$sunhour))/(max(data$sunhour)-min(data$sunhour))
data$winddirdegree <- (data$winddirdegree-min(data$winddirdegree))/(max(data$winddirdegree)-min(data$winddirdegree))
data$humidity <- (data$humidity-min(data$humidity))/(max(data$humidity)-min(data$humidity))
data$visibilityKm <- (data$visibilityKm-min(data$visibilityKm))/(max(data$visibilityKm)-min(data$visibilityKm))
data$pressureMB <- (data$pressureMB-min(data$pressureMB))/(max(data$pressureMB)-min(data$pressureMB))
data$cloudcover <- (data$cloudcover-min(data$cloudcover))/(max(data$cloudcover)-min(data$cloudcover))
data$HeatIndexC <- (data$HeatIndexC-min(data$HeatIndexC))/(max(data$HeatIndexC)-min(data$HeatIndexC))
data$DewPointC <- (data$DewPointC-min(data$DewPointC))/(max(data$DewPointC)-min(data$DewPointC))
data$WindChillC <- (data$WindChillC-min(data$WindChillC))/(max(data$WindChillC)-min(data$WindChillC))
data$WindGustKmph <- (data$WindGustKmph-min(data$WindGustKmph))/(max(data$WindGustKmph)-min(data$WindGustKmph))
data$FeelsLikeC <- (data$FeelsLikeC-min(data$FeelsLikeC))/(max(data$FeelsLikeC)-min(data$FeelsLikeC))
# Data Partition
# If we repeat the learning, we get the same result

ind <- sample(2,nrow(data), replace = T, prob =c(0.8,0.2))
training <- data[ind==1,]
testing <- data[ind==2,]
# Neural Network Model
install.packages("neuralnet")
library(neuralnet)

n <- neuralnet(maxtempC~.,
               data = testing,
               hidden =4,
               err.fct ="sse",
               linear.output = T)
n
plot(n)
# Prediction
output <-compute(n, training[,-1])
head(output$net.result)
head(training[1,])
# Prediction
output <-compute(n, training[,-1])
p1 <- output$net.result
p1
pred1 <- ifelse(p1 >0.5,1,0)
tab1
1-sum(diag(tab1))/sum(tab1)
output <- compute(n, testing[,-1])
p2 <- output$net.result
pred2 <- ifelse(p2>0.5,1,0)
tab2 <- table(pred2, testing$admit)
tab2
1-sum(diag(tab2))/sum(tab2)

