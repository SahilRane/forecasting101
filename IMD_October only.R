data <- read.csv("weather_data_24hr_master1.csv", header = T)
data = data[-4265,]
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
set.seed(1000)
ind <- sample(2,nrow(data), replace = T, prob =c(0.7,0.3))
training <- data[ind==1,]

n <- neuralnet(maxtempC~maxlag1+minlag1+avglag1,
               data = training, stepmax = 100000,
               hidden = 3,
               err.fct ="sse",
               linear.output = T)




data <- read.csv("weather_is_bad.csv", header = T)
data <- data[-342,]
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
set.seed(1000)
ind <- sample(2,nrow(data), replace = T, prob =c(0.6,0.4))
training <- data[ind==1,]
testing <- data[ind==2,]
install.packages("neuralnet")
library(neuralnet)
n <- neuralnet(maxtempC~maxlag1+minlag1+avglag1,
               data = training, stepmax = 10000000,
               hidden = 6,
               err.fct ="sse",
               linear.output = T)
plot(n)
n
output <-compute(n, training)
p1 <- output$net.result
sum((training$maxtempC-p1)^2)/nrow(training)
max((training$maxtempC-p1))
output <- compute(n, testing)
p2 <- output$net.result
sum((testing$maxtempC-p2)^2)/nrow(testing)

