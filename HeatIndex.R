data<-read.csv("HeatIndexOctober.csv", header = T)
data<-data[c(-32,-64,-96,-128,-160,-192,-224,-256,-288),]
hist(data$Heat.Index.in.Celcius)
mean(data$Heat.Index.in.Celcius, na.rm = T)
sd(data$Heat.Index.in.Celcius, na.rm = T)
plot(density(data$Heat.Index.in.Celcius))
table(data$Heat.Index.in.Celcius)




