library(dplyr)
library(ggplot2)
library(lubridate)
library(tseries)
library(forecast)
library(TTR)




#dataset
inspections<-read.csv("/home/vogiral/Desktop/Data Analytics Project 2018/inspections.csv")
violations<-read.csv("/home/vogiral/Desktop/Data Analytics Project 2018/violations.csv")
copy<-read.csv("/home/vogiral/Desktop/Data Analytics Project 2018/all.csv")
all<-inspections %>%
  inner_join(violations,by="serial_number") #joining noth the datasets

#cleaning the data
zip<-all$facility_zip
all$facility_zip<-substr(zip,start = 1, stop = 5)

#extracting data from 2016 and 2017
all <- all[ymd(all$activity_date)>="2016-01-01",]
a<-unique(copy$facility_city)
all<-all[all$facility_city %in% a,] #collecting data from 15 cities

all<-lapply(all, function(x) if(is.factor(x)) factor(x) else x) #formatting the levels of all attributes in the dateset
all$activity_date<-as.Date(all$activity_date)

write.csv(all,file="/home/vogiral/Desktop/Data Analytics Project 2018/all.csv")

#frequency table
counts<-table(all$activity_date)
counts<-ts(counts)

plot(counts,type = "l",col="#3A5795",main="Time Series plot for Restaurant Violation data",xlab = "Date",ylab = "Violation Count")


train_set <- window(counts,start=1,end=400)
test_set  <- window(counts,start=401,end=500)


plot(counts,type = "l",col="#3A5795",main="Time Series plot for Restaurant Violation data",xlab = "Date",ylab = "Violation Count")
lines(meanf(train_set,h=48)$mean,col=4)
lines(rwf(train_set,h=48)$mean,col=2)
lines(rwf(train_set,drift = TRUE,h=48)$mean,col=3)
lines(snaive(train_set,h=48)$mean,col=5)

accuracy(meanf(train_set,h=48), test_set)
accuracy(rwf(train_set,h=48), test_set)
accuracy(rwf(train_set,drift = TRUE,h=48), test_set)
accuracy(snaive(train_set,h=48), test_set)

train_set.mean <- meanf(train_set,h=48)$mean
train_set.naive <- rwf(train_set,h=48)$mean

plot(test_set,type = "l",col="#3A5795",main="Time Series plot for Restaurant Violation data",xlab = "Date",ylab = "Violation Count")
lines(train_set.mean,col=4)
lines(train_set.naive,col=3)
plot(train_set.mean)


sr.naive <- rwf(sr,h=48)$mean
sr.drift <- rwf(sr,drift=TRUE,h=48)$mean
sr.seas <- snaive(sr,h=48)$mean



#Test for stationarity
adf.test(counts,alternative = "stationary")

(acf(counts))
(pacf(counts))

arma<-auto.arima(counts)
summary(arma)

#forecasting violations for 20 days
arma_forecast <-forecast(arma,h=10)$mean
plot(arma_forecast)
plot(test_set,col="red")

plot(test_set,type = "l",col="#3A5795",main="Time Series plot for Restaurant Violation data",xlab = "Date",ylab = "Violation Count")

plot(arma_forecast,xlab = "Days", ylab="Counts")
plot.ts(arma_forecast$residuals,type = "p",main='Residual Plot',xlab="Time",ylab="Residuals")
abline(a=0,b=0,col='blue')

qqnorm(arma_forecast$residuals)
acf(arma_forecast$residuals)

plot(counts,col="green",type="l",main="Fitted vs Observed",xlab="Days",ylab="Counts")
lines(arma_forecast$fitted,col="blue")

plot(arma_forecast$fitted,arma_forecast$residuals,ylab="Residuals",xlab="Fitted",main="Fitted vs Residuals",col="red")
abline(a=0,b=0,col='blue')


counts<-table(all$activity_date)
counts<-as.data.frame(counts)
counts<-window(counts,start=2016)




#exponential smoothing method of analysis
e_forecasts<-HoltWinters(counts,beta=FALSE, gamma=FALSE)
e_forecasts$fitted

plot(counts,col="green",type="l",main="Fitted vs Observed",xlab="Days",ylab="Counts")
plot(e_forecasts)

exp_s<- es(counts)
















