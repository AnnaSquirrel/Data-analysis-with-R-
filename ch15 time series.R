#ch15 time series 
myseries <- ts(data, start=, end=, frequency=) 
#1.generate ts 
sales <- c(18, 33, 41, 7, 34, 35, 24, 25, 24, 21, 25, 20, 
            22, 31, 40, 29, 25, 21, 22, 54, 31, 25, 26, 35)
tsales <- ts(scales,start=c(2003,1),frequency = 12)
tsales 

#centered moving average  
install.packages("forecast")
library(forecast)
opar <- par(no.readonly = TRUE) 
par(mfrow=c(2,2)) 
ylim <- c(min(Nile),max(Nile)) 
plot(Nile,main="Raw ts") 
plot(ma(Nile,3),main="simple moving average(k=3)",ylim = ylim) 
plot(ma(Nile,7),main="simple moving average(k=7)",ylim = ylim) 
plot(ma(Nile,15),main="simple moving average(k=15)",ylim = ylim) 
par(opar) 

#seasonly decomposition slt() 
plot(AirPassengers)
lAirPassengers <- log(AirPassengers)
plot(lAirPassengers,ylab="log(AirPassengers)")
fit <- stl(lAirPassengers,s.window="period") 
plot(fit) 
fit$time.series 
exp(fit$time.series) 
#visualization 
par(mfrow=c(2,1)) 
monthplot(AirPassengers,xlab="",ylab="")
seasonplot(AirPassengers,year.labels = "TRUE",main="") 


#3.exponential model 
library(forecast) 
ets(ts,model="") 
fit <- ets(nhtemp, model="ANN") 
#with seasons 
fit <- ets(log(AirPassengers),model="AAA") 
fit



