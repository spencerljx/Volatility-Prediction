library(tseries)
library(lmtest)
library(forecast)

data_set=read.csv('State_Zhvi_AllHomes.csv',header=T,sep=',')

ny_data = (t(data_set[3,-(1:5)]))[1:252,ncol=1]
data = (t(data_set[3,-(1:5)]))[1:291,ncol=1]
plot(data)

## choose a trend
TIME = c(1:252)
TIME2 = TIME*TIME
TIME3=TIME2*TIME

result1 =lm(ny_data ~ TIME)
result2 =lm(ny_data ~ TIME + TIME2)
result3=lm(ny_data~TIME+TIME2+TIME3)
result4=lm(ny_data~log(TIME))

summary(result1)
summary(result2)
summary(result3)
summary(result4)

BIC(result1)
BIC(result2)
BIC(result3)
BIC(result4)

## detrend
ny_data.detrend = result3$residuals


## fit with arma
house.ar1=arma(ny_data.detrend,order=c(1,0))
house.ma1=arma(ny_data.detrend,order=c(0,1),method="CG")
house.arma11=arma(ny_data.detrend,order=c(1,1))

##choose a model 
print(summary(house.ar1)$aic) 
print(summary(house.ma1)$aic)
print(summary(house.arma11)$aic)

##forecast
time=c(253:291)
time2=time*time
time3 = time2*time


#ar1
house.ar1=Arima(ny_data.detrend,order=c(1,0,0),method="CSS")
fcst=forecast.Arima(house.ar1,h=39)
#trend
trend=matrix(NA,39,1)
for(i in 1:39)
{
  trend[i]=result3$coef[1]+result3$coef[2]*time[i]+result3$coef[3]*time2[i]+result3$coef[4]*time3[i]
}
#with trend and arimafcst
fcstdata=fcst$mean[1:39]+trend

#with white noise
whitenoise=rnorm(39,mean=0,sd=sd(ny_data.detrend))
fcstdata1=fcst$mean[1:39]+trend+whitenoise

#one replacement
fcst2=rep(house.ar1$coef[2],39)+house.ar1$coef[1]*data[252:290]
fcstdata2=trend+fcst2

#plot the lines of real and fsct
datav2=ts(data)[253:291]
plot(datav2,type="l",xlab="time",xlim=c(1,47),col="red")
par(new=T)
plot(fcstdata,type="l",axes=F,xlab="",ylab="",main='Forecast',xlim=c(1,47),col="green")
par(new=T)
plot(fcstdata1,type="l",axes=F,xlab="",ylab="",main='Forecast',xlim=c(1,47),col="blue")
par(new=T)
plot(fcstdata2,type="l",axes=F,xlab="",ylab="",main='Forecast',xlim=c(1,47),col="black")
box()
legend(x='topright',y=null,c("Actual","Forecast","Forecast1","Forecast2"),lty=c(1,1),col=c("red","green","blue","black"),cex=0.9)
