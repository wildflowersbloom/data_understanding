rm(list = ls())
library(dplyr)
library(ggplot2)
library(lubridate)
library(autoTS)
library(plotly)
library(GGally)

#load and clean data, select relevant vars
data<-readRDS(file = "./data/dat_clean.rds") #data_clean variable from ZZ2-data_preparation.R
data<-data%>%filter(is_bike) %>% #cyclocross and road_biking are few data points
  filter(activityType%in%c("cycling"))%>%#,"indoor_cycling","virtual_ride"))%>%
  droplevels()   

data<-data%>% mutate(year=year(data$start_time),
                    month=month(data$start_time),
                    weekday=wday(data$start_time,week_start = 1),
                    week=week(data$start_time),
                    hour=hour(data$start_time))

data<-data%>% select(activityType,#what
                     year,month,week,weekday,hour,start_time,date, #when
                     duration, distance, avgSpeed, maxSpeed,maxVerticalSpeed, #how long, far,fast
                     #locationName,startLongitude,startLatitude,#where
                     #minElevation,#maxElevation, elevation 
                     elevationGain,#elevationLoss, #elevation change, gain~loss
                     avgHr,#maxHr, #heart rate
                     avgPower,max20MinPower,#power
                     avgBikeCadence,#maxBikeCadence,#cadence
                     calories,#energy
                     minTemperature)#,maxTemperature)#temp

#clear some anomalous data
#duration of events should be at least 10min
data<-filter(data,duration>10)
#indoor_cycling often missing values for distance, speed, and obviously elevation
data<-filter(data,!(is.na(avgSpeed)| avgSpeed<5)) 
data<-filter(data,!(maxSpeed>120))
data<-filter(data,!(maxSpeed==0))


ggplot(data,aes(y=distance,x=duration/60,color=avgSpeed))+geom_point()+theme_minimal()+facet_grid(.~activityType) #remove datapoints with likely measurement error, for distance, duration or avgSpeed
ggplot(data,aes(x=avgSpeed))+geom_histogram()+theme_minimal()+facet_grid(.~activityType) 
#cycling avgSpeed is bimodal (different bike types, possibly)

# correlogram
library(corrgram)
corrgram(select(data,-c(activityType,year,month, week,weekday,hour)),
         order=T, lower.panel=panel.shade,
         upper.panel=NULL)
#duration,distance and calories, are positively correlated, as well as avgPower and max20MinPower,
#avgHr and Power are moderately correlated
#avgPower and temperature are moderately negatively linked. 

#ggpairs(select(data,duration,distance, avgSpeed, maxSpeed, maxVerticalSpeed))

#plots
ggplot(data,aes(avgSpeed))+geom_histogram()+theme_minimal()+facet_grid(.~activityType)
ggplotly(ggplot(data,aes(duration/60,x=hour))+geom_point()+theme_minimal())
ggplotly(ggplot(data,aes(x=hour))+geom_bar()+theme_minimal())
#favorite start time at 7, 12-13 never start, lunchtime, and again 17-18 (back from work) 
ggplotly(ggplot(data,aes(x=weekday))+geom_bar()+theme_minimal())
ggplotly(ggplot(filter(data,duration>180),aes(x=weekday))+geom_bar()+theme_minimal())
#the longest rides take place on  the weekend
ggplotly(ggplot(data,aes(duration,x=month))+geom_point()+theme_minimal())
#in the winter months(nov-feb) rides are <3h
ggplotly(ggplot(data,aes(x=month))+geom_bar()+theme_minimal())

ggplot(data,aes(avgSpeed,x=date))+geom_point()+theme_minimal()+geom_smooth(method="lm",se=F)
ggplot(data,aes(maxSpeed,x=date))+geom_point()+theme_minimal()+geom_smooth(method="lm",se=F)
#there is an upward trend in maxSpeed
ggplot(data,aes(duration/60,x=month))+geom_point()+theme_minimal()+facet_grid(.~activityType)

#timeseries analysis

#one duplicated event! row 1371 "2016-11-01 14:52:50"
data<-data[-1371,]

#smaller dataframe to work with, select relevant variables
data<-select(data,activityType,year,month,week,start_time,duration,distance,avgSpeed,maxSpeed, avgPower)%>%
  filter(activityType%in%c("cycling"))

#aggregate into one value per month
mdata <- data %>% mutate(time =  as.Date(cut(start_time, "month")))%>%
  group_by(time) %>%
  summarise(mduration = sum(duration,na.rm=T),
            endurance = max(duration,na.rm=T), 
         distance = sum(distance,na.rm=T),
         avgSpeed = mean(avgSpeed,na.rm=T),
         maxSpeed = max(maxSpeed,na.rm=T),
         avgPower =mean(avgSpeed,na.rm=T))

mdata<-mdata %>% #add NA  rows in missing months
  tidyr::complete(time = seq.Date(min(time), max(time), by = "month"))
library(imputeTS)
mdata<-na_interpolation(mdata)  #impute those NA rows with interpolation
ggplot(mdata,aes(maxSpeed,x=time))+geom_line()+geom_point()+theme_minimal()+geom_smooth(method="lm",se=F)

ggplotly(ggplot(mdata,aes(mduration,x=time))+geom_line()+geom_point()+theme_minimal())
ggplotly(ggplot(mdata,aes(endurance,x=time))+geom_line()+geom_point()+theme_minimal())
ggplotly(ggplot(mdata,aes(distance,x=time))+geom_line()+geom_point()+theme_minimal())
ggplotly(ggplot(mdata,aes(avgSpeed,x=time))+geom_line()+geom_point()+theme_minimal())
ggplotly(ggplot(mdata,aes(maxSpeed,x=time))+geom_line()+geom_point()+theme_minimal())
ggplotly(ggplot(mdata,aes(avgPower,x=time))+geom_line()+geom_point()+theme_minimal())

#aggregate into one value per week
wdata <- data %>% mutate(time =  as.Date(cut(start_time, "week")))%>%
  group_by(time) %>%
  summarise(mduration = sum(duration,na.rm=T),
            endurance = max(duration,na.rm=T),
            distance = sum(distance,na.rm=T),
            avgSpeed = mean(avgSpeed,na.rm=T),
            maxSpeed = max(maxSpeed,na.rm=T))


wdata<-wdata %>% #add NA rows in missing weeks
  tidyr::complete(time = seq.Date(min(time), max(time), by = "week"))
wdata<-na_interpolation(wdata)  #impute those NA rows with interpolation

ggplotly(ggplot(wdata,aes(mduration,x=time))+geom_line()+geom_point()+theme_minimal())
ggplotly(ggplot(wdata,aes(endurance,x=time))+geom_line()+geom_point()+theme_minimal())
ggplotly(ggplot(wdata,aes(distance,x=time))+geom_line()+geom_point()+theme_minimal())
ggplotly(ggplot(wdata,aes(avgSpeed,x=time))+geom_line()+geom_point()+theme_minimal())
ggplotly(ggplot(wdata,aes(maxSpeed,x=time))+geom_line()+geom_point()+theme_minimal())

library(zoo) 
ts_duration <- zoo(mdata$mduration, mdata$time) 
plot(ts_duration)
ts_endurance <- zoo(mdata$endurance, mdata$time) 
plot(ts_endurance)
ts_distance <- zoo(mdata$distance, mdata$time)
plot(ts_distance)
ts_maxSpeed <- zoo(mdata$maxSpeed, mdata$time) 
plot(ts_maxSpeed)
ts_avgSpeed <- zoo(mdata$avgSpeed, mdata$time) 
plot(ts_avgSpeed)

ts_endurance = ts(mdata$endurance, start = c(2012,1), frequency = 12)
components.ts = decompose(ts_endurance) #to see clear seasonal component #endurance, is max duration
plot(components.ts)

ts_maxSpeed = ts(mdata$maxSpeed, start = c(2012,1), frequency = 12)
components.ts = decompose(ts_maxSpeed) #to see clear seasonal component #endurance, is max duration
plot(components.ts)



#The autocorrelation function is an important tool for revealing the interrelationships within a time series. It is a collection of correlations,  ρ_k for k = 1, 2, 3, …, where  ρ_k is the correlation between all pairs of data points that are exactly k steps apart.

#Visualizing the autocorrelations is useful, below the autocorrelations for each value of k. 
acf(ts_endurance,na.action=na.pass)
acf(ts_maxSpeed,na.action=na.pass)
#The presence of autocorrelations is one indication that an autoregressive integrated moving average (ARIMA) model could model the time series. From the ACF, you can count the number of significant autocorrelations, which is a useful estimate of the number of moving average (MA) coefficients in the model. the figure shows twelve significant autocorrelations, so we estimate that its ARIMA model will require seven MA coefficients (MA(12)). That estimate is just a starting point, however, and must be verified by fitting and diagnosing the model.

#The partial autocorrelation function (pacf) is another tool for revealing the interrelationships in a time series. The partial autocorrelation at lag k is the correlation between all data points that are exactly k steps apart—after accounting for their correlation with the data between those k steps. The practical value of a PACF is that it helps you to identify the number of autoregression (AR) coefficients in an ARIMA model. There are four significant values, at k = 1-4, so our initial ARIMA model will have four AR coefficients (AR(4))
pacf(ts_endurance,na.action=na.pass)
pacf(ts_maxSpeed,na.action=na.pass)

#detrending, not much of a trend
m <- lm(coredata(ts_endurance) ~ index(ts_endurance))
autoplot(ts_endurance)
detr <- zoo(resid(m), index(ts_endurance))
autoplot(detr)

m <- lm(coredata(ts_maxSpeed) ~ index(ts_maxSpeed))
autoplot(ts_maxSpeed)
detr <- zoo(resid(m), index(ts_maxSpeed))
autoplot(detr)


#seasonal plot
ggseasonplot(ts_endurance, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("endurance") +
  ggtitle("Seasonal plot")+theme_minimal()

#seasonal plot polar
ggseasonplot(ts_endurance,polar=TRUE) +
  ylab("endurance") +
  ggtitle("Seasonal plot")+theme_minimal()

#naive forecasts
autoplot(ts_endurance) +
  autolayer(meanf(ts_endurance, h=20),
            series="Mean", PI=FALSE) +
  autolayer(naive(ts_endurance, h=20),
            series="Naïve", PI=FALSE) +
  autolayer(snaive(ts_endurance, h=20),
            series="Seasonal naïve", PI=FALSE) +
  autolayer(rwf(ts_endurance, drift=TRUE, h=20),
            series="Drift", PI=FALSE) +
  ggtitle("Forecasts for monthly endurance") +
  xlab("Year") + ylab("Max cycling duration") +
  guides(colour=guide_legend(title="Forecast"))

autoplot(ts_maxSpeed) +
  autolayer(meanf(ts_maxSpeed, h=20),
            series="Mean", PI=FALSE) +
  autolayer(naive(ts_maxSpeed, h=20),
            series="Naïve", PI=FALSE) +
  autolayer(snaive(ts_maxSpeed, h=20),
            series="Seasonal naïve", PI=FALSE) +
  autolayer(rwf(ts_maxSpeed, drift=TRUE, h=20),
            series="Drift", PI=FALSE) +
  ggtitle("Forecasts for maximum speed") +
  xlab("Year") + ylab("Max cycling speed") +
  guides(colour=guide_legend(title="Forecast"))

fc<-snaive(ts_maxSpeed)
autoplot(fc)
res <- residuals(fc)
autoplot(res)
checkresiduals(res)

#fit an arima
library(forecast)
m=auto.arima(ts_maxSpeed)

myts<-ts_maxSpeed
#
myts.train <- window(myts, end=c(2018,12))
myts.test <- window(myts, start=2019)

autoplot(myts) +
  autolayer(myts.train, series="Training") +
  autolayer(myts.test, series="Test")

fc <- snaive(myts.train)
accuracy(fc,myts.test)
checkresiduals(fc)

myts %>% decompose(type="multiplicative") %>%
  autoplot() + xlab("Year") +
  ggtitle("Classical multiplicative decomposition
    of max cycling speed")

myts %>% seas(x11="") -> fit
autoplot(myts, series="Data") +
  autolayer(trendcycle(fit), series="Trend") +
  autolayer(seasadj(fit), series="Seasonally Adjusted") +
  xlab("Year") + ylab("New orders index") +
  ggtitle(" Max cycling speed") +
  scale_colour_manual(values=c("gray","blue","red"),
                      breaks=c("Data","Seasonally Adjusted","Trend"))


#stationary, time series with trends, or seasonality are not stationary
Box.test(diff(ts_endurance), type="Ljung-Box")
acf(diff(ts_endurance)) 
Box.test(diff(ts_maxSpeed), type="Ljung-Box")
acf(diff(ts_maxSpeed))

myts<-ts_endurance
#differencing
cbind("TS" =myts,
      "Logs" = log(myts),
      "Seasonally\n differenced logs" =
        diff(log(myts),12),
      "Doubly\n differenced logs" =
        diff(diff(log(myts),12),1)) %>%
  autoplot(facets=TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Endurance")


#ARIMA (2,1,1) In this case, auto.arima decided the best order was (2, 1, 1), which means that it differenced the data twice  (d = 2) before selecting a model with AR coefficients (p = 1) and zero MA coefficients (q = 1).

ggAcf(ts_maxSpeed)
ggPacf(ts_maxSpeed)



ts_maxSpeed %>% diff(lag=6) %>% diff() %>% ggtsdisplay()

m1<-auto.arima(ts_maxSpeed) #Arima, (2,1,1)AIC 833
checkresiduals(m1)
m2 <- Arima(ts_maxSpeed, order=c(2,1,1), seasonal=c(0,1,1))#768
checkresiduals(m2)
m3 <- Arima(ts_maxSpeed, order=c(0,1,2), seasonal=c(0,1,1))#766
checkresiduals(m3)

m1%>% forecast(h=12) %>% autoplot()+theme_minimal()

#check this out, super useful
####https://otexts.com/fpp2/seasonal-arima.html
