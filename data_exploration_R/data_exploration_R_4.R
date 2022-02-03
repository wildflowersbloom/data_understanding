#Linear regression and logistic regression
#Clear environment
rm(list = ls())
dat_bike<-readRDS(file = "dat_bike.rds")
dat_bike$activityType<-droplevels(dat_bike$activityType)

# Engineer a new variable
dat_bike<-dat_bike%>%
mutate(mymonth =format(date,"%m"),
  mymonthstr =format(date,"%B"),
       myweek =format(date,"%W"),
  myweekday=as.character(lubridate::wday(date,label=T)))

#lubridate::week(dat_bike$date)
#lubridate::week(dat_bike$date)

summ<-dat_bike%>%group_by(myweek)%>%summarise(meanSpeed=mean(avgSpeed,na.rm=T))
ggplot(summ, aes(myweek, y=meanSpeed))+geom_point()
summ<-dat_bike%>%group_by(mymonth)%>%summarise(meanSpeed=mean(avgSpeed,na.rm=T))
ggplot(summ, aes(mymonth, y=meanSpeed))+geom_point()
summ<-dat_bike%>%group_by(myweekday)%>%summarise(meanSpeed=mean(avgSpeed,na.rm=T))
ggplot(summ, aes(myweekday, y=meanSpeed))+geom_point()

#how to integrate month into model
reg<-lm(data=dat_bike,formula=avgSpeed~myweek+avgPower+avgBikeCadence+distance+avgHr+max20MinPower)
summary(reg)

reg<-lm(data=dat_bike,
        formula=avgSpeed~avgPower+avgBikeCadence+distance+max20MinPower+mymonth+myweekday)
summary(reg)

#exercise- regress for indoor bike activities
idat_bike<-filter(dat_bike,activityType%in%c("indoor_cycling","virtual_ride"))
#add a missing important variable
reg <- lm(avgSpeed~avgPower + avgBikeCadence + distance + avgHr + max20MinPower + elevationGain, data=dat_bike)
summary(reg)

#add interactions between regressors (predictors) and exponents to them
reg <- lm(avgSpeed~ avgPower + I(avgPower^2) + avgBikeCadence + 
            distance + avgPower*distance + max20MinPower , data=dat_bike)


#number of weeks after training started (w=8)
dat_bike<-dat_bike%>%
  mutate(weekstraining = ifelse(as.numeric(myweek)<8,0,as.numeric(myweek)-8))

summ<-dat_bike%>%group_by(weekstraining)%>%summarise(meanSpeed=mean(avgSpeed,na.rm=T))
ggplot(summ, aes(weekstraining, y=meanSpeed))+geom_point()

reg <- lm(avgSpeed~ avgPower  + avgBikeCadence + weekstraining+
            distance + avgPower*distance + max20MinPower , data=dat_bike)

summary(reg)
hist(residuals(reg))

#logistic
rm(list = ls())
dat_clean<-readRDS(file = "dat_clean.rds")
logit<-glm(is_bike~distance+duration+elevationGain+avgSpeed+avgHr,data=dat_clean,family="binomial")
summary(logit)

logit$coefficients%>%exp()

#goodness of fit 
pred<-predict(logit, dat_clean,type="response")
pred_bin<-as.numeric(pred>.5)
table(pred_bin,dat_clean$is_bike)

#exercises------
rm(list = ls())

performance_func<-function (confusion_matrix)
{Tab<-confusion_matrix
Recall<-Tab[2,2]/(Tab[2,2]+Tab[1,2])
Specificity<-Tab[1,1]/(Tab[1,1]+Tab[2,1])
Precision<-Tab[2,2]/(Tab[2,2]+Tab[2,1])
string<-cat(paste("Recall",Recall,
                  "\nSpecificity",Specificity,
                  "\nPrecision",Precision))
return(string)}  

dat_clean<-readRDS(file = "dat_clean.rds")
#dplyr::slice(dat_clean,c(1914,1292,3493))

#summary(dat_clean)
reg <- lm(avgSpeed~distance+ duration * activityType, data=dat_clean)
summary(reg)
hist(residuals(reg))
#plot(reg)

dat_zeros<-dat_clean
dat_zeros[dat_zeros$activity_recoded=="Swim",]$elevationGain=0
dat_zeros[dat_zeros$activity_recoded=="Other",]$elevationGain=0

logit<-glm(is_run~distance+avgSpeed+avgHr,data=dat_clean,family="binomial")
summary(logit)
pred<-predict(logit, dat_clean,type="response")
pred_bin<-as.numeric(pred>.5)
CM<-table(pred_bin,dat_clean$is_run)
performance_func(CM)


#using elevationGain, with zeros in Swim and Other doesn't help
logit<-glm(is_run~distance+avgSpeed+avgHr+elevationGain,data=dat_zeros,family="binomial")
summary(logit)
pred<-predict(logit, dat_zeros,type="response")
pred_bin<-as.numeric(pred>.5)
CM<-table(pred_bin,dat_clean$is_run)
performance_func(CM)

logit<-glm(is_run~distance+avgSpeed*avgHr,data=dat_clean,family="binomial")
summary(logit)
pred<-predict(logit, dat_clean,type="response")
pred_bin<-as.numeric(pred>.5)
CM<-table(pred_bin,dat_clean$is_run)
performance_func(CM)

require(rpart)
require(visNetwork)

tree<-rpart(activityType~distance+avgSpeed,data=dat_clean)
visTree(tree)
