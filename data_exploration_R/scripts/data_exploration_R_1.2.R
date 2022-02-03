library(tidyverse)
library(jsonlite)
## Read, clean and parse json
datapath<-"./data/summarizedActivities.json"
rawdat<-fromJSON(datapath,flatten = F, simplifyDataFrame = F, simplifyVector = F,simplifyMatrix = F)

rawdat<-rawdat[[1]] #get rid of first 
rawdat<-rawdat[[1]] #and second level

# Get metrics in a vector
listnames<-lapply(rawdat,names) %>% unlist() %>% unique()

# Figure out how to extract one specific metric for one activity
function_extract<-function(activity, metric){
  if (is.null(activity[[metric]])) return(NA)
  else return(activity[[metric]])
}

function_extract(rawdat[[1]],'duration')
rawdat[[1]]['duration']
function_extract(rawdat[[1]],'avgPower')
rawdat[[1]]['avgPower']

lapply(rawdat,function_extract)

# My solution also kinda worked..
#rawdat<-fromJSON(datapath)
#flatdat<-jsonlite::flatten(rawdat,recursive = T)
#dat<-as.data.frame(flatdat$summarizedActivitiesExport[[1]])
#names(dat)

# Start from already-cleaned csv file
dat <- read.csv("./data/Activities_2022.csv",header = T)


# The start time is miliseconds from a specific date,  Sunday, Dec 31st, 1989
#https://www.epochconverter.com/?TimeStamp=1642187654
lubridate::as_datetime(dat$startTimeGmt)



dat<-dat%>% mutate( startTimeGmt= as_datetime(startTimeGmt/1000), #create timestamp
                    date=lubridate ::floor_date(startTimeGmt,"day")) #round to the day

dat<-dat%>% mutate(across(c(duration,contains("Duration")), #to minutes
              function(xx) time_length(xx/1000,"minutes")))
                   
                    
dat<-dat%>% mutate(is_bike = ifelse(activityType %in%
                 c("cycling","virtual_ride","indoor_cycling","road_biking","cyclocross"),T,F),
                    is_run = str_detect(activityType,"running|hicking"),
                    activity_recoded = case_when(is_bike ~ "Bike",
                             is_run ~ "Run",
                             str_detect(activityType,"swim") ~"Swim",
                             TRUE ~ "Other")) 

dat<-dat%>% mutate(across(c(contains("Id"),contains("uuid")),
                as.character)) 
        
dat<-dat%>% mutate( startTimeLocal= as_datetime(startTimeLocal/1000))

dat<-dat%>% mutate( distance= distance/1E5, calories=calories/4.184) #convert m to km, and joules to cal
dat<-dat%>% mutate( across(contains("elevation"),function(xx) xx/1E2)) # to m
dat<-dat%>% mutate( across(contains("speed"),function(xx) xx*36))  #to km/h


dat<-dat%>% mutate(qual_distance=as.character(cut(distance,
                                      quantile(distance,probs = 0:5/5,na.rm=T),
                                      include.lowest = T,
                                      labels=c("Very short","Short",
                                               "Average","Long","Very long"))),
       qual_avgHr=as.character(cut(avgHr,quantile(avgHr,0:3/5,na.rm = T),
                                   include.lowest = T,
                                   labels=c("Low intensity","Average intensity",
                                            "High intensity"))),
       qual_distance=ifelse(is.na(qual_distance),"Very short",qual_distance))

#checks
select(dat,contains("distance"),contains("activity"))%>%
  group_by(activityType) %>%
  summarise(across(where(is.numeric),function(xx) range(xx,na.rm=T)))

select(dat,contains("elevation"),contains("activity"))%>%
  group_by(activityType) %>%
  summarise(across(where(is.numeric),function(xx) range(xx,na.rm=T)))

select(dat,contains("speed"),contains("activity"))%>%
  group_by(activityType) %>%
  summarise(across(where(is.numeric),function(xx) range(xx,na.rm=T)))

select(dat,contains("calories"),contains("activity"))%>%
  group_by(activityType) %>%
  summarise(across(where(is.numeric),function(xx) range(xx,na.rm=T)))

select(dat,contains("duration"),contains("activity"))%>%
  group_by(activityType) %>%
  summarise(across(where(is.numeric),function(xx) range(xx,na.rm=T)))



dat %>%
  group_by(activityType) %>%
  summarize(count = n(), avgDistance = round(mean(distance),0))

sd(dat$avgPower,na.rm = T)/mean(dat$avgPower,na.rm = T)
sd(dat$distance,na.rm = T)/mean(dat$distance,na.rm = T)

quantile(dat$duration,probs=0:20/20,na.rm = T)
quantile(dat$duration,probs=c(.2, .95),na.rm = T)


dat_clean <- filter(dat,!(activityId %in% c(407226313,2321338)) & year(date)>=2012)


#Is there more dispersion in the distance or the average power ? using the facet_wrap function of ggplot2, compare the distributions of distance and avgPower.

subdat<-subset(dat_clean,activity_recoded=="Bike",select=c(distance,avgPower,activity_recoded))
pdat<-subdat%>%pivot_longer(!activity_recoded, names_to = "metric",values_to="value")

ggplot(pdat,aes(value)) + geom_histogram() +theme_minimal()+
  facet_wrap(.~metric,scales="free")

ggplot(pdat,aes(value,fill=metric)) + geom_density()+ theme_minimal()
  

#discretization (1. based on quantiles, 2. based on mean and sd, 3. fisher jenks natural breaks method for heavily skewed)


dat <- mutate(dat_clean,qual_distance=as.character(cut(distance,
                                                 quantile(distance,probs = 0:5/5,na.rm=T),
                                                 include.lowest = T,
                                                 labels=c("Very short","Short",
                                                          "Average","Long","Very long"))),
              qual_avgHr=as.character(cut(avgHr,quantile(avgHr,0:3/5,na.rm = T),
                                          include.lowest = T,
                                          labels=c("Low intensity","Average intensity",
                                                   "High intensity"))),
              qual_distance=ifelse(is.na(qual_distance),"Very short",qual_distance))


#categorical variables
ggplot(dat,aes(activityType))+geom_bar()+coord_flip()

table(dat$activity_recoded,dat$qual_distance) 
table(dat$activity_recoded,dat$qual_distance) %>%   
  prop.table()*100
table(dat$activity_recoded,dat$qual_distance) %>%   
  prop.table(margin=1)*100
table(dat$activity_recoded,dat$qual_distance) %>%   
  prop.table(margin=2)*100

#install.packages("lsr") # If not installed
table(dat$activity_recoded,dat$qual_distance) %>% 
  chisq.test() # significance
table(dat$activity_recoded,dat$qual_distance) %>% 
  lsr::cramersV() #[0,1], compare across different tables


#Exercises
#Explore the distribution of the average speed. What can you say about it ?
dat<-dat[dat$avgSpeed<100,]
dat<-dat[!is.na(dat$avgSpeed),]
ggplot(dat,aes(avgSpeed)) + geom_histogram() +theme_minimal()+facet_wrap(as.factor(dat$activity_recoded),scales="free")
dim(dat[is.na(dat$avgSpeed),])

#Explore the correlation between average speed and average power
ndat<-filter(dat,avgSpeed>0)
sapply(c("pearson","spearman","kendall"),function(xx) cor(ndat$avgSpeed,ndat$avgPower,method=xx,use = "complete.obs"))
ggplot(ndat,aes(avgSpeed,avgPower))+geom_point()+geom_smooth(method="lm")

#For all the the categorical variables, get the frequent category (with table AND dplyr/tidyr)

get_most_frequent<-function(xx){
  res<-table(xx)%>%
  which.max()%>%
  names()
  return(res)
}
get_most_frequent(dat$activityType)


get_most_frequent_tidy<-function(nn){
group_by(dat,across(nn)) %>%
  summarise(n_obs=n()) %>% 
  top_n(n=1,n_obs)%>%
  pull(nn)
}
get_most_frequent_tidy("activityType")
#------------excercises.
# Kolmogorov smirnov test of distance
norm_d<-dat$distance #get vector, only distance
norm_d<-norm_d[norm_d>0] #no zero distances
norm_d<-norm_d[!is.na(norm_d)]
log_d<-log10(norm_d)

ks.test(norm_d,"pnorm", mean=mean(norm_d),sd=sd(norm_d))
ks.test(norm_d,"pnorm") #don't know what it uses as default, but yields different results!

ggplot(dat, aes(distance))+geom_histogram()+theme_classic()
ggplot(dat, aes(distance))+geom_histogram()+scale_x_log10()+theme_classic()

range(norm_d)
range(log_d) #with log transform you get INF

samplesize=5e3
fake_d<-rnorm(samplesize)
ks.test(norm_d,fake_d)
ks.test(log_d,"pnorm")

x <- rnorm(50)
x2 <- rnorm(50, -1)
plot(ecdf(x), xlim = range(c(x, x2)))
plot(ecdf(x2), add = TRUE, lty = "dashed")
t.test(x, x2, alternative = "g")
wilcox.test(x, x2, alternative = "g")
ks.test(x, x2, alternative = "l")


##root test, null=not stationary
library(tseries)
# generation of a random walk
set.seed(2)
Xt <- cumsum(rnorm(100))
series <- data.frame(Xt=Xt) %>% 
  mutate(index=row_number())
ggplot(series,aes(index,Xt)) + geom_line() + theme_minimal() +
  labs(title="A random walk")
pp.test(Xt)


