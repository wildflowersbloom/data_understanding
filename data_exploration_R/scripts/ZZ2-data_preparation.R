require(tidyverse)
require(lubridate)

# Start from already-cleaned csv file
dat <- read.csv("./data/Activities_2022.csv",header = T)
#dat <- read.csv("Data/Sports/Activities_2022.csv",header = T)

dat <- dat %>% 
  mutate(start_time=as_datetime(startTimeLocal/1000), # create a timestamp
         date = floor_date(start_time,"day"), # round to the day
         is_bike=ifelse(activityType %in%
                          c("cycling","virtual_ride","indoor_cycling","road_biking","cyclocross"),T,F),
         # is it bike or not ?
         is_run = str_detect(activityType,"running|hicking"),
         activity_recoded = case_when(is_bike ~ "Bike",
                                      is_run ~ "Run",
                                      str_detect(activityType,"swim") ~"Swim",
                                      TRUE ~ "Other")) %>% 
  mutate(across(c(contains("Id"),contains("uuid")),
                as.character)) %>% 
  mutate(qual_distance=as.character(cut(distance,
                                        quantile(distance,probs = 0:5/5,na.rm=T),
                                        include.lowest = T,
                                        labels=c("Very short","Short",
                                                 "Average","Long","Very long"))),
         qual_avgHr=as.character(cut(avgHr,quantile(avgHr,0:3/5,na.rm = T),
                                     include.lowest = T,
                                     labels=c("Low intensity","Average intensity",
                                              "High intensity"))),
         qual_distance=ifelse(is.na(qual_distance),"Very short",qual_distance),
         distance=distance/1E5, # convert in km
         calories = calories/4.184) %>% # convert from joules in calories
  mutate(across(contains("elevation"),function(xx) xx/1E2))  %>%  # convert toÒ meters
  mutate(across(contains("Speed"),function(xx) xx*36))  %>% # convert to km/h
  mutate(across(c(duration,contains("Duration")),
                function(xx) time_length(xx/1000,"minutes")))


dat_clean <- filter(dat,!(activityId %in% c(407226313,2321338)) & year(date)>=2012)

# Save an object to a file
saveRDS(dat_clean, file = "dat_clean.rds")

dat_bike <- filter(dat_clean,is_bike) # Bike activities

dat_bike<-slice(dat_bike,-c(2686,2690,2121,2594)) ##remove outliers
saveRDS(dat_bike,"dat_bike.rds") 


dat_bike_imp <- mutate(dat_bike,across(where(is.numeric),
                                       function(xx) ifelse(is.na(xx),median(xx,na.rm=T),xx)))
most_freq_cat <- function(xx)
{
  tab <- table(xx)
  return(names(tab[which.max(tab)]))
}

dat_bike_imp <- mutate(dat_bike_imp,across(where(is.character),
                                           function(xx) coalesce(xx,most_freq_cat(xx))))

sapply(dat_bike_imp,function(xx) sum(is.na(xx)))
missing <- which(sapply(dat_bike_imp,function(xx) sum(is.na(xx)))>0) %>%  
  names()
dat_bike_imp <- select(dat_bike_imp,-missing)

dat_bike_imp<-slice(dat_bike_imp,-c(2686,2690,2121,2594)) ##remove outliers

saveRDS(dat_bike_imp,"dat_bike_imp.rds")
