#Clear environment
rm(list = ls())
#Libraries
library(ggplot2)
library(plotly)
library(ggpubr)

# Load data object (data_exploration_R_1.2.R)
dat_clean<-readRDS(file = "dat_clean.rds")

gg<-ggplot(dat_clean,aes(x=avgSpeed,y=elevationGain,
                         size=calories,color=duration,alpha=avgHr))+
  geom_jitter()+theme_pubr()

#ggplotly(gg)

reshaped <- select(dat_clean,calories,distance) %>% 
  pivot_longer(cols = everything(),names_to="latent_variable",values_to="values")
reshaped

group_by(reshaped,latent_variable) %>% 
  mutate(values=(values-mean(values,na.rm = T))/sd(values,na.rm = T)) %>% 
  ggplot(aes(values,color=latent_variable)) + geom_histogram()+
  theme_minimal()

gg1<-ggplot(reshaped,aes(values)) + geom_histogram() + 
  facet_wrap(.~latent_variable,scales = "free")+
  theme_minimal()

require(ggpubr)
gg2 <- ggplot(dat_clean,aes(distance)) + geom_density() + 
  labs(title = "Distance distribution")+
  theme_minimal()

ggarrange(gg1,gg2,ncol = 2,widths = c(1,2))

require(GGally)
require(tidyverse)
select(dat_clean,distance,duration,avgSpeed,
       calories,activity_recoded) %>% 
  GGally::ggpairs()

###pretty cool trick to see data summary
ggplot(dat_clean,aes(activityType, y=duration,weight=duration))+
  geom_bar(stat="summary",fun="median" )+coord_flip()

#imputation
dat_bike <- filter(dat_clean,is_bike) #Bike activities
sapply(dat_bike,function(xx) sum(is.na(xx)))

col_to_remove<-sapply(dat_bike,function(xx) sum(is.na(xx)))==nrow(dat_bike) 
select(dat_bike,-which(col_to_remove))->dat_bike

#impute for continous variables
dat_bike_imp <- mutate(dat_bike,across(where(is.numeric), function(xx) ifelse(is.na(xx),median(xx,na.rm=T),xx)))

#check
sapply(dat_bike_imp,function(xx) sum(is.na(xx)))

#impute for categorical variables
most_freq_cat <- function(xx)
{
  tab <- table(xx)
  return(names(tab[which.max(tab)]))
}

dat_bike_imp <- mutate(dat_bike_imp,across(where(is.character), function(xx) coalesce(xx,most_freq_cat(xx))))

#check
sapply(dat_bike_imp,function(xx) sum(is.na(xx)))

##remove outliers
dat_bike_imp<-slice(dat_bike_imp,-c(2686,2690,2121,2594))

missing <- which(sapply(dat_bike_imp,function(xx) sum(is.na(xx)))>0) %>%  
  names()
dat_bike_imp <- select(dat_bike_imp,-missing)

View(dat_bike_imp) #now has no missing values

#
#normalization for ML
#standardization (statistics)
#min-max(ML)

require(FactoMineR)
acp_dat <- select(dat_bike_imp,deviceId,duration,distance,elevationGain,elevationLoss,avgSpeed,avgHr,calories,minTemperature,maxTemperature,lapCount,avgBikeCadence,avgPower,vO2MaxValue,max20MinPower) 
acp <-   PCA(acp_dat,graph = T,quali.sup = c(1))
acp$eig
#barplot(acp$eig[,2])
summary.PCA(acp)
#plot.PCA(acp)

acp$var$coord #new coordinates in new dimensions
acp$var$cos2  #how close is the projection, to the original coordinates
acp$var$contrib #coef of linear combinaiton

#---excercise Run activities, select relevant variables, do pca
dat_run <- filter(dat_clean,is_run) #Run activities

sapply(dat_run,function(xx) sum(is.na(xx)))

col_to_remove<-sapply(dat_run,function(xx) sum(is.na(xx)))==nrow(dat_run) 
select(dat_run,-which(col_to_remove))->dat_run
str(dat_run)

#impute for continous variables
library(impute)
func_knn_imp<-function (dat){
  impute.knn(as.matrix(dat))
}

#func_knn_imp<-function (dat){
#  ImputeKnn(dat_run, k = 15, scale = T, meth = #"weighAvg", distData = NULL)
#  }

dat_run_imp <- mutate(dat_run,across(where(is.numeric),func_knn_imp() ))

#check
sapply(dat_run_imp,function(xx) sum(is.na(xx)))

#impute for categorial variables
most_freq_cat <- function(xx)
{
  tab <- table(xx)
  return(names(tab[which.max(tab)]))
}

dat_run_imp <- mutate(dat_run_imp,across(where(is.character), function(xx) coalesce(xx,most_freq_cat(xx))))

#check
sapply(dat_run_imp,function(xx) sum(is.na(xx)))

missing <- which(sapply(dat_run_imp,function(xx) sum(is.na(xx)))>0) %>%  
  names()
dat_run_imp <- select(dat_run_imp,-missing)

View(dat_run_imp) #now has no missing values

#normalization for ML
#standardization (statistics)
#min-max(ML)

#visualize
ggplot(dat_run_imp,aes(activityType, y=duration,weight=duration))+
  geom_bar(stat="summary",fun="mean" )+coord_flip()

require(FactoMineR)
acp_dat <- select(dat_run_imp,deviceId,duration,distance,elevationGain,elevationLoss,avgSpeed,avgHr,calories,minTemperature,maxTemperature,vO2MaxValue) 
acp <-   PCA(acp_dat,graph = T,quali.sup = c(1))
acp$eig
barplot(acp$eig[,2])
summary.PCA(acp)
plot.PCA(acp)

acp$var$coord #new coordinates in new dimensions
acp$var$cos2  #how close is the projection, to the original coordinates
acp$var$contrib #coef of linear combinaiton

library(ggfortify)
apc<-prcomp(acp_dat[-1], scale. = TRUE)
biplot(prcomp(acp_dat[-1], scale = TRUE))

library(Rtsne)
tsne<-select(dat_bike_imp,where(is.numeric))%>%
  Rtsne(dims=2,max_iter=500,check_duplicates=F)
str(tsne)

## Generate t_SNE plot
par(mfrow=c(1,2))
plot(tsne$Y )
plot(tsne$Y,col= dat_bike_imp$activityType,
     bg= dat_bike_imp$activityType)
#