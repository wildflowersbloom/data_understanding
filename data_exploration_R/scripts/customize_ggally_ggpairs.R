#Clear environment
rm(list = ls())
#Libraries
library(ggplot2)
require(GGally)
require(tidyverse)

# Load data object (data_exploration_R_1.2.R)
dat_clean<-readRDS(file = "sports_data.rds")

# GGally start point ggpairs plot
select(dat_clean,distance,duration,avgSpeed,
       calories,activity_recoded) %>% 
  GGally::ggpairs()

# Customize GGally ggpairs plot
select(filter(dat_clean,activity_recoded!="Other"), #filter out "Other"
       distance,duration,avgSpeed,
       calories,activity_recoded) %>% 
  GGally::ggpairs(columns=c(1,2,4,5),  #select vars inside 
                  mapping = aes(color = activity_recoded, alpha=0.5))+ #color
  theme_pubr() 

# Customize further 
# .. Create a function to customize the regression displayed
lowerFn <- function(data, mapping, method = "lm", ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_point() +
    geom_smooth(method = method, color = "red", ...)
  p
}

select(filter(dat_clean,activity_recoded!="Other"), #filter out "Other"
       distance,duration,avgSpeed,
       calories,activity_recoded)%>% 
  GGally::ggpairs(columns=c(1,2,4),  #select vars inside 
                  mapping = aes(color = activity_recoded, alpha=0.5), #color
                  lower = list(continuous = GGally::wrap(lowerFn, method = "lm")))+
  theme_pubr() 

