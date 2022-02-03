require(lubridate)
require(tidyverse)
require(jsonlite)

# import one of the files to check what's in it
test <- fromJSON("Data/Sports/Raw/2022/ixxxen_1001_summarizedActivities.json",
                 simplifyDataFrame = F,simplifyVector = F,simplifyMatrix = F)
# get all possible fields for all activities (from sample)
all_fields <- sapply(test[[1]]$summarizedActivitiesExport, function(xx) names(xx)) %>% 
  unlist() %>% 
  unique()

# test[[1]]$summarizedActivitiesExport[[3]][["avgPower"]]

imports <- list.files("Data/Sports/Raw/2022",pattern = "json",full.names = T)

sports <- lapply(imports,
        function(xx) fromJSON(xx,simplifyDataFrame = F,simplifyVector = F,simplifyMatrix = F) )

test_and_extract <- function(activity,name)
{
  ### function that extracts a specific field of one activity 
  ### and returns NA if the field does not exist (eg power for running)
  #
  if (is.null(activity[[name]])) return(NA) 
  else return(activity[[name]])
}

extract_field_from_all_act <- function(json,name)
{
  res <- sapply(json[[1]]$summarizedActivitiesExport,function(xx) test_and_extract(xx,name))
  return(res)
}

extract_from_all_activities <- function(json,name)
{
  res <- lapply(json,function(xx) extract_field_from_single_act(xx,name)) %>% 
    unlist()
  names(res) <- NULL
  return(res)
}

dat <- lapply(all_fields,function(xx) extract_from_all_activities(sports,xx) )
names(dat) <- all_fields
dat$summarizedDiveInfo <- NULL
dat$splitSummaries <- NULL
dat$splits <- NULL
dat <- bind_cols(dat) %>% 
  mutate(across(-c(activityType,rule,sportType,name,locationName),
                as.numeric))


write.csv(dat,row.names = F,file="Data/Sports/Activities_2022.csv")
