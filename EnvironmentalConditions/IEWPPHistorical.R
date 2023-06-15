library(devtools)
library(CDECRetrieve)
library(tidyverse)
library(lubridate)
library(data.table)
library(plotrix)
library(readxl)

# for stable version

#devtools::install_github("flowwest/CDECRetrieve")

#sets the dates to be pulled from cdec for the OMR season

start.date <- "1997-10-01"
end.date <- "2022-06-28"

#series of cdec queries to pull data needed to fill out the reports datafile

FPT.cfs <- cdec_query("FPT", "20", "E", start.date, end.date)
#view(FPT.cfs)
FPT.fnu <- cdec_query("FPT", "221", "E", start.date, end.date)

############################
###########
###########

FPT.cfs.d <- FPT.cfs %>% mutate(date = date(datetime), year = year(datetime)) %>% 
  group_by(year, date) %>% summarise(daily.mean = mean(parameter_value, na.rm=TRUE)) %>% 
  mutate(three.day.avg.cfs = frollmean(daily.mean, 3)) 

FPT.cfs.f <- FPT.cfs.d %>% mutate(month = month(date)) %>%
  drop_na(three.day.avg.cfs) %>% 
  filter((month == 12 | month == 1))

#view(FPT.cfs.f)
#################################################.
###############
##########
FPT.fnu.d <- FPT.fnu %>% mutate(date = date(datetime), year = year(datetime)) %>% 
  group_by(year, date) %>% summarise(daily.mean = mean(parameter_value, na.rm=TRUE)) %>% 
  mutate(three.day.avg.fnu = frollmean(daily.mean, 3)) 

FPT.fnu.f <- FPT.fnu.d %>% mutate(month = month(date)) %>%
  drop_na(three.day.avg.fnu) %>% 
  filter((month == 12 | month == 1))

#view(FPT.fnu.f)
############################################
####################
##############

cfs.fnu <- left_join(FPT.cfs.f, FPT.fnu.f, by = "date")

res <-  cfs.fnu %>% mutate(exceeded.cfs = ifelse(three.day.avg.cfs > 25000, 1,0)) %>% 
  mutate(exceeded.fnu = ifelse(three.day.avg.fnu > 50, 1,0)) %>% 
  mutate(exceeded.all = exceeded.fnu + exceeded.cfs)
#view(res)

dates <- res %>% 
  filter(exceeded.all == "2") %>% 
  select(date, three.day.avg.cfs, three.day.avg.fnu)
View(dates)

write.csv(dates, file = "IEWPPdaysabovetrigger.csv")

################################
############
#######

#sets the dates to be pulled from cdec for the OMR season

start.date <- "2021-10-01"
end.date <- "2022-06-28"

#series of cdec queries to pull data needed to fill out the reports datafile

FPT.cfs <- cdec_query("FPT", "20", "E", start.date, end.date)
#view(FPT.cfs)
FPT.fnu <- cdec_query("FPT", "221", "E", start.date, end.date)

############################
###########
###########

FPT.cfs.d <- FPT.cfs %>% mutate(date = date(datetime), year = year(datetime)) %>% 
  group_by(year, date) %>% summarise(daily.mean = mean(parameter_value, na.rm=TRUE)) %>% 
  mutate(three.day.avg.cfs = frollmean(daily.mean, 3)) 

View(FPT.cfs.d)

write.csv(FPT.cfs.d, file = "FreeportCFS.csv")

FPT.fnu.d <- FPT.fnu %>% mutate(date = date(datetime), year = year(datetime)) %>% 
  group_by(year, date) %>% summarise(daily.mean = mean(parameter_value, na.rm=TRUE)) %>% 
  mutate(three.day.avg.fnu = frollmean(daily.mean, 3))

write.csv(FPT.fnu.d, file = "FreeportFNU.csv")
