library(devtools)
library(CDECRetrieve)
library(tidyverse)
library(lubridate)
library(data.table)
library(plotrix)
library(readxl)


#sets the dates to be pulled from cdec for the OMR season

start.date <- "2021-10-01"
end.date <- "2022-06-28"

#series of cdec queries to pull data needed to fill out the reports datafile

msd.F <- cdec_query("MSD", 25, "E", start.date, end.date) %>% 
  group_by(date = date(datetime)) %>% 
  mutate(daily.msd.f = mean(parameter_value,na.rm =TRUE)) %>% 
  select(agency_cd, location_id, date, parameter_cd, daily.msd.f) %>% 
  rename(datetime=date,parameter_value =  daily.msd.f) %>% 
  distinct() %>% 
  drop_na()
#view(msd.F)


ppt.F <- cdec_query("PPT", 25, "E", start.date, end.date)%>% 
  group_by(date = date(datetime)) %>% mutate(daily.ppt.f= mean(parameter_value,na.rm =TRUE)) %>% 
  select(agency_cd, location_id, date, parameter_cd, daily.ppt.f) %>% 
  rename(datetime=date,parameter_value =  daily.ppt.f) %>% 
  distinct() %>% 
  drop_na()
#View(ppt.F)

############################
###########
###########
DateSeriesWY2022 <- read_excel("DateSeriesWY2022.xlsx", 
                               col_types = c("date"))
#View(DateSeriesWY2022)
date.key <- DateSeriesWY2022
##########################################################################################
#checks date.key against the dates recorded by the sensor

#appends the date key to the cdec dataframe
ppt.test <- ppt.F %>% cbind(date.key)
#head(ppt.F)
#View(ppt.test)
#checks all the dates match from key to daily value
which(ppt.test$Date.m == ppt.test$datetime)
#generates two columns to  be used in final table. 
ppt.F.sal <- ppt.test %>% select(datetime, parameter_value) %>% rename(ppt.F.sal = parameter_value)
#View(ppt.F.sal)

##############################################################################
##########################################################################################
#checks date.key against the dates recorded by the sensor

#appends the date key to the cdec dataframe
msd.test <- msd.F %>% cbind(date.key)
#View(msd.test)
#checks all the dates match from key to daily value
which(msd.test$Date.m == msd.test$datetime)
#generates two columns to  be used in final table. 
msd.F.sal <- msd.test %>% select(datetime, parameter_value) %>% rename(msd.F.sal = parameter_value)
#View(msd.F.sal)

##############################################################################
####################################################################################################
#Binds all separate data frames into a single data frame.

sal.dayflow <-msd.F.sal %>% cbind(ppt.F.sal)
#View(smelt.dayflow)

sal.dayflow <- sal.dayflow %>% select("datetime...1", "msd.F.sal","ppt.F.sal") %>% 
  rename(datetime = datetime...1)
View(sal.dayflow)
#################################
#########
#######
#creates the csv file
write.csv(sal.dayflow, file = "sal_dayflow2022.csv")
##########
#################
###########
