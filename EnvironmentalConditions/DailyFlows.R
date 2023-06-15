library(devtools)
library(CDECRetrieve)
library(tidyverse)
library(lubridate)
library(data.table)
library(plotrix)
library(readxl)
#this code pulls the event data on stations (associated with Salmonids mostly) to average the daily means to pasted into the final data file.

#sets the dates to be pulled from cdec for the OMR season

start.date <- "2021-10-01"
end.date <- "2022-06-30"

#series of cdec queries to pull data needed to fill out the reports datafile

fpt.cfs <- cdec_query("FPT", 20, "E", start.date, end.date) %>% 
  group_by(date = date(datetime)) %>% 
  mutate(daily.msd.f = mean(parameter_value,na.rm =TRUE)) %>% 
  select(agency_cd, location_id, date, parameter_cd, daily.msd.f) %>% 
  rename(datetime=date,parameter_value =  daily.msd.f) %>% 
  distinct() %>% 
  drop_na()
view(fpt.cfs)  

#################################
#########
#######
#creates the csv file
write.csv(fpt.cfs, file = "fpt_cfs.csv")
##########
#################
###########


vns.cfs <- cdec_query("VNS", 20, "E", start.date, end.date) %>% 
  group_by(date = date(datetime)) %>% 
  mutate(daily.msd.f = mean(parameter_value,na.rm =TRUE)) %>% 
  select(agency_cd, location_id, date, parameter_cd, daily.msd.f) %>% 
  rename(datetime=date,parameter_value =  daily.msd.f) %>% 
  distinct()# %>% 
 # drop_na()
view(vns.cfs) 
#################################
#########
#######
#creates the csv file
write.csv(vns.cfs, file = "vns_cfs.csv")
##########
#################
###########


wlk.cfs <- cdec_query("wlk", 20, "E", start.date, end.date) %>% 
  group_by(date = date(datetime)) %>% 
  mutate(daily.msd.f = mean(parameter_value,na.rm =TRUE)) %>% 
  select(agency_cd, location_id, date, parameter_cd, daily.msd.f) %>% 
  rename(datetime=date,parameter_value =  daily.msd.f) %>% 
  distinct()# %>% 
# drop_na()
view(wlk.cfs) 
#################################
#########
#######
#creates the csv file
write.csv(wlk.cfs, file = "wlk_cfs.csv")
##########
#################
###########


mlm.cfs <- cdec_query("mlm", 20, "E", start.date, end.date) %>% 
  group_by(date = date(datetime)) %>% 
  mutate(daily.msd.f = mean(parameter_value,na.rm =TRUE)) %>% 
  select(agency_cd, location_id, date, parameter_cd, daily.msd.f) %>% 
  rename(datetime=date,parameter_value =  daily.msd.f) %>% 
  distinct()# %>% 
# drop_na()
view(mlm.cfs) 
#################################
#########
#######
#creates the csv file
write.csv(mlm.cfs, file = "mlm_cfs.csv")
##########
#################
###########


DCV.cfs <- cdec_query("DCV", 20, "E", start.date, end.date) %>% 
  group_by(date = date(datetime)) %>% 
  mutate(daily.msd.f = mean(parameter_value,na.rm =TRUE)) %>% 
  select(agency_cd, location_id, date, parameter_cd, daily.msd.f) %>% 
  rename(datetime=date,parameter_value =  daily.msd.f) %>% 
  distinct()# %>% 
# drop_na()
view(DCV.cfs) 
#################################
#########
#######
#creates the csv file
write.csv(DCV.cfs, file = "DCV_cfs.csv")
##########
#################
###########                      