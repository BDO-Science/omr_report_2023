library(devtools)
library(CDECRetrieve)
library(tidyverse)
library(lubridate)
library(data.table)
library(plotrix)
library(readxl)
library(padr)


#sets the dates to be pulled from cdec for the OMR season

start.date <- "2022-10-01"
end.date <- "2023-06-28"

#series of cdec queries to pull data needed to fill out the reports datafile ------------
clc.C <- cdec_query("CLC", "146", "D", start.date, end.date)

OBI.fnu <- cdec_query("OBI", "221", "D", start.date, end.date) %>%
  rename(date = datetime) %>%
  mutate(date = as.Date(date))

OBI.fnu.event <- cdec_query("OBI", "221", "E", start.date, end.date) %>%
  rename(date = datetime) %>%
  mutate(date = as.Date(date))

FPT.cfs <- cdec_query("FPT", "20", "D", start.date, end.date)

FPT.fnu <- cdec_query("FPT", "221", "D", start.date, end.date)

FPT.cfs <- cdec_query("FPT", "20", "D", start.date, end.date)

#### Clean up data and make sure no dates missing -------------------------------

obi.fnu.alldates <- OBI.fnu %>%
  select(datetime, parameter_value) %>% rename(OBI.fnu.smelt = parameter_value) %>%
  pad #double check all dates in there

(obi.fnu.alldates %>% filter(is.na(parameter_value))) # 1 day missing

FPT.cfs.smelt <- FPT.cfs %>% 
  select(datetime, parameter_value) %>% rename(FPT.cfs.smelt = parameter_value) %>%
  pad

FPT.fnu.smelt <- FPT.fnu %>%
  select(datetime, parameter_value) %>% rename(FPT.fnu.smelt = parameter_value) %>%
  pad

CLC.C.smelt <- clc.C %>% 
  select(datetime, parameter_value) %>% rename(CLC.C.smelt = parameter_value) %>%
  pad






obi_fill <- OBI.fnu.event %>%
  filter(date == "2023-01-12") %>%
  summarize(mean(parameter_value, na.rm = TRUE))

#checks date.key against the dates recorded by the sensor

#appends the date key to the cdec dataframe
OBI.test <- OBI.fNU %>% cbind(date.key)
#View(OBI.test)
#checks all the dates match from key to daily value
which(OBI.test$Date == OBI.test$datetime)
#generates two columns to  be used in final table. 
obi.fnu.smelt <- OBI.test %>% select(datetime, parameter_value) %>% rename(OBI.fnu.smelt = parameter_value)
#View(obi.fnu.smelt)

##############################################################################
#checks date.key against the dates recorded by the sensor

#appends the date key to the cdec dataframe
FPT.test <- FPT.cfs %>% cbind(date.key)
#View(OBI.test)
#checks all the dates match from key to daily value
which(FPT.test$Date == FPT.test$datetime)
#generates two columns to  be used in final table. 
FPT.cfs.smelt <- FPT.test %>% select(datetime, parameter_value) %>% rename(FPT.cfs.smelt = parameter_value)
#View(FPT.cfs.smelt)
############################################################################################
#checks date.key against the dates recorded by the sensor

#appends the date key to the cdec dataframe
FPT.test2 <- FPT.fnu %>% cbind(date.key)
#View(OBI.test)
#checks all the dates match from key to daily value
which(FPT.test2$Date == FPT.test2$datetime)
#generates two columns to  be used in final table. 
FPT.fnu.smelt <- FPT.test2 %>% select(datetime, parameter_value) %>% rename(FPT.fnu.smelt = parameter_value)
#View(FPT.fnu.smelt)
#########################################################################################
#checks date.key against the dates recorded by the sensor

#appends the date key to the cdec dataframe
clc.test <- clc.C %>% cbind(date.key)
#View(OBI.test)
#checks all the dates match from key to daily value
which(clc.test$Date == clc.test$datetime)
#generates two columns to  be used in final table. 
clc.C.smelt <- clc.test %>% select(datetime, parameter_value) %>% rename(CLC.C.smelt = parameter_value)
#View(clc.C.smelt)

####################################################################################################
#Binds all separate data frames into a single data frame.

smelt.dayflow <-obi.fnu.smelt %>% cbind(FPT.cfs.smelt,FPT.fnu.smelt, clc.C.smelt)
#View(smelt.dayflow)

smelt.dayflow <- smelt.dayflow %>% select("datetime", "OBI.fnu.smelt","FPT.cfs.smelt","FPT.fnu.smelt", "CLC.C.smelt")
#View(smelt.dayflow)

smelt.dayflow$OBI.fnu.smelt <- as.numeric(smelt.dayflow$OBI.fnu.smelt)
smelt.dayflow$FPT.cfs.smelt <- as.numeric(smelt.dayflow$FPT.cfs.smelt)
smelt.dayflow$FPT.fnu.smelt <- as.numeric(smelt.dayflow$FPT.fnu.smelt)
smelt.dayflow$CLC.C.smelt <- as.numeric(smelt.dayflow$CLC.C.smelt)
###############
###########
##########
#generates the three day averages for freeport
for(i in 4:nrow(smelt.dayflow)){
  #d5[i, "avg"] places the result of the mean funtion into the NA cell of the column "avg"
  #required to be done as.numeric to avoid errors.
  #i-1 ori-2 select the values in the cells relative to the i position 
  smelt.dayflow[i, "FPT.cfs.3Davg.smelt"] <- mean(smelt.dayflow[(i-2):i, "FPT.cfs.smelt"], na.rm=TRUE)
  smelt.dayflow[i, "FPT.fnu.3Davg.smelt"] <- mean(smelt.dayflow[(i-2):i, "FPT.fnu.smelt"], na.rm=TRUE)
  #  browser()
}
View(smelt.dayflow)
#################################
#########
#######
write.csv(smelt.dayflow, file = "smelt_dayflow2021.csv")
##########