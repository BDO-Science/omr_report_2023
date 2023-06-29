library(tidyverse)
library("parzer")
library(readxl)
library(readxl)


AllDataYC2022Adults_metadata_06102022 <- read_excel("C:/Users/nbertrand/Desktop/Bertrand/GitHub/OMRSeasonalReport/DeltaSmeltData/AllDataYC2022Adults_metadata_06152022.xlsx",
                                                    sheet = "RoughAllDataYC2022Adults", col_types = c("skip",
                                                                                                      "text", "text", "date", "text", "date",
                                                                                                      "numeric", "numeric", "text", "text",
                                                                                                      "text", "text", "text", "text", "numeric",
                                                                                                      "numeric", "text", "numeric", "text",
                                                                                                      "numeric", "text", "numeric", "numeric",
                                                                                                      "text", "numeric", "text", "date",
                                                                                                      "text", "text", "date", "text", "text",
                                                                                                      "numeric", "numeric", "text", "text",
                                                                                                      "text", "text", "text", "text", "text",
                                                                                                      "text", "text", "text", "text", "text",
                                                                                                      "text", "text", "date", "text", "text",
                                                                                                      "text", "text", "text", "text", "numeric",
                                                                                                      "text", "text", "text", "text", "text",
                                                                                                      "text", "text", "text"))
#View(AllDataYC2022Adults_metadata_06102022)
#a1<- head(AllDataYC2022Adults_metadata_06102022)
#view(a1)
data.sm <- AllDataYC2022Adults_metadata_06102022
####################################################################


data.sm.skt <- data.sm %>% filter(survey == "skt")

data.sm.edsm <- data.sm %>% filter(survey == "edsm")

#View(data.sm.edsm)

data.sm.skt.sl <- data.sm.skt %>%
  select(fish_id, survey, origin.sup, origin.ref,SampleDate,StationCode, ForkLength.x,Latitude, Longitude) %>% 
  rename(Station = StationCode, Date = SampleDate, ForkLength =ForkLength.x )
head(data.sm.skt.sl)

#view(data.sm.skt.sl)

data.sm.skt.sl.con = data.sm.skt.sl %>%
  mutate(Latitude = parzer::parse_lat(Latitude),
         Longitude = parzer::parse_lon(Longitude))

#view(data.sm.skt.sl.con)

data.sm.edsm.sl <- data.sm.edsm %>% 
  select(fish_id, survey, origin.sup, origin.ref, Date, Station, ForkLength.y,target_latitude, target_longitude) %>% 
  rename(ForkLength = ForkLength.y, Latitude = target_latitude, Longitude = target_longitude) 


#view(data.sm.edsm.sl)

data <-rbind(data.sm.skt.sl.con,data.sm.edsm.sl, ID = NULL) 
head(data)

View(data)

#write.csv(data, file = "C:/Users/nbertrand/Desktop/Bertrand/GitHub/OMRSeasonalReport/DeltaSmeltData/YC2022AdultMapdata.csv")

############
#######################
##########################
#generates the SKT stations in a correct format. 
#note the original file does not makes the longitude negative and this was corrected before use in ARC GIS

library(readxl)
SKT_station_locations <- read_excel("C:/Users/nbertrand/Desktop/Bertrand/GitHub/OMRSeasonalReport/DeltaSmeltData/SKT station locations.xlsx")
View(SKT_station_locations)

SKT_stat<- SKT_station_locations %>%
  mutate(Latitude = parzer::parse_lat(Latitude),
         Longitude = parzer::parse_lon(Longitude)) 
View(SKT_stat)

write.csv(SKT_stat, file = "C:/Users/nbertrand/Desktop/Bertrand/GitHub/OMRSeasonalReport/DeltaSmeltData/SKTstations.csv")
