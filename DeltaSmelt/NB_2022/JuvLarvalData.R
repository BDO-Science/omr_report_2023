library(tidyverse)
library("parzer")
library(readxl)
library(readxl)


################
#Larval delta smelt data assembly 


#######
#takes in the SLS and 20 mm data file

library(readxl)
X2022_DS_collected_in_SLS_and_20_mm <- read_excel("C:/Users/nbertrand/Desktop/Bertrand/GitHub/OMRSeasonalReport/DeltaSmeltData/2022 DS collected in SLS and 20-mm.xlsx")
View(X2022_DS_collected_in_SLS_and_20_mm)

lar <- X2022_DS_collected_in_SLS_and_20_mm

#pairs the stations from 20mm to the station reported for the catch
library(readr)
CDFW_20mm_station_gps_csv_file <- read_csv("C:/Users/nbertrand/Desktop/Bertrand/GitHub/OMRSeasonalReport/DeltaSmeltData/CDFW 20mm station gps csv file.csv")
#View(CDFW_20mm_station_gps_csv_file)
stat.20mm <- CDFW_20mm_station_gps_csv_file

lar.gps<-left_join(lar,stat.20mm, by = "Station")
view(lar.gps)
########################################
###############
###############


##############################
write.csv(lar.gps, file = "C:/Users/nbertrand/Desktop/Bertrand/GitHub/OMRSeasonalReport/DeltaSmeltData/CDFWDataonly.csv")



