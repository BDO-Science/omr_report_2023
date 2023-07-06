library(dplyr)
library(sf)
library(ggplot2)
library(readr)


# TFCF: 37.815176 -121.560709 (WGS84)
# Skinner: 37.82524 -121.59523

# Compile Stations 
sta_20mm <- read_csv("DeltaSmelt/data/CDFW 20mm station gps csv file.csv") %>%
  mutate(Survey = "20mm")
sta_skt <- read_csv("DeltaSmelt/data/SKTstations.csv") %>%
  select(-1, -Description)
sta_salvage <- data.frame(Survey = "Salvage",
                          Station = c("TFCF", "Skinner"),
                          Latitude = c(37.815176,37.82524),
                          Longitude = c(-121.560709, -121.59523))
sta_chipps <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.244.11&entityid=99a038d691f27cd306ff93fdcbc03b77") %>%
  filter(MethodCode == "MWTR" & Location == "Chipps Island") %>%
  mutate(Survey = "Chipps Trawl") %>%
  rename(Station = StationCode) %>%
  select(Survey, Station, Latitude, Longitude)

# Read in fish data
# Broodstock fish: 38 03' 816" N 121 47, 915" W


# Create map