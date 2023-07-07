#Code by Nick Bertrand
#nbertrand@usbr.gov

#this script will create the graph for the DCC gate opening and closures which are triggered
#by the catch indexes

library(readxl)
library(tidyverse)
library(scales)

#take in data files from SacPas from site below
#https://www.cbr.washington.edu/sacramento/data/query_sampling_graph.html

#data files pulled for WY2023 imported to R
#files need to be combined into the a single data file

WY2023_WR_KLCI <- read_csv("DCCgateOpeningsandClosures/WY2023_WR_KLCI.csv")
#removes SacPas Disclaimer text from dataframe
WY2023_WR_KLCI <- WY2023_WR_KLCI %>% slice(1:243)
#View(WY2023_WR_KLCI)
KLCI <- WY2023_WR_KLCI %>% rename(KLCI = `Raw Knights Landing RST`)

WY2023_WRSCI_Seines <- read_csv("DCCgateOpeningsandClosures/WY2023_WRSCI_Seines.csv")
WY2023_WRSCI_Seines <- WY2023_WRSCI_Seines %>% slice(1:365)
#View(WY2023_WRSCI_Seines)

SCI_Seines <- WY2023_WRSCI_Seines %>% rename(sci_seines = `Index Sacramento Beach Seines (SR080E SR071E SR062E SR057E SR055E SR060E AM001S SR049E)`)

WY2023_WRSCI_Trawls <- read_csv("DCCgateOpeningsandClosures/WY2023_WRSCI_Trawls.csv")
WY2023_WRSCI_Trawls <- WY2023_WRSCI_Trawls %>% slice(1:361)
#View(WY2023_WRSCI_Trawls)

SCI_Trawls <- WY2023_WRSCI_Trawls %>% rename(sci_trawls = `Index Sacramento Trawls (SR055M SR055E SR055W SR055X)`)

#joins all dataframes
dcc_SCI <-left_join(SCI_Seines,SCI_Trawls, by = "Date") 
#View(dcc_SCI)
dcc_SCI_KLCI <- left_join(dcc_SCI, KLCI, by = "Date")

#Delete unnecessary columns, gather by index label, & filter to OMR season dates
dcc_data <- dcc_SCI_KLCI %>% 
  select(Date, sci_seines, sci_trawls, KLCI) %>% 
  gather("sci_seines", "sci_trawls", "KLCI", key = "Name", value = "Index") %>% 
  filter(Date >= as.Date("2022-10-01") & Date <= as.Date("2023-06-30"))

#view(dcc_data)

dcc_plot <- ggplot(dcc_data, aes(x = as.Date(Date), y = Index, shape = Name))+
  geom_point()+
  theme_classic()+
  scale_x_date(name= "Date", breaks = date_breaks("months"),labels = date_format("%m/%d")) +
  scale_y_continuous(name = "Catch Index", breaks = pretty_breaks(n = 5))+
  theme(axis.text.x = element_text(angle=45),legend.position="bottom")

dcc_plot

