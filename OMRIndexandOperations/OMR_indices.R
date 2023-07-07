#Code by Nick Bertrand
#nbertrand@usbr.gov

#this script will create the graph for the OMR index figure


library(readxl)
library(tidyverse)
library(scales)

#data provided by Reclamation CVO and DWR

###################################################
#sample data for figure development
#delete once actual data is available
library(readxl)
Appendix_C_WY2022_OMR_Seasonal_Report_Data_File <- read_excel("Appendix C - WY2022 OMR Seasonal Report Data File.xlsx", 
                                                              sheet = "DayFlow Daily")
View(Appendix_C_WY2022_OMR_Seasonal_Report_Data_File)

test <- Appendix_C_WY2022_OMR_Seasonal_Report_Data_File

OMR_data <- test %>% select(Date,`OMR Index`,`OMR Index 5-day`, `OMR Index 14-day`) %>% 
  gather(`OMR Index`,`OMR Index 5-day`, `OMR Index 14-day`, key = "Indexes", value = "cfs")

#######################################################
#plots data

OMR_plot <- ggplot(OMR_data, aes(x = as.Date(Date), y = cfs, linetype = Indexes))+
  geom_line()+
  scale_x_date(name= "Date", breaks = date_breaks("months"),labels = date_format("%m/%d")) +
  scale_y_continuous(name = "Flow (cfs)", breaks = pretty_breaks(n = 10))+
  theme_classic()+
  theme(axis.text.x = element_text(angle=45), legend.position="bottom")

OMR_plot




