#Code by Nick Bertrand
#nbertrand@usbr.gov

#this script will create the graph for the OMR index and exports  figure


library(readxl)
library(tidyverse)
library(scales)
library(ggpubr)

#data provided by Reclamation CVO and DWR

###################################################
#sample data for figure development
#delete once actual data is available
library(readxl)
Appendix_C_WY2022_OMR_Seasonal_Report_Data_File <- read_excel("Appendix C - WY2022 OMR Seasonal Report Data File.xlsx", 
                                                              sheet = "DayFlow Daily")
View(Appendix_C_WY2022_OMR_Seasonal_Report_Data_File)

test2 <- Appendix_C_WY2022_OMR_Seasonal_Report_Data_File

exp_data <- test2 %>% select(Date,`Jones PP (cfs)`,`Clifton Court Inflow (cfs)`,`OMR Index`,`Maximum Capacity`) %>% 
  gather(`Jones PP (cfs)`,`Clifton Court Inflow (cfs)`,`OMR Index`,`Maximum Capacity`, key = "Variable", value = "cfs")

#######################################################
#Filters data to just the exports
exp_data_filter1 <- exp_data %>% filter(Variable == "Jones PP (cfs)" |Variable == "Clifton Court Inflow (cfs)" )
#view(exp_data_filter1)
exp_data_filter2 <- exp_data %>% filter(Variable == "OMR Index")
#plots data
exp_plot <- ggplot(exp_data_filter1, aes(x = as.Date(Date), y = cfs, fill = Variable))+
  geom_bar(stat="identity")+
  scale_x_date(name= "Date", breaks = date_breaks("months"),labels = date_format("%m/%d")) +
  scale_y_continuous(name = "Flow (cfs)", breaks = pretty_breaks(n = 10))+
  theme_classic()+
  theme(axis.text.x = element_text(angle=45), legend.position="bottom")

exp_plot

OMRIndex_plot <- ggplot(exp_data_filter2, aes(x = as.Date(Date), y = cfs, fill = Variable))+
  geom_line()+
  scale_x_date(name= "Date", breaks = date_breaks("months"),labels = date_format("%m/%d")) +
  scale_y_reverse(name = "Daily OMR Index (cfs)", breaks = pretty_breaks(n = 10))+
  theme_classic()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

OMRIndex_plot


figure <- ggarrange(OMRIndex_plot,exp_plot,
                    labels = c("A", "B"),
                    ncol = 1, nrow = 2)
figure

