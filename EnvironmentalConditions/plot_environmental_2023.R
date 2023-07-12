library(devtools)
library(CDECRetrieve)
library(tidyverse)
library(lubridate)
library(padr)
library(zoo)
library(purrr)
library(patchwork)
library(readr)

# This file pulls and plots the environmental data for Smelt OMR Season for the 2023 OMR Seasonal Report.
# Last edited by C. Pien (cpien@usbr.gov), data pulling code derived from code by N. Bertrand


#sets the dates to be pulled from cdec for the OMR season

start.date <- "2022-10-01"
start.date.FPT <- "2022-09-29"
end.date <- "2023-06-30"

#series of cdec queries to pull data needed to fill out the reports datafile ------------
clc.C <- cdec_query("CLC", "146", "D", start.date, end.date)%>%
  rename(date = datetime) %>%
  mutate(date = as.Date(date))

OBI.fnu <- cdec_query("OBI", "221", "D", start.date, end.date) %>%
  rename(date = datetime) %>%
  mutate(date = as.Date(date))

OBI.fnu.event <- cdec_query("OBI", "221", "E", start.date, end.date) %>%
  rename(date = datetime) %>%
  mutate(date = as.Date(date))

FPT.cfs <- cdec_query("FPT", "20", "D", start.date.FPT, end.date)%>%
  rename(date = datetime)%>%
  mutate(date = as.Date(date))

FPT.fnu <- cdec_query("FPT", "221", "D", start.date.FPT, end.date)%>%
  rename(date = datetime)%>%
  mutate(date = as.Date(date))

FPT.cfs <- cdec_query("FPT", "20", "D", start.date, end.date)%>%
  rename(date = datetime)%>%
  mutate(date = as.Date(date))

MSD.f <- cdec_query("MSD", 25, "E", start.date, end.date) %>% 
  mutate(date = date(datetime))

PPT.f <- cdec_query("PPT", 25, "E", start.date, end.date)%>% 
  mutate(date = date(datetime)) 

#### Old method  of creating data

# DateSeriesWY2023 <- data.frame(date = seq(as.Date(start.date),as.Date(end.date), by = "1 days"))
# date.key = DateSeriesWY2023

#### Clean up data and make sure not too many dates missing -------------------------------

OBI.fnu.smelt <- OBI.fnu %>%
  select(date, parameter_value) %>% rename(OBI.fnu.smelt = parameter_value) %>%
  pad #double check all dates in there

(OBI.fnu.smelt %>% filter(is.na(OBI.fnu.smelt))) # 1 day missing
 
FPT.cfs.smelt <- FPT.cfs %>% 
  select(date, parameter_value) %>% rename(FPT.cfs.smelt = parameter_value) %>%
  pad %>%
  arrange(date) %>%
  mutate(FPT.cfs.smelt = as.numeric(FPT.cfs.smelt),
         FPT.3day.cfs = rollapplyr(FPT.cfs.smelt,3,  mean, align = "right", partial =T)) %>%
  filter(date >= start.date)

(FPT.cfs.smelt %>% filter(is.na(FPT.cfs.smelt))) # 2 days missing

FPT.fnu.smelt <- FPT.fnu %>%
  select(date, parameter_value) %>% rename(FPT.fnu.smelt = parameter_value) %>%
  pad %>%
  arrange(date)%>%
  mutate(FPT.fnu.smelt = as.numeric(FPT.fnu.smelt),
         FPT.3day.fnu = rollapplyr(FPT.fnu.smelt,3, mean, align = "right", partial =TRUE)) %>%
  filter(date >= start.date)

(FPT.fnu.smelt %>% filter(is.na(FPT.fnu.smelt))) # 6 days missing

CLC.F.smelt <- clc.C %>% 
  select(date, parameter_value) %>% rename(CLC.C.smelt = parameter_value) %>%
  mutate(CLC.F.smelt = (CLC.C.smelt * 9/5) + 32) %>%
  pad

(CLC.F.smelt %>% filter(is.na(CLC.F.smelt))) # 0 days missing

MSD.F.salmon <- MSD.f %>%
  group_by(date) %>% 
  mutate(msd.F = mean(parameter_value,na.rm =TRUE)) %>% 
  ungroup() %>%
  select(date, msd.F) %>% 
  distinct() %>% 
  drop_na() %>%
  pad() %>%
  arrange(date) 

PPT.F.salmon <- PPT.f %>%
  group_by(date) %>% 
  mutate(ppt.F = mean(parameter_value,na.rm =TRUE)) %>% 
  ungroup() %>%
  select(date, ppt.F) %>% 
  distinct() %>% 
  drop_na() %>%
  pad() %>%
  arrange(date) 

# Combine into one df and write ----------------------------------
smelt_env_params <- reduce(list(OBI.fnu.smelt, FPT.cfs.smelt, FPT.fnu.smelt), dplyr::left_join, by = "date")
# write_csv(smelt_env_params, "EnvironmentalConditions/output/Data_smelt_environmental.csv")

offramp_env_params <- reduce(list(CLC.F.smelt, MSD.F.salmon, PPT.F.salmon), dplyr::left_join, by = "date") %>%
  filter(date>="2023-06-01")
 # write_csv(offramp_env_params, "EnvironmentalConditions/output/Offramp_temperatures_smelt_salmon.csv")

# Make plots -----------------------------------------

theme_plots <- theme(axis.title.x = element_blank(),
                     axis.text = element_text(size = 11),
                     axis.title = element_text(size = 12))

(plot_obi <- ggplot(smelt_env_params) + 
   geom_hline(yintercept = 12,  linewidth = 1, linetype = "dashed", color = "gray70") +
  geom_line(aes(date, OBI.fnu.smelt)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
  labs(y = "OBI Turbidity (FNU)") +
  theme_bw() +
    theme_plots)

(plot_fpt1 <- ggplot(smelt_env_params) + 
    geom_hline(yintercept = 25000, linewidth = 1, linetype = "dashed", color = "gray70") +
  geom_line(aes(date, FPT.cfs.smelt)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
  labs(y = "FPT Flow (cfs)", title = "A") +
  theme_bw() +
  theme_plots)

(plot_fpt2 <- ggplot(smelt_env_params) + 
    geom_hline(yintercept = 50, linewidth = 1, linetype = "dashed", color = "gray70") +
    geom_line(aes(date, FPT.fnu.smelt)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
    labs(y = "FPT Turbidity (FNU)", title = "B") +
    theme_bw() +
    theme_plots)

(plot_fpt <- plot_fpt1/plot_fpt2)


(plot_clc <- ggplot(offramp_env_params) + 
    geom_hline(yintercept = 77, linewidth = 1, linetype = "dashed", color = "gray70") +
    geom_line(aes(date, CLC.F.smelt)) +
    labs(y = "CLC Temperature (°C)", title = "C") +
    theme_bw() +
    theme_plots)

(plot_msd <- ggplot(offramp_env_params) + 
    geom_hline(yintercept = 71.6, linewidth = 1, linetype = "dashed", color = "gray70") +
    geom_line(aes(date, msd.F)) +
    labs(y = "MSD Temperature (°F)", title = "A") +
    theme_bw() +
    theme_plots)

(plot_ppt <- ggplot(offramp_env_params) + 
    geom_hline(yintercept = 71.6, linewidth = 1, linetype = "dashed", color = "gray70") +
    geom_line(aes(date, ppt.F)) +
    labs(y = "PPT Temperature (°F)", title = "B") +
    theme_bw() +
    theme_plots)

(plot_offramp <- plot_msd/plot_ppt/plot_clc)



# Write plots------------------------------------------
tiff("EnvironmentalConditions/output/Figure_obi_turbidity.tiff", width = 8, height = 5, units = "in", res = 300, compression = "lzw")
plot_obi
dev.off()

tiff("EnvironmentalConditions/output/Figure_fpt_flow_turbidity.tiff", width = 8, height = 9, units = "in", res = 300, compression = "lzw")
plot_fpt
dev.off()

tiff("EnvironmentalConditions/output/Figure_offramp_temperatures.tiff", width = 7, height = 9, units = "in", res = 300, compression = "lzw")
plot_offramp
dev.off()
