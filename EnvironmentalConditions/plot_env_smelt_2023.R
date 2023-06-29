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
end.date <- "2023-06-28"

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



#### Old method  of creating data

# DateSeriesWY2023 <- data.frame(date = seq(as.Date(start.date),as.Date(end.date), by = "1 days"))
# date.key = DateSeriesWY2023

#### Clean up data and make sure no dates missing -------------------------------

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

CLC.C.smelt <- clc.C %>% 
  select(date, parameter_value) %>% rename(CLC.C.smelt = parameter_value) %>%
  pad

(CLC.C.smelt %>% filter(is.na(CLC.C.smelt))) # 0 days missing


# Combine into one df and write ----------------------------------
smelt_env_params <- reduce(list(OBI.fnu.smelt, FPT.cfs.smelt, FPT.fnu.smelt, CLC.C.smelt), dplyr::left_join, by = "date")
# write_csv(smelt_env_params, "EnvironmentalConditions/output/Data_smelt_environmental.csv")


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


(plot_clc <- ggplot(smelt_env_params) + 
    geom_hline(yintercept = 25, linewidth = 1, linetype = "dashed", color = "gray70") +
    geom_line(aes(date, CLC.C.smelt)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
    scale_y_continuous(breaks = seq(0,26, 2)) +
    labs(y = "CLC Temperature (°C)") +
    theme_bw() +
    theme_plots)

# Write plots------------------------------------------
tiff("EnvironmentalConditions/output/Figure_obi_turbidity.tiff", width = 8, height = 5, units = "in", res = 300, compression = "lzw")
plot_obi
dev.off()

tiff("EnvironmentalConditions/output/Figure_fpt_flow_turbidity.tiff", width = 8, height = 9, units = "in", res = 300, compression = "lzw")
plot_fpt
dev.off()

tiff("EnvironmentalConditions/output/Figure_clc_temperature.tiff", width = 8, height = 5, units = "in", res = 300, compression = "lzw")
plot_clc
dev.off()
