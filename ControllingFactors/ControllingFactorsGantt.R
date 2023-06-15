library(tidyverse)
library(lubridate)
library(DiagrammeR)
library(htmlwidgets)
library(ggplot2)
library(readr)

library(vistime)



# #ControlFactorTime <- read_delim("ControlFactorTime.txt", 
#                                 "\t", escape_double = FALSE, col_types = cols(start = col_character()), 
#                                 trim_ws = TRUE)

library(readxl)
ControlFactorTime <- read_excel("WY2022/ControllinginputFileForRandi_20220721.xlsx", 
                                                    sheet = "Controlling Periods 2022")
#View(ControlFactorTime)


#View(ControlFactorTime)
ControlFactorTime$start <- as.Date(ControlFactorTime$start)
ControlFactorTime$end <- as.Date(ControlFactorTime$end)


library(plotly)
t1 <- vistime(ControlFactorTime, optimize_y = T, col.group = "group", col.color = "color",show_labels = FALSE)
#t2<- t1 %>% layout(title = "Time Series with Custom Date-Time Format", xaxis = list(type = 'date', tickformat = "%Y",autotick = F, dtick = "%m"))

#t2

t1 %>% layout(xaxis=list(fixedrange=TRUE, tickfont=list(size=30, color="black",type = 'date', tickformat = "%Y",autotick = F, dtick = "%m")), 
              yaxis=list(fixedrange=TRUE, tickfont=list(size=30, color="black"), tickangle=30, mirror = FALSE, range = c(0.7, 3.5), showgrid = T))
t1

?layout
?vistime


##################################
############
#####

library(readxl)
BalEx <- read_excel("WY2022/BlanaceVSExcessWY2022.xlsx")
View(BalEx)


b1 <- vistime(BalEx, optimize_y = T, col.group = "group", col.color = "color",,show_labels = FALSE)
#t2<- t1 %>% layout(title = "Time Series with Custom Date-Time Format", xaxis = list(type = 'date', tickformat = "%Y",autotick = F, dtick = "%m"))

#t2

b1 %>% layout(xaxis=list(fixedrange=TRUE, tickfont=list(size=30, color="black",type = 'date', tickformat = "%Y",autotick = F, dtick = "%m")), 
              yaxis=list(fixedrange=TRUE, tickfont=list(size=30, color="black"), tickangle=30, mirror = FALSE, range = c(0.7, 3.5), showgrid = T))
b1
