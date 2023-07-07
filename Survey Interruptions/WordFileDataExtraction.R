##Code by Nick Bertrand
#nbertrand@usbr.gov

#this script will extract the 
#survey interrruptions table from the weekly outlooks from the whole season 


library(docxtractr)
library(tidyverse)

#########################################
#A function to iterate through multiple files in a directory
#creates the dataframe to dump the data in
survey <- data.frame()
#reached to the directory with all the Outlook .docx files and creates a list of file names 
fileNames <- Sys.glob("C:/Users/nbertrand/Desktop/Bertrand/GitHub/OMRSeasonalReport/omr_report_2023/Survey Interruptions/WY2023Outlooks/*.docx")
#view(fileNames)

#function uses file name list
for (fileName in fileNames) {
  
  # to read in the data
  doc <- read_docx(fileName)
  #data is extracted based on table number
  data1 <- docx_extract_tbl(doc, 8)
  
  data2 <- data1 %>% mutate(week = names(data1)[3]) 
  
  data2$week <- gsub("Notes..as.of.", "", data2$week)
  #'.' does not work but for some reason '\\.' does as described by stack overflow
  data2$week <- gsub('\\.','/',data2$week)
  data2$week <- as.Date(data2$week, "%m/%d/%y")
  #renames Columns
  data3 <- data2 %>%rename(Survey = 1, Region = 2, Notes = 3, Status = 4, Week = 5)
  #binds extracted data back to the dump dataframe
  survey <- rbind(survey,data3)
}

View(survey)
#' #########################################
#' #extracts a table from one file, just wanted to save this code
#' #git hub example for read_docx is broken, code below worked
#' doc <- read_docx("C:/Users/nbertrand/Desktop/Bertrand/GitHub/OMRSeasonalReport/omr_report_2023/Survey Interruptions/WY2023Outlooks/20230627 fish and water operations outlook.docx")
#' 
#' #assigns number to all tables by counting
#' docx_tbl_count(doc)
#' #use number assigned in count to extract a specific table.
#' data <- docx_extract_tbl(doc, 8) %>% 
#'   mutate(week = names(data)[3]) 
#' data$week <- gsub("Notes..as.of.", "", data$week)
#' #'.' does not work but for some reason '\\.' does as described by stack overflow
#' data$week <- gsub('\\.','/',data$week)
#' data$week <- as.Date(data$week, "%m/%d/%y")
#' #renames Columns
#' data <- data %>%rename(Survey = 1, Region = 2, Notes = 3, Status = 4, Week = 5)
#' 
#' view(data)
#' 

########################







