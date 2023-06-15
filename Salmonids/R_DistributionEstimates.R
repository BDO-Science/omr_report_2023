library(tidyverse)
library(readxl)

# Load distribution estimate data
distribution_data<-read_excel(file.path("data","DistributionEstimates_WOMT_WY2023.xlsx"),sheet="DATA Dist WOMT Export OMRrange")
str(distribution_data)

# Split range data into upper and lower dist estimates
dist_est_split <- function(datafile=distribution_data,old_col="Natural_WR_YTE_Range",newcol1="Natural_WR_YTE_Lower",newcol2="Natural_WR_YTE_Upper"){
  distribution_data_newdata <- separate(datafile, col=old_col,
                                     c("dummy1", "dummy2"), sep="-",extra = "drop") %>% 
    mutate(dummy2=gsub("\\%", "", dummy2)) %>%
    mutate(dummy1=as.numeric(dummy1),dummy2=as.numeric(dummy2)) 
  
  colnames(distribution_data_newdata)[colnames(distribution_data_newdata) == "dummy1"] = newcol1
  colnames(distribution_data_newdata)[colnames(distribution_data_newdata) == "dummy2"] = newcol2
  
  return(distribution_data_newdata)
}

# Create new columns for upper and lower
distribution_data_edit <- dist_est_split()
distribution_data_edit <- dist_est_split(datafile=distribution_data_edit,old_col="Natural_WR_ID_Range",newcol1="Natural_WR_ID_Lower",newcol2="Natural_WR_ID_Upper")
distribution_data_edit <- dist_est_split(datafile=distribution_data_edit,old_col="Natural_WR_E_Range",newcol1="Natural_WR_E_Lower",newcol2="Natural_WR_E_Upper")

distribution_data_edit <- dist_est_split(datafile=distribution_data_edit,old_col="Natural_SR_YTE_Range",newcol1="Natural_SR_YTE_Lower",newcol2="Natural_SR_YTE_Upper")
distribution_data_edit <- dist_est_split(datafile=distribution_data_edit,old_col="Natural_SR_ID_Range",newcol1="Natural_SR_ID_Lower",newcol2="Natural_SR_ID_Upper")
distribution_data_edit <- dist_est_split(datafile=distribution_data_edit,old_col="Natural_SR_E_Range",newcol1="Natural_SR_E_Lower",newcol2="Natural_SR_E_Upper")

distribution_data_edit <- dist_est_split(datafile=distribution_data_edit,old_col="Hatch_WR_YTE_Range",newcol1="Hatch_WR_YTE_Lower",newcol2="Hatch_WR_YTE_Upper")
distribution_data_edit <- dist_est_split(datafile=distribution_data_edit,old_col="Hatch_WR_ID_Range",newcol1="Hatch_WR_ID_Lower",newcol2="Hatch_WR_ID_Upper")
distribution_data_edit <- dist_est_split(datafile=distribution_data_edit,old_col="Hatch_WR_E_Range",newcol1="Hatch_WR_E_Lower",newcol2="Hatch_WR_E_Upper")

distribution_data_edit <- dist_est_split(datafile=distribution_data_edit,old_col="Natural_SH_YTE_Range",newcol1="Natural_SH_YTE_Lower",newcol2="Natural_SH_YTE_Upper")
distribution_data_edit <- dist_est_split(datafile=distribution_data_edit,old_col="Natural_SH_ID_Range",newcol1="Natural_SH_ID_Lower",newcol2="Natural_SH_ID_Upper")
distribution_data_edit <- dist_est_split(datafile=distribution_data_edit,old_col="Natural_SH_E_Range",newcol1="Natural_SH_E_Lower",newcol2="Natural_SH_E_Upper")

#Load monitoring csv data from SacPAS
#https://www.cbr.washington.edu/sacramento/data/query_sampling_graph.html
monitoring_data_raw <- read.csv(file.path("data","samplingdaily_1686867510_630.csv"))
str(monitoring_data_raw)

#Calculate percentage of sum
monitoring_data_WR <- monitoring_data_raw %>% rename(
  KL_RST=Raw.Knights.Landing.RST,
  SacSeine=Raw.Sacramento.Beach.Seines..SR080E.SR071E.SR062E.SR057E.SR055E.SR060E.AM001S.SR049E.,
  SacTrawl=Raw.Sacramento.Trawls..SR055M.SR055E.SR055W.SR055X.,
  ChippsTrawl=Raw.Chipps.Island.Trawls..SB018M.SB018N.SB018S.SB018X.
) %>% select(Date,KL_RST,SacSeine,SacTrawl,ChippsTrawl) %>%
  mutate(Date=as.Date(Date)) %>% rowwise() %>% mutate(SacTrawl_Seine= sum(SacSeine, SacTrawl, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(Date)) %>% select(-c(SacSeine, SacTrawl)) %>%
  mutate(KL_RST = replace_na(KL_RST, 0),ChippsTrawl=replace_na(ChippsTrawl, 0)) %>%
  mutate(KL_RST_sum=cumsum(KL_RST),ChippsTrawl_sum=cumsum(ChippsTrawl),SacTrawl_Seine_sum=cumsum(SacTrawl_Seine)) %>%
  mutate(KL_RST_sum=KL_RST_sum/max(KL_RST_sum),ChippsTrawl_sum=ChippsTrawl_sum/max(ChippsTrawl_sum),SacTrawl_Seine_sum=SacTrawl_Seine_sum/max(SacTrawl_Seine_sum)) 

monitoring_data_WR <- monitoring_data_WR %>% select(Date,KL_RST_sum,ChippsTrawl_sum,SacTrawl_Seine_sum) %>%
  gather("Category","Percent",2:4)

#Create figure for Natural Winter-run LAD distribution estimates
str(distribution_data_edit)
data_WR_nat<-distribution_data_edit %>% select(Date,Natural_WR_YTE,Natural_WR_ID,Natural_WR_E) %>% gather("Category","Percent",2:4)

plot_distest_WR_nat <- ggplot() +  
  geom_line(data=data_WR_nat, aes(Date, Percent, colour=Category)) + 
  geom_ribbon(data=distribution_data_edit,aes(x=Date, ymax=Natural_WR_YTE_Upper, ymin=Natural_WR_YTE_Lower), 
              alpha=0.2,fill="blue")+
  geom_ribbon(data=distribution_data_edit,aes(x=Date, ymax=Natural_WR_ID_Upper, ymin=Natural_WR_ID_Lower), 
              alpha=0.2,fill="green")+
  geom_ribbon(data=distribution_data_edit,aes(x=Date, ymax=Natural_WR_E_Upper, ymin=Natural_WR_E_Lower), 
              alpha=0.2,fill="red")+
  ylim(-5, 105)
distest_WR_nat

plot_monitoring_WR_nat <- ggplot() +  
  geom_line(data=monitoring_data_WR, aes(Date, Percent, colour=Category))
plot_monitoring_WR_nat


###################################################################3
df <- data.frame(x = c(NA, "x.y", "x.z", "y.z"))
str(df)
df %>% separate(x, c("A", "B"))

at <- seq(from = min(distribution_data$Date), to = max(distribution_data$Date), by = "month")

#Natural winter-run
tiff(filename="Fig_NaturalWinter_DistributionEstimates.tiff",width=10,height = 8, units = "in",  res=300, compression ="lzw")

plot(Natural_WR_YTE ~ Date, distribution_data, xaxt = "n", type = "l", col="black",main="Natural Winter-Run",ylab="Percentage of Population",xlab="")
lines(distribution_data$Date, distribution_data$Natural_WR_ID, col = "black", type = "l", lty = 2)
lines(distribution_data$Date, distribution_data$Natural_WR_E, col = "black", type = "l", lty = 3)
axis(1, at=at, format(at, "%b %d"), cex.axis = .7)
legend(min(distribution_data$Date), 50, legend=c("Yet to enter Delta", "In Delta", "Exited Delta"),
       col=c("black", "black",'black'), lty=1:3, cex=0.8)

dev.off()

#Natural Spring-run
tiff(filename="Fig_NaturalSpring_DistributionEstimates.tiff",width=10,height = 8, units = "in",  res=300, compression ="lzw")

plot(Natural_SR_YTE ~ Date, distribution_data, xaxt = "n", type = "l", col="black",main="Natural Spring-Run",ylab="Percentage of Population",xlab="")
lines(distribution_data$Date, distribution_data$Natural_SR_ID, col = "black", type = "l", lty = 2)
lines(distribution_data$Date, distribution_data$Natural_SR_E, col = "black", type = "l", lty = 3)
axis(1, at=at, format(at, "%b %d"), cex.axis = .7)
legend(min(distribution_data$Date), 50, legend=c("Yet to enter Delta", "In Delta", "Exited Delta"),
       col=c("black", "black",'black'), lty=1:3, cex=0.8)

dev.off()


#Natural Steelhead
tiff(filename="Fig_NaturalSteelhead_DistributionEstimates.tiff",width=10,height = 8, units = "in",  res=300, compression ="lzw")

plot(Natural_SH_YTE ~ Date, distribution_data, xaxt = "n", type = "l", col="black",main="Natural Steelhead",ylab="Percentage of Population",xlab="")
lines(distribution_data$Date, distribution_data$Natural_SH_ID, col = "black", type = "l", lty = 2)
lines(distribution_data$Date, distribution_data$Natural_SH_E, col = "black", type = "l", lty = 3)
axis(1, at=at, format(at, "%b %d"), cex.axis = .7)
legend(min(distribution_data$Date), 50, legend=c("Yet to enter Delta", "In Delta", "Exited Delta"),
       col=c("black", "black",'black'), lty=1:3, cex=0.8)

dev.off()

#Hatchery Winter-run
tiff(filename="Fig_HatcheryWinter_DistributionEstimates.tiff",width=10,height = 8, units = "in",  res=300, compression ="lzw")

plot(Hatch_WR_YTE ~ Date, distribution_data, xaxt = "n", type = "l", col="black",main="Hatchery Winter-Run",ylab="Percentage of Population",xlab="")
lines(distribution_data$Date, distribution_data$Hatch_WR_ID, col = "black", type = "l", lty = 2)
lines(distribution_data$Date, distribution_data$Hatch_WR_E, col = "black", type = "l", lty = 3)
axis(1, at=at, format(at, "%b %d"), cex.axis = .7)
legend(min(distribution_data$Date), 50, legend=c("Yet to enter Delta", "In Delta", "Exited Delta"),
       col=c("black", "black",'black'), lty=1:3, cex=0.8)

dev.off()