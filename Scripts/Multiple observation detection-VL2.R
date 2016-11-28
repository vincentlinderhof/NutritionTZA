if(Sys.info()["user"] == "Tomas"){
  dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA/2010/Data" }
if(Sys.info()["user"] == "linde069"){
  dataPath <- "D:/Analyses/CIMMYT/TZA_Anne/Data/"
  setwd("D:/Analyses/CIMMYT/TZA_Anne")}
#dataPath <- "C:/Users/afver/OneDrive/Documenten/Removable Disk/verbe038/TZA"

library(haven)
library(stringr)
library(reshape2)
library(dplyr)
library(stargazer)
library(stats)
library(zoo)
#######################################################################
############# Taking out the double observation of households #########  
#######################################################################

#ADD VL
Data2 <-readRDS(file.path(dataPath, "Data2010_2012maize"))
table(Data2$surveyyear)

#Data2$dubbel10 <-          ifelse(Data2$hhid2010==lag(Data2$hhid2010), 2, 1) 
order(Data2, method=c("surveyyear","hhid"))

Data2$dubbel10 <- 
  ifelse(is.na(Data2$hhid2010),0, 
      ifelse(Data2$hhid2010==lag(Data2$hhid2010,-1), 2, 1)) 
newdata <- Data2[c("hhid2010", "hhid2012",  "dubbel10", "DAP", "UREA", "TSP", "surveyyear")]
Data2$dubbel10 <- ifelse(is.na(Data2$dubbel10),1,Data2$dubbel10)
table(Data2$dubbel10,Data2$surveyyear)
newdata <- Data2[c("hhid2010", "hhid2012",  "dubbel10", "DAP", "UREA", "TSP", "surveyyear")]
newdata$var_test <- lag(newdata$hhid2010,1)

Data2$dubbel12 <- 
  ifelse(is.na(Data2$hhid2012),0, 
         ifelse(Data2$hhid2012==lag(Data2$hhid2012),2,1))
Data2$dubbel12 <- ifelse(is.na(Data2$dubbel12),1,Data2$dubbel12)
table(Data2$dubbel12,Data2$surveyyear)

Data2$dubbel <- 
  ifelse(Data2$surveyyear==2010, Data2$dubbel10, Data2$dubbel12)

newdata <- Data2[c("hhid2010", "hhid2012", "dubbel", "dubbel10", "dubbel12","DAP", "UREA", "TSP", "surveyyear")]
table(Data2$dubbel,Data2$surveyyear)
rm(newdata)

Data2010_2012maize <-subset(Data2010_2012maize, dubbel==1)
rm(Data2)
