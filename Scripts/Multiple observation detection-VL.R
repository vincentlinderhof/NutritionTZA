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

#Open three waves
TZA2008 <- readRDS(file.path(dataPath, "TZA2008.rds"))
TZA2010 <- readRDS(file.path(dataPath, "TZA2010.rds"))
TZA2012 <- readRDS(file.path(dataPath, "TZA2012.rds"))

#ADD VL
Data2010_2012maize <-readRDS(file.path(dataPath, "Data2010_2012maize"))

Data2010_2012maize$dubbel10 <- 
  ifelse(is.na(Data2010_2012maize$hhid2010),0, 
      ifelse(Data2010_2012maize$hhid2010==lag(Data2010_2012maize$hhid2010),2,1))
Data2010_2012maize$dubbel10 <- ifelse(is.na(Data2010_2012maize$dubbel10),1,Data2010_2012maize$dubbel10)
table(Data2010_2012maize$dubbel10,Data2010_2012maize$surveyyear)

Data2010_2012maize$dubbel12 <- 
  ifelse(is.na(Data2010_2012maize$hhid2012),0, 
         ifelse(Data2010_2012maize$hhid2012==lag(Data2010_2012maize$hhid2012),2,1))
Data2010_2012maize$dubbel12 <- ifelse(is.na(Data2010_2012maize$dubbel12),1,Data2010_2012maize$dubbel12)
table(Data2010_2012maize$dubbel12,Data2010_2012maize$surveyyear)

Data2010_2012maize$dubbel <- 
  ifelse(Data2010_2012maize$surveyyear==2010, Data2010_2012maize$dubbel10, Data2010_2012maize$dubbel12)

newdata <- Data2010_2012maize[c("hhid2010", "hhid2012", "dubbel", "dubbel10", "dubbel12","DAP", "UREA", "TSP", "surveyyear")]
table(Data2010_2012maize$dubbel,Data2010_2012maize$surveyyear)
rm(newdata)

Data2010_2012 <-subset(Data2010_2012maize, dubbel==1)

