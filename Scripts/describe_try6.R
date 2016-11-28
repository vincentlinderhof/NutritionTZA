dataPath <- "C:/Users/afver/OneDrive/Documenten/Removable Disk/verbe038/TZA"
setwd("C:/Users/afver/OneDrive/Documenten/Removable Disk/a.Thesis/Outputs")
library(xlsx)
library(haven)
library(stringr)
library(reshape2)
library(stargazer)
library(plm)
library(plyr)
library(psych)
library(Deducer)
library(dplyr)

c = c("Mean1", "Std1", "N1", "Mean2", "Std2", "N2")
r = c("hybrd1", "fert1" , "fem_head" , "ed_any" , "years", "hhsize" , "dependency" , "hqi", "income",
      "area_tot"  , "asset/100000" ,
      "TLU" , "SACCO" , "central", "eastern", "lake", "northern", "southern", "southern_highlands", "western", "zanzibar", 
      "avgpPrecip" , "avgTemp" , "dist2town" , "dist2market", "subseed", "subfert", "DDS", "FVS", "FCS", "CSI", "off_farm_income_hh")
z = c("Central", "Eastern", "Lake", "Northern", "Southern", "Southern Highlands", "Western", "Zanzibar")

fullData <- readRDS(file.path(dataPath, "fullData"))
all_maize <- readRDS(file.path(dataPath, "all_maize"))
Data2010_2012 <- readRDS(file.path(dataPath, "Data2010_2012"))
zone
#Add maize dummy

#Remove double observations from total sample
detach("package:dplyr", character.only = TRUE)
library("dplyr", character.only = TRUE)
fullData <- select(fullData, hhid2008, hhid2010, hhid2012, status, hybrd1, fert1 , fem_head , ed_any , years, hhsize , dependency ,
       income, area_tot  , asset , hqi ,
       TLU , SACCO , ZONE, central, eastern, lake, northern, southern, southern_highlands, western, zanzibar, 
       avgpPrecip , avgTemp , dist2town , dist2market, DDS, FVS, subseed1, subfert1, m)

Data2010_2012 <- select(Data2010_2012, hhid2008, hhid2010, hhid2012, status, hybrd1, fert1 , fem_head , ed_any , years, hhsize , dependency ,
                   income, off_farm_income_hh, area_tot  , asset , hqi ,
                   TLU , SACCO , ZONE, central, eastern, lake, northern, southern, southern_highlands, western, zanzibar, 
                   avgpPrecip , avgTemp , dist2town , dist2market, DDS, FVS, FCS, CSI, subseed1, subfert1, m)

all_maize <- select(all_maize, hhid2008, hhid2010, hhid2012, status, hybrd1, fert1 , fem_head , ed_any , years, hhsize , dependency ,
                    income, area_tot  , asset , hqi ,
                    TLU , SACCO , ZONE, central, eastern, lake, northern, southern, southern_highlands, western, zanzibar, 
                    avgpPrecip , avgTemp , dist2town , dist2market, DDS, FVS, subseed1, subfert1, m )

Data2010_2012maize <- select(Data2010_2012maize, hhid2008, hhid2010, hhid2012, status, hybrd1, fert1 , fem_head , ed_any , years, hhsize , dependency ,
                             income, off_farm_income_hh, area_tot  , asset , hqi ,
                             TLU , SACCO , ZONE, central, eastern, lake, northern, southern, southern_highlands, western, zanzibar, 
                             avgpPrecip , avgTemp , dist2town , dist2market, DDS, FVS, FCS, CSI, subseed1, subfert1, m)

fullData <- filter(fullData, status %in% "HEAD")
Data2010_2012 <- filter(Data2010_2012, status %in% "HEAD")
all_maize <- unique(all_maize)
fullData <- unique(fullData)
Data2010_2012 <- unique(Data2010_2012)
Data2010_2012maize <- unique(Data2010_2012maize)

#General table
#full data describe
full <- descriptive.table(vars = d(hybrd1, fert1 , fem_head , ed_any , years, hhsize , dependency , hqi, income/100000, 
                                   area_tot  , asset/100000 ,
                                   TLU , SACCO , central, eastern, lake, northern, southern, southern_highlands, western, zanzibar, 
                                   avgpPrecip , avgTemp , dist2town , dist2market, subseed1, subfert1, DDS, FVS),data= fullData, func.names =c("Mean","St. Deviation", "Min", "Max", "Skew", "Valid N"))
FCS_CSI <-descriptive.table(vars = d(FCS, CSI, off_farm_income_hh/100000),data= Data2010_2012, func.names =c("Mean","St. Deviation", "Min", "Max", "Skew", "Valid N"))

stargazer(full, FCS_CSI, out="full.htm")

#maize and full

full <- descriptive.table(vars = d(hybrd1, fert1 , fem_head , ed_any, years, hhsize , dependency , hqi, income/100000, 
                                    area_tot  , asset/100000 , 
                                    TLU , SACCO , central, eastern, lake, northern, southern,
                                    southern_highlands, western, zanzibar,avgpPrecip , avgTemp , dist2town , dist2market, subseed1, subfert1, DDS, FVS), strata=d(m), data= fullData, func.names =c("Mean","St. Deviation","Valid N"))

full1 <- descriptive.table(vars = d(FCS, CSI, off_farm_income_hh), strata=d(m), data=Data2010_2012, func.names =c("Mean","St. Deviation","Valid N"))


full1 <- descriptive.table(vars = d(FCS, CSI, off_farm_income_hh/100000), strata=d(m), data=Data2010_2012, func.names =c("Mean","St. Deviation","Valid N"))

fulldf<-as.data.frame(full,stringsAsFactors=FALSE)
full1df<-as.data.frame(full1,stringsAsFactors=FALSE)
fulldf <- rbind(fulldf, full1df); rm(full1df)
colnames(fulldf) <-c


fulldf <- mutate(fulldf, a= Mean1*N1, b=Mean2*N2, c=(1-Mean1)*N1, d=(1-Mean2)*N2)
fulldf <- mutate(fulldf, chi2=((a*d-b*c)^2*12199)/((a+b)*(c+d)*N1*N2)) %>% select(-a, -b, -c, -d)
fulldf <- mutate(fulldf, test='*')


fulldf <- mutate(fulldf, t=(Mean1-Mean2)/sqrt((Std1^2)/N1+(Std2^2)/N2), 
                 p=pt(-abs(t),df=pmin(N1-1,N2-1)))
rownames(fulldf)<-r

write.xlsx(fulldf, "fulldf.xlsx", row.names = TRUE)

#critical value p chi
qchisq(0.90, df=1)
qchisq(0.95, df=1)
qchisq(0.99, df=1)
#adopt vs non adopt 

adopth <- descriptive.table(vars = d(hybrd1, fert1 , fem_head , ed_any, years, hhsize , dependency , hqi, income/100000, 
                                   area_tot  , asset/100000 , 
                                   TLU , SACCO , central, eastern, lake, northern, southern,
                                   southern_highlands, western, zanzibar,avgpPrecip , avgTemp , dist2town , dist2market, subseed1, subfert1, DDS, FVS), strata=d(hybrd1), data= all_maize, func.names =c("Mean","St. Deviation","Valid N"))


adopth1 <- descriptive.table(vars = d(FCS, CSI, off_farm_income_hh/100000), strata=d(hybrd1), data=Data2010_2012maize, func.names =c("Mean","St. Deviation","Valid N"))

adopthdf<-as.data.frame(adopth,stringsAsFactors=FALSE)
adopth1df<-as.data.frame(adopth1,stringsAsFactors=FALSE)
adopthdf <- rbind(adopthdf, adopth1df); rm(adopth1df)
colnames(adopthdf) <-c


adopthdf <- mutate(adopthdf, a= Mean1*N1, b=Mean2*N2, c=(1-Mean1)*N1, d=(1-Mean2)*N2)
adopthdf <- mutate(adopthdf, chi2=((a*d-b*c)^2*12199)/((a+b)*(c+d)*N1*N2)) %>% select(-a, -b, -c, -d)

adopthdf <- mutate(adopthdf, t=(Mean1-Mean2)/sqrt((Std1^2)/N1+(Std2^2)/N2), 
                   p=pt(-abs(t),df=pmin(N1-1,N2-1)))
rownames(adopthdf)<-r

write.xlsx(adopthdf, "adopthdf.xlsx", row.names = TRUE)

#By zone

zone <-descriptive.table(vars = d(hybrd1, fert1,  DDS, FVS), 
                         strata=d(ZONE), data= all_maize, func.names =c("Mean"))
zone1 <-descriptive.table(vars = d(FCS, CSI), strata=d(ZONE), 
                          data=Data2010_2012maize, func.names =c("Mean"))
zonedf<-as.data.frame(zone,stringsAsFactors=FALSE)
zone1df<-as.data.frame(zone1,stringsAsFactors=FALSE)
zonedf <- rbind(zonedf, zone1df); rm(zone1df)
colnames(zonedf) <- z 
write.xlsx(zonedf, "zonedf.xlsx", row.names = TRUE)
table(all_maize$ZONE)
