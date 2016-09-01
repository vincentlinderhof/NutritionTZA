dataPath <- "C:/Users/afver/OneDrive/Documenten/Removable Disk/verbe038/TZA"


library(haven)
library(stringr)
library(reshape2)
library(dplyr)
library(stargazer)
library(plm)
library(plyr)
library(psych)
library(Deducer)


all_maize2008 <- readRDS(file.path(dataPath, "all_maize2008"))
all_maize2010 <- readRDS(file.path(dataPath, "all_maize2010"))
all_maize2012 <- readRDS(file.path(dataPath, "all_maize2012"))
all_maize <- readRDS(file.path(dataPath, "all_maize"))
Data2010_2012maize <- readRDS(file.path(dataPath, "Data2010_2012maize"))

#Remove double observations from total sample

fullData <- readRDS(file.path(dataPath, "fullData")) %>%
  select(hhid2008, hhid2010, hhid2012, status, hybrd1, fert1 , fem_head , ed_any , years, hhsize , dependency ,
         area_tot  , asset , hqi ,
         TLU , SACCO , central, eastern, lake, northern, southern, southern_highlands, western, zanzibar, 
         avgpPrecip , avgTemp , dist2town , dist2market, DDS, FVS)
fullData <- filter(fullData, status=="HEAD")
fullData <- unique(fullData)


Data2010_2012 <- readRDS(file.path(dataPath, "Data2010_2012")) %>%
  select(hhid2008, hhid2010, hhid2012, status, FCS, CSI)
Data2010_2012 <- filter(Data2010_2012, status=="HEAD")
Data2010_2012 <- unique(Data2010_2012)


#Maize compared to full
full <- descriptive.table(vars = d(hybrd1, fert1 , fem_head , ed_any , years, hhsize , dependency ,
                          area_tot  , asset/100000 , hqi ,
                          TLU , SACCO , central, eastern, lake, northern, southern, southern_highlands, western, zanzibar, 
                          avgpPrecip , avgTemp , dist2town , dist2market, DDS, FVS),data= fullData, func.names =c("Mean","St. Deviation", "Min", "Max", "Skew", "Valid N"))


maize <- descriptive.table(vars = d(hybrd1, fert1 , fem_head , ed_any , years, hhsize , dependency ,
                                    area_tot  , asset/100000 , hqi ,
                                    TLU , SACCO , central, eastern, lake, northern, southern, southern_highlands, western, zanzibar, avgpPrecip , avgTemp , dist2town , dist2market, DDS, FVS),data=all_maize, func.names =c("Mean","St. Deviation","Valid N"))
FCS_CSI <-descriptive.table(vars = d(FCS, CSI),data= Data2010_2012, func.names =c("Mean","St. Deviation", "Min", "Max", "Skew", "Valid N"))
FCS_CSImaize <- descriptive.table(vars = d(FCS, CSI),data= Data2010_2012maize, func.names =c("Mean","St. Deviation","Valid N"))
stargazer(full, maize, FCS_CSI, FCS_CSImaize, out="describe_maize.htm")
stargazer(full, maize, FCS_CSI, FCS_CSImaize, out="describe_maize.htm")
stargazer(full, FCS_CSI,out="full.htm")

#by year

desc2012 <- descriptive.table(vars = d(hybrd1, fert1 , fem_head , ed_any , years, hhsize , dependency ,
                                       area_tot  , asset/100000 , hqi ,
                                       TLU , SACCO , central, eastern, lake, northern, southern, 
                                       southern_highlands, western, zanzibar, avgpPrecip , avgTemp , dist2town , dist2market, DDS, FVS, FCS, CSI), strata=d(surveyyear), data= Data2010_2012maize, func.names =c("Mean","St. Deviation","Valid N"))
desc2008 <- descriptive.table(vars = d(hybrd1, fert1 , fem_head , ed_any , years, hhsize , dependency ,
                                       area_tot  , asset/100000 , hqi ,
                                       TLU , SACCO , central, eastern, lake, northern, southern, 
                                       southern_highlands, western, zanzibar, avgpPrecip , avgTemp , dist2town , dist2market, DDS, FVS), strata=d(surveyyear),data= all_maize2008, func.names =c("Mean","St. Deviation","Valid N"))

stargazer(desc2012, desc2008, out="describe_year.htm")

#by hybrd and fert

hybrd <- descriptive.table(vars = d(hybrd1, fert1 , fem_head , ed_any, years, hhsize , dependency ,
                                       area_tot  , asset/100000 , hqi ,
                                       TLU , SACCO , central, eastern, lake, northern, southern,                                         southern_highlands, western, zanzibar,avgpPrecip , avgTemp , dist2town , dist2market, DDS, FVS), strata=d(hybrd1), data= all_maize, func.names =c("Mean","St. Deviation","Valid N"))
hybrd1 <- descriptive.table(vars = d(FCS, CSI), strata=d(hybrd1), data=Data2010_2012maize, func.names =c("Mean","St. Deviation","Valid N"))

fert <- descriptive.table(vars = d(hybrd1, fert1 , fem_head , ed_any, years, hhsize , dependency ,
                                       area_tot  , asset/100000 , hqi ,
                                       TLU , SACCO , central, eastern, lake, northern, southern,                                         southern_highlands, western, zanzibar,avgpPrecip , avgTemp , dist2town , dist2market, DDS, FVS), strata=d(fert1),data= all_maize, func.names =c("Mean","St. Deviation","Valid N"))
fert1 <- descriptive.table(vars = d(FCS, CSI), strata=d(fert1), data=Data2010_2012maize, func.names =c("Mean","St. Deviation","Valid N"))

stargazer(hybrd, hybrd1, fert, fert1, out="adopt.htm")

zone <-descriptive.table(vars = d(hybrd1, fert1,  DDS, FVS), strata=d(ZONE), data= all_maize, func.names =c("Mean", "Valid N"))
zone1 <-descriptive.table(vars = d(FCS, CSI), strata=d(ZONE), data=Data2010_2012maize, func.names =c("Mean"))
zone
stargazer(zone, out="zone.html")
stargazer(zone1, out="zone1.html")
zone
