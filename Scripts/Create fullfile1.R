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
TZA2008$record <- 1

#Find common names
good <- Reduce(intersect, list(names(TZA2008), names(TZA2010), names(TZA2012)))

TZA2008_2 <- TZA2008[, good]
TZA2010_2 <- TZA2010[, good]
TZA2012_2 <- TZA2012[, good]

#New file with all three waves
fullData <- rbind(TZA2008_2, TZA2010_2, TZA2012_2) %>%
  select(hhid2008, indidy1, hhid2010, indidy2, hhid2012, indidy3, everything())

fullData <- mutate(fullData, ed_any= ifelse(ed_any=='Yes', 1,
                                            ifelse(ed_any=='No', 0, NA)))

fullData$fert <- fullData$DAP + 
  fullData$UREA + 
  fullData$TSP + 
  fullData$CAN + 
  fullData$SA + 
  fullData$generic_NPK + 
  fullData$MRP
fullData <- mutate(fullData, fert1 = ifelse(fert >0, 1, 0))
fullData <- select(fullData, -fert)

#Remove NA's from instruments
fullData$subseed1[is.na(fullData$subseed1)] <- 0
fullData$subfert1[is.na(fullData$subfert1)] <- 0
fullData$hybrd1[is.na(fullData$hybrd1)] <-0
fullData$fert1[is.na(fullData$fert1)] <-0

# create zone dummies
fullData <- mutate(fullData, central = ifelse(ZONE=='CENTRAL', 1, 0),
                   eastern = ifelse(ZONE=='EASTERN', 1, 0),
                   lake= ifelse(ZONE=='LAKE', 1, 0),
                   northern= ifelse(ZONE=='NORTHERN', 1, 0),
                   southern= ifelse(ZONE=='SOUTHERN', 1, 0),
                   southern_highlands= ifelse(ZONE=='SOUTHERN-HIGHLANDS', 1, 0),
                   western=ifelse(ZONE=='WESTERN', 1, 0),
                   zanzibar=ifelse(ZONE=='ZANZIBAR', 1, 0))


# put 2010 and 2012 together
good1 <- Reduce(intersect, list(names(TZA2010), names(TZA2012)))
TZA2010_3 <- TZA2010[, good1]
TZA2012_3 <- TZA2012[, good1]
Data2010_2012 <- rbind(TZA2010_3, TZA2012_3) %>%
  select(hhid2008, indidy1, hhid2010, indidy2, hhid2012, indidy3, everything())


#Save
saveRDS(fullData, file.path(dataPath, "fullData.rds"))
saveRDS(Data2010_2012, file.path(dataPath, "Data2010_2012"))

Data2010_2012 <- mutate(Data2010_2012, ed_any= ifelse(ed_any=='Yes', 1,
                                                      ifelse(ed_any=='No', 0, NA)))

Data2010_2012$fert <- Data2010_2012$DAP + 
  Data2010_2012$UREA + 
  Data2010_2012$TSP + 
  Data2010_2012$CAN + 
  Data2010_2012$SA + 
  Data2010_2012$generic_NPK + 
  Data2010_2012$MRP
Data2010_2012 <- mutate(Data2010_2012, fert1 = ifelse(fert >0, 1, 0))
Data2010_2012 <- select(Data2010_2012, -fert)

Data2010_2012 <- mutate(Data2010_2012, central = ifelse(ZONE=='CENTRAL', 1, 0),
                        eastern = ifelse(ZONE=='EASTERN', 1, 0),
                        lake= ifelse(ZONE=='LAKE', 1, 0),
                        northern= ifelse(ZONE=='NORTHERN', 1, 0),
                        southern= ifelse(ZONE=='SOUTHERN', 1, 0),
                        southern_highlands= ifelse(ZONE=='SOUTHERN-HIGHLANDS', 1, 0),
                        western=ifelse(ZONE=='WESTERN', 1, 0),
                        zanzibar=ifelse(ZONE=='ZANZIBAR', 1, 0))
Data2010_2012maize <- filter(Data2010_2012, status %in% "HEAD", zaocode %in% 11)

Data2010_2012maize$subseed1[is.na(Data2010_2012maize$subseed1)] <- 0
Data2010_2012maize$subfert1[is.na(Data2010_2012maize$subfert1)] <- 0
Data2010_2012maize$hybrd1[is.na(Data2010_2012maize$hybrd1)] <-0
Data2010_2012maize$fert1[is.na(Data2010_2012maize$fert1)] <-0

#Select only Maizefarmers
all_maize <- filter(fullData, status %in% "HEAD", zaocode %in% 11)


#AV sum hybrd and fert to district level
all_maize <- group_by(all_maize, DISNAME, surveyyear) %>%
  mutate(dishybrd = mean(hybrd1, na.rm=TRUE)) %>%
  ungroup

all_maize <- group_by(all_maize, DISNAME, surveyyear) %>%
  mutate(disfert = mean(fert1, na.rm=TRUE)) %>%
  ungroup

Data2010_2012maize <- group_by(Data2010_2012maize, DISNAME) %>%
  mutate(dishybrd = mean(hybrd1, na.rm=TRUE)) %>%
  ungroup

Data2010_2012maize <- group_by(Data2010_2012maize, DISNAME) %>%
  mutate(disfert = mean(fert1, na.rm=TRUE)) %>%
  ungroup

#AV insert lag hybrd and fert at district level

lag<- readRDS(file.path(dataPath, "lag"))
Data2010_2012maize <- left_join(Data2010_2012maize, lag); rm(lag)

# get separate files for each year
Data2010maize <-filter(Data2010_2012maize, surveyyear %in% 2010)
Data2012maize <-filter(Data2010_2012maize, surveyyear %in% 2012)
all_maize2008 <- filter(all_maize, surveyyear %in% 2008)                         
all_maize2010 <- filter(all_maize, surveyyear %in% 2010)                        
all_maize2012 <- filter(all_maize, surveyyear %in% 2012)

#save
saveRDS(all_maize, file.path(dataPath, "all_maize"))
saveRDS(all_maize2008, file.path(dataPath, "all_maize2008"))
saveRDS(all_maize2010, file.path(dataPath, "all_maize2010"))
saveRDS(all_maize2012, file.path(dataPath, "all_maize2012"))
saveRDS(Data2010_2012maize, file.path(dataPath, "Data2010_2012maize"))
saveRDS(Data2010maize, file.path(dataPath, "Data2010maize"))
saveRDS(Data2012maize, file.path(dataPath, "Data2012maize"))
saveRDS(fullData, file.path(dataPath, "fullData"))


