#######################################
########## TANZANIA 2010-11 ###########
#######################################

#rm(list=ls() )
if(Sys.info()["user"] == "Tomas"){
  dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA/2010/Data" }
if(Sys.info()["user"] == "linde069"){
  dataPath <- "D:/Analyses/CIMMYT/TZA_Anne/SurveyData/2010/Data"
  setwd("D:/Analyses/CIMMYT/TZA_Anne")}

# Anne
#dataPath <- dataPath <- "D:/UserData/verbe038/TZA/2010/Data"
#dataPath <- dataPath <- "C:/Users/afver/OneDrive/Documenten/Removable Disk/verbe038/TZA/2010/Data"

library(haven)
library(stringr)
library(reshape2)
library(dplyr)
library(tidyr)

options(scipen=999)

#######################################
############## LOCATION ###############
#######################################

# file containing the zone, region and district

location <- read_dta(file.path(dataPath, "TZNPS2HH1DTA/HH_SEC_A.dta")) %>%
  select(hhid=y2_hhid, REGCODE = region, DISCODE = district, rural = y2_rural)
location$rural <- as.integer(location$rural)
location$REGCODE <- as.integer(location$REGCODE)
location$DISCODE <- as.integer(location$DISCODE)

# match up with the names from the survey (prepared in a seperate file)

if(Sys.info()["user"] == "Tomas"){
  ZONEREGDIS <- read.csv(file.path(paste0(dataPath,"/../.."), "ZONEREGDIS.csv"))
}
if(Sys.info()["user"] == "linde069"){
  ZONEREGDIS <- read.csv("SurveyData/Other/Spatial/TZA/ZONEREGDIS.csv")
}
# join with household identifications

location <- left_join(location, ZONEREGDIS)

saveRDS(location, "Data/Location_2010.rds")

rm(ZONEREGDIS)

#######################################
########### SOCIO/ECONOMIC ############
#######################################

source("Scripts/socioecon_TZA_2010.r")

# Income
source("Scripts/income_TZA_2010.r")
#income_2010 <- readRDS(file.path(dataPath, "income_2010"))


#######################################
########### Housing quality ###########
#######################################

# AV: make new variables: HQI Housing Quality Index
source("Scripts/HQI_TZA_2010.r")

#######################################
############### OUTPUT ################
#######################################

oput <- read_dta(file.path(dataPath, "TZNPS2AGRDTA/AG_SEC4A.dta")) %>%
  select(hhid=y2_hhid, plotnum, zaocode, inter_crop=ag4a_04,
         harv_area=ag4a_08, qty=ag4a_15, value=ag4a_16, hybrd=ag4a_23, subseed=ag4a_20)

oput$inter_crop <- ifelse(oput$inter_crop %in% 1, 1, 0)
oput$hybrd <- ifelse(oput$hybrd %in% 2, 1, 0)
oput$zaocode <- as.integer(oput$zaocode)
oput$subseed <- as.integer(oput$subseed)
oput$subseed <- ifelse(oput$subseed %in% 2, 1, 0)
# -------------------------------------
# create dummy variables for crop groups
# (fruit, cash crops (permanent),
# Cereals/Tubers/Roots, cash crops (not permanent),
# vegetables, legumes)
# -------------------------------------

fruit <- c(70:74, 76:85, 97:99, 67, 38, 39)
cashCropsPerm <- c(53:61, 63:66, 18, 34, 21, 75, 44:46) # permanent cash crops
CTR <- c(11:17, 22:27) # Cereals, Tubers, Roots
cashCropNPerm <- c(50, 51, 53, 62, 19) # non permanent cash crops
vegetables <- c(86:96, 100, 101)
legumes <- c(31, 32, 33, 35, 36, 37, 41, 42, 43, 47, 48)

oput_x <- group_by(oput, hhid, plotnum) %>%
  summarise(crop_count=length(unique(zaocode[!is.na(zaocode)])),
            fruit=ifelse(any(zaocode %in% fruit), 1, 0),
            cashCropsPerm=ifelse(any(zaocode %in% cashCropsPerm), 1, 0),
            CTR=ifelse(any(zaocode %in% CTR), 1, 0),
            cashCropNPerm=ifelse(any(zaocode %in% cashCropNPerm), 1, 0),
            vegetables=ifelse(any(zaocode %in% vegetables), 1, 0),
            legume=ifelse(any(zaocode %in% legumes), 1, 0),
            maize_=ifelse(any(zaocode %in% 11), 1, 0), # maize has crop code 11
            wheat=ifelse(any(zaocode %in% 16), 1, 0)) # wheat has crop code 16

oput <- left_join(oput, oput_x); rm(oput_x)

oput$maize_harv <- ifelse(oput$zaocode %in% 11, oput$harv_area, 0)
oput$maize_qty  <- ifelse(oput$zaocode %in% 11, oput$qty, 0)
oput$maize_value <- ifelse(oput$zaocode %in% 11, oput$value, 0) 
summary(oput$value)

oput$wheat_harv <- ifelse(oput$zaocode %in% 16, oput$harv_area, 0)
oput$wheat_qty  <- ifelse(oput$zaocode %in% 16, oput$qty, 0)
oput$wheat_value <- ifelse(oput$zaocode %in% 16, oput$value, 0) 

# in the maize farmers, exclude farmers
# who responded they produced zero crop, or did not respond (NA)
#VL: added
oputHH <- group_by(oput, hhid) %>%
  summarise(inter_crop   =max(inter_crop),
            hybrd        =max(hybrd),
            crop_count   =max(crop_count),
            fruit        =max(fruit),
            cashCropsPerm=max(cashCropsPerm),
            CTR          =max(CTR), 
            cashCropNPerm=max(cashCropNPerm),
            legume       =max(legume),
            maize        =max(maize_),
            wheat        =max(wheat),
            maize_harv   =sum(maize_harv),
            maize_qty    =sum(maize_qty),
            maize_value  =sum(maize_value), 
            wheat_harv   =sum(wheat_harv), 
            wheat_qty    =sum(wheat_qty), 
            wheat_value  =sum(wheat_value) ) 

oput <- oput[! is.na(oput$qty) & !oput$qty %in% 0, ]
oput$crop_price <- oput$value/oput$qty
oput$value <- NULL 

rm(list=c("legumes", "cashCropNPerm", "cashCropsPerm",
          "CTR", "fruit", "vegetables"))

#######################################
############# CHEMICAL ################
#######################################

plot <- read_dta(file.path(dataPath, "TZNPS2AGRDTA/AG_SEC3A.dta")) %>%
  dplyr::select(hhid=y2_hhid, plotnum, zaocode, soil=ag3a_09, slope_farmer=ag3a_16, irrig=ag3a_17, title=ag3a_27,
                manure=ag3a_39, pest=ag3a_58, pest_q=ag3a_60_1, pest_q_unit=ag3a_60_2, fallow_year=ag3a_21, fallow=ag3a_22, inorg=ag3a_45)

plot$zaocode <- as.integer(plot$zaocode)
plot$maize <- ifelse(plot$zaocode %in% 11, 1, 0)
plot$soil <- factor(plot$soil, levels=c(1,2,3,4), labels=c("Sandy", "Loam", "Clay", "Other"))
plot$slope_farmer <- factor(plot$slope_farmer, levels=c(1,2,3,4), labels=c("Flat bottom", "Flat top", "Slightly sloped", "Very steep"))
plot$title <- ifelse(plot$title %in% 1, 1, 0) # assume that they don't have a title if NA
plot$irrig <- ifelse(plot$irrig %in% 1, 1, 0)
plot$manure <- ifelse(plot$manure %in% 1, 1, 0)
plot$pest <- ifelse(plot$pest %in% 1, 1, 0)
plot$pest_q_unit <- as_factor(plot$pest_q_unit)
plot$inorg <- as_factor(plot$inorg)

plot$pest_q <- ifelse(plot$pest_q_unit %in% c("LITRE", "KG"), plot$pest_q,
                      ifelse(plot$pest_q_unit %in% "MILLILITRE", plot$pest_q*0.001, NA))


# two questions on fallow - make sure they match up correctly
# fallow value of 98 means subject did not know how long plot
# was left fallow

plot$fallow_year <- ifelse(plot$fallow_year %in% 98, NA, plot$fallow_year)
plot$fallow <- ifelse(plot$fallow_year %in% 0, 0, plot$fallow )
plot$fallow <- ifelse(is.na(plot$fallow_year), NA, plot$fallow)
plot <- dplyr::select(plot, -fallow_year, - pest_q_unit)

fert1 <- read_dta(file.path(dataPath, "TZNPS2AGRDTA/AG_SEC3A.dta")) %>%
  dplyr::select(hhid=y2_hhid, plotnum, typ=ag3a_46, qty=ag3a_47, vouch=ag3a_48, valu=ag3a_49)

fert2 <- read_dta(file.path(dataPath, "TZNPS2AGRDTA/AG_SEC3A.dta")) %>%
  dplyr::select(hhid=y2_hhid, plotnum, typ=ag3a_53, qty=ag3a_54, vouch=ag3a_55, valu=ag3a_56)

fert1$typ <- as_factor(fert1$typ)
fert1$vouch <- ifelse(fert1$vouch %in% 2, 0, fert1$vouch)

fert2$typ <- as_factor(fert2$typ)
fert2$vouch <- ifelse(fert2$vouch %in% 2, 0, fert2$vouch)

levels(fert1$typ) <- levels(fert2$typ) <-
  c("DAP", "UREA", "TSP", "CAN", "SA", "generic NPK (TZA)", "MRP")

#AV add fertiliser dummy

fert1 <- mutate(fert1, DAP = ifelse(typ =='DAP', 1, 0) %>% replace(is.na(.), 0))
fert1 <- mutate(fert1, UREA = ifelse(typ == 'UREA', 1, 0)%>% replace(is.na(.), 0))
fert1 <- mutate(fert1, TSP = ifelse(typ =='TSP', 1, 0)%>% replace(is.na(.), 0))
fert1 <- mutate(fert1, CAN = ifelse(typ=='CAN', 1, 0)%>% replace(is.na(.), 0))
fert1 <- mutate(fert1, SA = ifelse(typ == 'SA', 1, 0)%>% replace(is.na(.), 0))
fert1 <- mutate(fert1, generic_NPK = ifelse(typ =='generic NPK (TZA)', 1, 0)%>% replace(is.na(.), 0))
fert1 <- mutate(fert1, MRP = ifelse(typ == 'MRP', 1, 0)%>% replace(is.na(.), 0))
fert1$season <- 'long'
fdum <- fert1 %>% group_by(hhid, season) %>%
  summarise(DAP = max(DAP), UREA = max(UREA), TSP = max(TSP), CAN = max(CAN), SA = max(SA), generic_NPK = max(generic_NPK), MRP = max(MRP))

fert2 <- mutate(fert2, DAP = ifelse(typ =='DAP', 1, 0) %>% replace(is.na(.), 0))
fert2 <- mutate(fert2, UREA = ifelse(typ == 'UREA', 1, 0)%>% replace(is.na(.), 0))
fert2 <- mutate(fert2, TSP = ifelse(typ =='TSP', 1, 0)%>% replace(is.na(.), 0))
fert2 <- mutate(fert2, CAN = ifelse(typ=='CAN', 1, 0)%>% replace(is.na(.), 0))
fert2 <- mutate(fert2, SA = ifelse(typ == 'SA', 1, 0)%>% replace(is.na(.), 0))
fert2 <- mutate(fert2, generic_NPK = ifelse(typ =='generic NPK (TZA)', 1, 0)%>% replace(is.na(.), 0))
fert2 <- mutate(fert2, MRP = ifelse(typ == 'MRP', 1, 0)%>% replace(is.na(.), 0))
fert2$season <- 'short'       

fdum2 <- fert2 %>% group_by(hhid, season) %>%
  summarise(DAP = max(DAP), UREA = max(UREA), TSP = max(TSP), CAN = max(CAN), SA = max(SA), generic_NPK = max(generic_NPK), MRP = max(MRP))

fdum <- rbind(fdum, fdum2)

fdum <- group_by(fdum, hhid) %>%
  summarise(DAP = max(DAP), UREA = max(UREA), TSP = max(TSP), CAN = max(CAN), SA = max(SA), generic_NPK = max(generic_NPK), MRP = max(MRP))
rm(fdum2)

fdum <- fdum %>% group_by(hhid) %>%
  summarise(DAP = max(DAP), UREA = max(UREA), TSP = max(TSP), CAN = max(CAN), SA = max(SA), generic_NPK = max(generic_NPK), MRP = max(MRP))


fert1 <- select(fert1, -DAP, -UREA, -TSP, -CAN, -SA, -generic_NPK, -MRP)
fert2 <- select(fert2, -DAP, -UREA, -TSP, -CAN, -SA, -generic_NPK, -MRP)
# -------------------------------------
# reorganize data so that observations
# on fertilizer type occupy a single row
# fertilizer is unit of observation
# Data on NPK composition from Sheahan et al (2014), Food Policy
# -------------------------------------

#conv <- read.csv(file.path(paste0(dataPath,"/../.."), "Fert_comp.csv")) %>%
conv <- read.csv("SurveyData/Other/Fertilizer/Fert_comp.csv")  %>%
  transmute(typ=Fert_type2, n=N_share/100, p=P_share/100) %>%
  filter(typ %in% levels(fert1$typ))

fert1 <- left_join(fert1, conv)
fert2 <- left_join(fert2, conv)

# -------------------------------------
# organize fertilizer data for analysis

fert <- rbind(fert1, fert2)

fert <- mutate(fert,
               Vfert=valu/qty,
               Qn=qty*n,
               Qp=qty*p)

fert$Pn <- fert$Vfert/fert$n

# Compute subsidised, non-subsidised and mix fertilizer prices per plot
# As valu or N is sometimes 0, all prices that are 0 are set to NA

fertnosub   <- filter(fert, vouch==0) %>%
  group_by(hhid, plotnum) %>%
  summarise(N=sum(Qn, na.rm=TRUE),
            WPnnosub=sum((Qn/N)*Pn, na.rm=TRUE)) %>%
  select(-N) %>%
  mutate(WPnnosub = replace(WPnnosub, WPnnosub==0, NA))


fertsub <- filter(fert, vouch==1) %>%
  group_by(hhid, plotnum) %>%
  summarise(N=sum(Qn, na.rm=TRUE),
            WPnsub=sum((Qn/N)*Pn, na.rm=TRUE)) %>%
  select(-N) %>% 
  mutate(WPnsub = replace(WPnsub, WPnsub==0, NA))

fertmix <- filter(fert, vouch %in% c(0,1)) %>%
  group_by(hhid, plotnum) %>%
  summarise(N=sum(Qn, na.rm=TRUE),
            P=sum(Qp, na.rm=TRUE),
            WPn=sum((Qn/N)*Pn, na.rm=TRUE)) %>%
  mutate(WPn = replace(WPn, WPn==0, NA))

# join back with the rest of the data and set N and P to 0 for NA values
fert <- select(fert, hhid, vouch) %>%
  group_by(hhid) %>%
  summarise(subfert = max(vouch)) %>%
  mutate(subfert = ifelse(is.na(subfert), 0, subfert))


summary(fert$vouch)
plot <- left_join(plot, fertmix) %>%
  left_join(., fertnosub) %>%
  left_join(., fertsub) %>%
  left_join(.,fert) %>%
  mutate(N = ifelse(is.na(N), 0, N),
         P = ifelse(is.na(P), 0, P))

plotHH <- group_by(plot, hhid) %>%
  summarise(irrig   =max(irrig),
            title   =max(title),
            manure  =max(manure),
            pest    =max(pest),
            fallow  =max(fallow),
            inorg   =sum(inorg %in% "YES"),
            N_inorg =sum(N),
            P_inorg =sum(P),
            WPn     =sum(WPn))


rm(list=c("fert1", "fert2", "fert", "fertsub", "fertnosub", "fertmix", "conv"))



#######################################
############### GEO ###################
#######################################

geo10 <- read_dta(file.path(dataPath, "TZNPS2GEODTA/HH.Geovariables_Y2.dta")) %>%
  select(hhid=y2_hhid, lon=lon_modified, lat=lat_modified, dist2town=dist02,
         dist2market=dist03, dist2HQ=dist05, avgTemp=clim01, avgpPrecip=clim03)

#######################################
############### AREAs #################
#######################################

areas <- read_dta(file.path(dataPath, "areas_tza_y2_imputed.dta")) %>%  
  select(hhid=case_id, plotnum,
                area_farmer=area_sr, area_gps=area_gps_mi_50)

areas$area_gps <- ifelse(areas$area_gps %in% 0, NA, areas$area_gps)

# add an ownership variable if the household owns
# a plot 

own <- read_dta(file.path(dataPath, "TZNPS2AGRDTA/AG_SEC3A.dta")) %>%
    select(hhid=y2_hhid, plotnum, own=ag3a_24)
own$own <- ifelse(own$own %in% 1 | own$own %in% 5, 1, 0)
areas <- left_join(areas, own); rm(own)

# calcualte the households total land holdings
areaTotal <- group_by(areas, hhid) %>%
  summarise(area_tot = sum(area_gps))

areaTotal$area_tot <- ifelse(areaTotal$area_tot %in% 0, NA, areaTotal$area_tot)

#######################################
############### ASSETS ################
#######################################

implmt <- read_dta(file.path(dataPath, "TZNPS2AGRDTA/AG_SEC11.dta")) %>%
  dplyr::select(hhid=y2_hhid, itemcode, qty=ag11_01, valu=ag11_02) %>%
  filter(!qty %in% 0, !is.na(qty), !valu %in% 0, !is.na(valu)) %>%
  transmute(hhid, valu=qty*valu) %>%
  group_by(hhid) %>%
  summarise(value=sum(valu))

# -------------------------------------
# Livestock assets
# -------------------------------------

# classifications from wave 3 classification table
LR <- c("BULLS", "COWS", "STEERS", "HEIFERS", "MALE-CALVES", "FEMALE-CALVES")
SR <- c("GOATS", "SHEEP")
PIGS <- c("PIGS")
POULTRY <- c("CHICKENS", "DUCKS", "TURKEYS")
OTHER <- c("RABBITS", "HORSES", "DOG", "OTHER")

# read in the data
lvstock <- read_dta(file.path(dataPath, "TZNPS2AGRDTA/AG_SEC10A.dta")) %>%
  select(hhid=y2_hhid, animal = lvstkcode, owned = ag10a_02, indigQty = ag10a_05_1,
         improvBeefQty = ag10a_05_2, improvDairyQty = ag10a_05_3 )
lvstock$owned <- ifelse(lvstock$owned %in% 1, 1, 0)
lvstock$animal <- as_factor(lvstock$animal)

# remove white space
lvstock$animal <- gsub(" ", "-", lvstock$animal)

# count the number of animals of each class a household owns
lvstock_x <- select(lvstock, hhid, animal, indigQty, improvBeefQty, improvDairyQty) %>%
  melt(id = c("hhid", "animal")) %>%
  group_by(hhid, animal) %>%
  mutate(class = ifelse(animal %in% LR, "LR",
                   ifelse(animal %in% SR, "SR", 
                     ifelse(animal %in% PIGS, "PIGS_",
                       ifelse(animal %in% POULTRY, "POULTRY",
                         ifelse(animal %in% OTHER, "OTHER_")))))) %>%
  group_by(hhid, class) %>%
  summarise(n=sum(value, na.rm=TRUE)) %>%
  dcast(hhid ~ class)

# count the number of each animal a household owns
lvstock_y <- select(lvstock, hhid, animal, indigQty, improvBeefQty, improvDairyQty) %>%
  melt(id = c("hhid", "animal")) %>%
  group_by(hhid, animal) %>%
  summarise(n=sum(value, na.rm=TRUE)) %>%
  dcast(hhid ~ animal)

# AV: make new variable TLU
lvstock_y <- lvstock_y %>%
  mutate(TLU = BULLS*0.7 + COWS*0.7 + `FEMALE-CALVES`*0.7 + `MALE-CALVES`*0.7 + HEIFERS*0.7 + STEERS*0.7
         + GOATS*0.1 + SHEEP*0.1
         + HORSES*0.8
         + CHICKENS*0.01 + TURKEYS*0.02
         + RABBITS*0.01
         )
  

# join together
lvstock <- left_join(lvstock_x, lvstock_y)

rm("LR", "SR", "lvstock_x", "lvstock_y", "OTHER", "PIGS", "POULTRY")

#######################################
######### Crop production #############
#######################################

source("Scripts/Crop_production_TZA_2010.r")

#######################################
############ Nutrition ################
#######################################

source("Scripts/Nutrition_indicators_TZA_2010.r")

#######################################
############ PANEL KEY ################
#######################################

# key for joing individuals and households across years
key <- read_dta(file.path(dataPath, "../../2012/Data/NPSY3.PANEL.KEY.dta")) %>%
   rename(hhid2008 = y1_hhid, hhid = y2_hhid, hhid2012=y3_hhid)
key$hhid2008 <- zap_empty(key$hhid2008)
key$hhid <- zap_empty(key$hhid)
key$hhid2012 <- zap_empty(key$hhid2012)

keyHH <-subset(key, indidy2==1)

#######################################
########### CROSS SECTION #############
#######################################

# joins at the household level (y2_hhid)
TZA2010HH <- left_join(location, keyHH)
TZA2010HH <- left_join(TZA2010HH, FNS2010)
TZA2010HH <- left_join(TZA2010HH, SOCIO_HH)
TZA2010HH <- left_join(TZA2010HH, income_2010)
TZA2010HH <- left_join(TZA2010HH, HQI)
TZA2010HH <- left_join(TZA2010HH, geo10)
TZA2010HH <- left_join(TZA2010HH, implmt)
TZA2010HH <- left_join(TZA2010HH, lvstock)
TZA2010HH <- left_join(TZA2010HH, areaTotal)
TZA2010HH <- left_join(TZA2010HH, crop_prod_area_rel)
TZA2010HH <- left_join(TZA2010HH, oputHH)
TZA2010HH <- left_join(TZA2010HH, plotHH)
TZA2010HH <- left_join(TZA2010HH, fdum)


                                              
#TZA2010 <- left_join(TZA2010, income_2010); rm(income_2010)
TZA2010HH <- left_join(TZA2010HH, HQI); rm(key)
TZA2010HH <- left_join(TZA2010HH, key); rm(key)
TZA2010HH <- left_join(TZA2010HH, geo10); rm(geo10)
TZA2010HH <- left_join(TZA2010HH, implmt); rm(implmt)
TZA2010HH <- left_join(TZA2010HH, areaTotal); rm(areaTotal)
TZA2010HH <- left_join(TZA2010HH, lvstock); rm(lvstock)
TZA2010HH <- left_join(TZA2010HH, fdum); rm(fdum)

rm(SOCIO_HH, location, keyHH, income_2010, HQI, geo08, implmt, areaTotal, lvstock, fdum, vouch, oputHH, plotHH, key)

# joins at the household level (y2_hhid)
TZA2010 <- left_join(location, SOCIO_HH); rm(location); rm(SOCIO_HH)
#TZA2010 <- left_join(TZA2010, income_2010); rm(income_2010)
TZA2010 <- left_join(TZA2010, HQI); rm(key)
TZA2010 <- left_join(TZA2010, key); rm(key)
TZA2010 <- left_join(TZA2010, geo10); rm(geo10)
TZA2010 <- left_join(TZA2010, implmt); rm(implmt)
TZA2010 <- left_join(TZA2010, areaTotal); rm(areaTotal)
TZA2010 <- left_join(TZA2010, lvstock); rm(lvstock)
TZA2010 <- left_join(TZA2010, fdum); rm(fdum)

# joins at the plot level
TZA2010 <- left_join(TZA2010, plot); rm(plot)
TZA2010 <- left_join(TZA2010, oput); rm(oput)
TZA2010 <- left_join(TZA2010, tc); rm(tc)
TZA2010 <- left_join(TZA2010, lab); rm(lab)
TZA2010 <- left_join(TZA2010, areas); rm(areas)

# -------------------------------------
# Make some new variables
# -------------------------------------

# amend the death and SACCO variables

TZA2010$SACCO <- ifelse(TZA2010$SACCO %in% 1, 1, 0)
TZA2010$death <- ifelse(TZA2010$death %in% 1, 1, 0)

# per hectacre
TZA2010 <- mutate(TZA2010,
                  yld=qty/area_gps,
                  N=N/area_gps,
                  P=P/area_gps,
                  lab=lab/area_gps,
                  pest_q=pest_q/area_gps,
                  asset=value/area_tot
)

# -------------------------------------
# Inflate 2011 prices to 2013 prices: assets, fertilizer and maize prices
# using inflation rate for 2011 and 2013. These years were selected as the main part of the survey takes place in these years.
# from world bank:
# http://data.worldbank.org/indicator/FP.CPI.TOTL.ZG/countries/TZ?display=graph
# -------------------------------------

inflation <- read.csv(file.path(paste0(dataPath,"/../.."), "inflation.csv"))
rate2011 <- inflation$inflation[inflation$code=="TZ" & inflation$year==2011]
rate2013 <- inflation$inflation[inflation$code=="TZ" & inflation$year==2013]

TZA2010 <- mutate(TZA2010,
                  asset = asset*(1 + rate2011)*(1 + rate2013),
                  crop_price = crop_price*(1 + rate2011)*(1 + rate2013),
                  WPn = WPn*(1 + rate2011)*(1 + rate2013),
                  WPnnosub = WPnnosub*(1 + rate2011)*(1 + rate2013),
                  WPnsub = WPnsub*(1 + rate2011)*(1 + rate2013))

TZA2010 <- select(TZA2010, -qty, -value)

# add final variables

TZA2010<- mutate(TZA2010, surveyyear=2010) %>% rename(hhid2010=y2_hhid)
TZA2010$hhid <-TZA2010$hhid2010

rm(list=ls()[!ls() %in% c("TZA2010", "dataPath")])

CSI2010 <- readRDS(file.path(dataPath, "CSI2010.rds"))
FCS2010 <- readRDS(file.path(dataPath, "FCS2010.rds"))
DDS2010total <- readRDS(file.path(dataPath, "DDS2010total.rds"))
FVS2010 <-readRDS(file.path(dataPath, "FVS2010.rds"))

#AV sum hybrd to hh level
TZA2010 <- group_by(TZA2010, hhid2010) %>%
  mutate(hybrd1 = max(hybrd)) %>%
  mutate(subseed1 = max(subseed)) %>%
  mutate(subfert1 = max(subfert)) %>%
  ungroup

#AV add maize dummy
TZA2010 <- group_by(TZA2010, hhid2010) %>%
  mutate(m= ifelse(zaocode %in% 11, 1, 0)) %>%
  mutate(m = max(m)) %>%
  ungroup


TZA2010 <- left_join(TZA2010, CSI2010); rm(CSI2010)
TZA2010 <- left_join(TZA2010, FCS2010); rm(FCS2010)
TZA2010 <- left_join(TZA2010, DDS2010total); rm(DDS2010total)
TZA2010 <- rename(TZA2010, DDS = dietary_diversity_score)
TZA2010 <- left_join(TZA2010, FVS2010); rm(FVS2010)


saveRDS(TZA2010, file.path(dataPath, "TZA2010.rds"))
saveRDS(TZA2010, file.path(dataPath, "/../../TZA2010.rds"))

TZA2010 <- unique(TZA2010)
TZA2010 <- select(TZA2010, -m)
