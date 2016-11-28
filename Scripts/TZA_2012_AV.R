#######################################
########## TANZANIA 2012-13 ###########
#######################################

if(Sys.info()["user"] == "Tomas"){
  dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA/2012/Data" }
if(Sys.info()["user"] == "linde069"){
  dataPath <- "D:/Analyses/CIMMYT/TZA_Anne/SurveyData/2012/Data"
  setwd("D:/Analyses/CIMMYT/TZA_Anne")}
# Anne
#dataPath <- dataPath <- "D:/UserData/verbe038/TZA/2012/Data"
#dataPath <- dataPath <- "C:/Users/afver/OneDrive/Documenten/Removable Disk/verbe038/TZA/2012/Data"


library(haven)
library(dplyr)
library(reshape2)
library(Deducer)

options(scipen=999)

#######################################
############## LOCATION ###############
#######################################

# file containing the zone, region and district

location <- read_dta(file.path(dataPath, "HH_SEC_A.dta")) %>%
  dplyr::select(hhid=y3_hhid, REGCODE = hh_a01_1 , DISCODE = hh_a02_1, rural = y3_rural)
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

saveRDS(location, "Data/Location_2012.rds")


rm(ZONEREGDIS)

#######################################
########### SOCIO/ECONOMIC ############
#######################################

source("Scripts/socioecon_TZA_2012.r")

source("Scripts/income_TZA_2012.r")

#######################################
########### Housing quality ###########
#######################################

source("Scripts/HQI_TZA_2012.R")

#######################################
############### OUTPUT ################
#######################################

oput <- read_dta(file.path(dataPath, "AG_SEC_4A.dta")) %>%
  dplyr::select(hhid=y3_hhid, plotnum, zaocode, inter_crop=ag4a_04,
         harv_area=ag4a_21, qty=ag4a_28, valu=ag4a_29, hybrd=ag4a_08, subseed=ag4a_11)

oput$inter_crop <- ifelse(oput$inter_crop %in% 1, 1, 0)
oput$hybrd <- ifelse(oput$hybrd %in% 1, 1, 0)
oput$zaocode <- as.integer(oput$zaocode)

oput$inter_crop <- ifelse(oput$inter_crop %in% 1, 1, 0)
oput$hybrd <- ifelse(oput$hybrd %in% 1, 1, 0)
oput$zaocode <- as.integer(oput$zaocode)
oput$subseed <- as.integer(oput$subseed)
oput$subseed <- ifelse(oput$subseed %in% 1, 1, 0)
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
            maize=ifelse(any(zaocode %in% 11), 1, 0), # maize has crop code 11
            wheat=ifelse(any(zaocode %in% 16), 1, 0)) # wheat has crop code 16

oput <- left_join(oput, oput_x); rm(oput_x)
# remove farmers who responded they produced
# zero quantity ofcrop, or did not respond (NA)

oput <- oput[!is.na(oput$qty) & !oput$qty %in% 0, ]
oput$crop_price <- oput$valu/oput$qty
oput <- subset(oput, select = -c(valu))

rm(list=c("legumes", "cashCropNPerm", "cashCropsPerm",
          "CTR", "fruit", "vegetables"))

#######################################
############# CHEMICAL ################
#######################################

# two que
plot <- read_dta(file.path(dataPath, "AG_SEC_3A.dta")) %>%
  dplyr::select(hhid=y3_hhid, plotnum, zaocode=ag3a_07_2,  soil=ag3a_10, slope_farmer=ag3a_17, irrig=ag3a_18, title=ag3a_28, 
                manure=ag3a_41, pest=ag3a_60, pest_q=ag3a_62_1, pest_q_unit=ag3a_62_2, fallow_year=ag3a_22, fallow=ag3a_23)

plot$zaocode <- as.integer(plot$zaocode)
plot$soil <- factor(plot$soil, levels=c(1,2,3,4), labels=c("Sandy", "Loam", "Clay", "Other"))
plot$slope_farmer <- factor(plot$slope_farmer, levels=c(1,2,3,4), labels=c("Flat bottom", "Flat top", "Slightly sloped", "Very steep"))
plot$title <- as.numeric(as_factor(plot$title))
plot$title <- ifelse(plot$title %in% c(1:10), 1, 0)
plot$irrig <- ifelse(plot$irrig %in% 1, 1, 0)
plot$manure <- ifelse(plot$manure %in% 1, 1, 0)
plot$pest <- ifelse(plot$pest %in% 1, 1, 0)
plot$pest_q_unit <- as_factor(plot$pest_q_unit)

plot$pest_q <- ifelse(plot$pest_q_unit %in% c("LITRE", "KG"), plot$pest_q,
                      ifelse(plot$pest_q_unit %in% "MILLILITRE", plot$pest_q*0.001, NA))
#stions on fallow - make sure they match up correctly
# fallow value of 98 means subject did not know how long plot
# was left fallow
plot$fallow_year <- ifelse(plot$fallow_year %in% 98, NA, plot$fallow_year)
plot$fallow <- ifelse(plot$fallow_year %in% 0, 0, plot$fallow )
plot$fallow <- ifelse(is.na(plot$fallow_year), NA, plot$fallow)
plot <- dplyr::select(plot, -fallow_year, -pest_q_unit)

fert1 <- read_dta(file.path(dataPath, "AG_SEC_3A.dta")) %>%
  dplyr::select(hhid=y3_hhid, plotnum, typ=ag3a_48, qty=ag3a_49, vouch=ag3a_50, valu=ag3a_51)

fert2 <- read_dta(file.path(dataPath, "AG_SEC_3A.dta")) %>%
  dplyr::select(hhid=y3_hhid, plotnum, typ=ag3a_55, qty=ag3a_56, vouch=ag3a_57, valu=ag3a_58)

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
  summarise(DAP = max(DAP), 
            UREA = max(UREA), 
            TSP = max(TSP), 
            CAN = max(CAN), 
            SA = max(SA), 
            generic_NPK = max(generic_NPK), 
            MRP = max(MRP))

fdum <- rbind(fdum, fdum2)

fdum <- group_by(fdum, hhid) %>%
  summarise(DAP = max(DAP), 
            UREA = max(UREA), 
            TSP = max(TSP), 
            CAN = max(CAN), 
            SA = max(SA), 
            generic_NPK = max(generic_NPK), 
            MRP = max(MRP))
rm(fdum2)

fdum <- fdum %>% group_by(hhid) %>%
  summarise(DAP = max(DAP), 
            UREA = max(UREA), 
            TSP = max(TSP), 
            CAN = max(CAN), 
            SA = max(SA), 
            generic_NPK = max(generic_NPK), 
            MRP = max(MRP))


fert1 <- subset(fert1, select = -c(DAP, UREA, TSP, CAN, SA, generic_NPK, MRP))
fert2 <- subset(fert2, select = -c(DAP, UREA, TSP, CAN, SA, generic_NPK, MRP))

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
rm(conv)

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
  dplyr::select(-N) %>%
  mutate(WPnnosub = replace(WPnnosub, WPnnosub==0, NA))


fertsub   <- filter(fert, vouch==1) %>%
  group_by(hhid, plotnum) %>%
  summarise(N=sum(Qn, na.rm=TRUE),
            WPnsub=sum((Qn/N)*Pn, na.rm=TRUE)) %>%
  dplyr::select(-N) %>% 
  mutate(WPnsub = replace(WPnsub, WPnsub==0, NA))

fertmix <- filter(fert, vouch %in% c(0,1)) %>%
  group_by(hhid, plotnum) %>%
  summarise(N=sum(Qn, na.rm=TRUE),
            P=sum(Qp, na.rm=TRUE),
            WPn=sum((Qn/N)*Pn, na.rm=TRUE)) %>%
  mutate(WPn = replace(WPn, WPn==0, NA))

# join back with the rest of the data and set N and P to 0 for NA values
fert <- subset(fert, select = c(hhid, vouch))

fert <- group_by(fert, hhid) %>%
  summarise(subfert = max(vouch)) %>%
  mutate(subfert = ifelse(is.na(subfert), 0, subfert))

plot <- left_join(plot, fertmix) %>%
  left_join(., fertnosub) %>%
  left_join(., fertsub) %>%
  left_join(.,fert) %>%
  mutate(N = ifelse(is.na(N), 0, N),
         P = ifelse(is.na(P), 0, P))
rm(list=c("fert1", "fert2", "fert", "fertsub", "fertnosub", "fertmix"))


#######################################
############### ASSETS ################
#######################################

library(haven)
implmt <- read_dta(file.path(dataPath, "AG_SEC_11.dta"))

library(dplyr)
implmt <- read_dta(file.path(dataPath, "AG_SEC_11.dta")) %>%
  dplyr::select(hhid=y3_hhid, itemname, qty=ag11_01, valu=ag11_02) %>%
  filter(!qty %in% 0, !is.na(qty), !valu %in% 0, !is.na(valu)) %>%
  transmute(hhid, valu=qty*valu) %>%
  group_by(hhid) %>%
      summarise(value=sum(valu))

#Deducer::frequencies(implmt$value)

# -------------------------------------
# Livestock assets
# -------------------------------------

# classifications from wave 3 classification table
LR <- c("BULLS", "COWS", "STEERS", "HEIFERS", "MALE-CALVES", "FEMALE-CALVES")
SR <- c("GOATS", "SHEEP")
PIGS <- c("PIGS")
POULTRY <- c("CHICKENS", "DUCKS", "OTHER-POULTRY")
OTHER <- c("RABBITS", "DONKEYS", "DOGS", "OTHER")

# read in the data
lvstock <- read_dta(file.path(dataPath, "LF_SEC_02.dta")) %>%
  dplyr::select(hhid=y3_hhid, animal = lvstckid, owned = lf02_01,
         indigQty = lf02_04_1, improvQty = lf02_04_2)
lvstock$owned <- ifelse(lvstock$owned %in% 1, 1, 0)
lvstock$animal <- as_factor(lvstock$animal)

# remove white space
lvstock$animal <- gsub(" ", "-", lvstock$animal)
lvstock <- filter(lvstock, !is.na(animal) )

# count the number of animals of each class a household owns
lvstock_x <- dplyr::select(lvstock, hhid, animal, indigQty, improvQty) %>%
  reshape2::melt(id = c("hhid", "animal"))
livestock_X <- group_by(lvstock_x, hhid, animal) %>%
  mutate(class = ifelse(animal %in% LR, "LR",
                   ifelse(animal %in% SR, "SR", 
                     ifelse(animal %in% PIGS, "PIGS_",
                       ifelse(animal %in% POULTRY, "POULTRY",
                         ifelse(animal %in% OTHER, "OTHER_", ifelse(NA))))))) %>%
  group_by(hhid, class) %>%
  summarise(n=sum(value, na.rm=TRUE)) %>%
  reshape2::dcast(hhid ~ class)

lvstock_x <- dplyr::select(lvstock, hhid, animal, indigQty, improvQty) %>%
  reshape2::melt(id = c("hhid", "animal")) %>%
  group_by(hhid, animal) %>%
  mutate(class = ifelse(animal %in% LR, "LR",
                   ifelse(animal %in% SR, "SR", 
                     ifelse(animal %in% PIGS, "PIGS_",
                       ifelse(animal %in% POULTRY, "POULTRY",
                         ifelse(animal %in% OTHER, "OTHER_", ifelse(NA))))))) %>%
  group_by(hhid, class) %>%
  summarise(n=sum(value, na.rm=TRUE)) %>%
  reshape2::dcast(hhid ~ class)

# count the number of each animal a household owns
lvstock_y <- subset(lvstock, select = c(hhid, animal, indigQty, improvQty)) %>%
  reshape2::melt(id = c("hhid", "animal")) %>%
  group_by(hhid, animal) %>%
  summarise(n=sum(value, na.rm=TRUE)) %>%
  reshape2::dcast(hhid ~ animal)

# AV: make new variable TLU
lvstock_y <- lvstock_y %>%
  mutate(TLU = BULLS*0.7 + COWS*0.7 + `FEMALE-CALVES`*0.7 + `MALE-CALVES`*0.7 + HEIFERS*0.7 + STEERS*0.7
         + GOATS*0.1 + SHEEP*0.1
         + DONKEYS*0.6
         + CHICKENS*0.01 + DUCKS*0.02
         + RABBITS*0.01
  )

# join together
lvstock <- left_join(lvstock_x, lvstock_y)

rm("LR", "SR", "lvstock_x", "lvstock_y", "OTHER", "PIGS", "POULTRY")

#######################################
############### GEO ###################
#######################################

geo12 <- read_dta(file.path(dataPath, "HouseholdGeovars_Y3.dta")) %>%
  select(hhid=y3_hhid, lon=lon_dd_mod, lat=lat_dd_mod, dist2Rd=dist01,
         dist2town=dist02, dist2market=dist03, dist2HQ=dist05, avgTemp=clim01,
         avgpPrecip=clim03)

#######################################
############### AREAs #################
#######################################

areas <- read.csv(file.path(dataPath, "/areas_w3.csv")) %>%
  dplyr::select(hhid=y3_hhid, plotnum,
                area_farmer=area.est,
                area_gps=gps_imputed)

areas$hhid <- as.character(areas$hhid)
areas$plotnum <- as.character(areas$plotnum)
areas$area_gps <- ifelse(areas$area_gps %in% 0, NA, areas$area_gps)

# 2012 areas are in acres, change to hecacres in line
# with the 2010 data
areas$area_gps <- areas$area_gps*0.404686 # from wikipedia

# plot ownership
own <- read_dta(file.path(dataPath, "AG_SEC_3A.dta")) %>%
  dplyr::select(hhid=y3_hhid, plotnum, own=ag3a_25)
own$own <- ifelse(own$own %in% 1 | own$own %in% 5, 1, 0)

areas <- left_join(areas, own); rm(own)

# total farm holdings
areaTotal <- group_by(areas, hhid) %>%
  summarise(area_tot = sum(area_gps))

#######################################
######### Crop production #############
#######################################

source("Scripts/Crop_production_TZA_2012.r")

#######################################
############ Nutrition ################
#######################################

source("Scripts/Nutrition_indicators_TZA_2012.r")

#######################################
############ PANEL KEY ################
#######################################

# key for joing individuals and households across years
key <- read_dta(file.path(dataPath, "NPSY3.PANEL.KEY.dta")) %>%
  select(-UPI3) %>% rename(hhid2008 = y1_hhid, hhid2010=y2_hhid)
key$hhid2008 <- zap_empty(key$hhid2008)
key$hhid2010 <- zap_empty(key$hhid2010)
key$y3_hhid <- zap_empty(key$y3_hhid)

keyHH <-subset(key, indidy3==1)
rm(key)

#######################################
########### CROSS SECTION #############
#######################################

# joins at the household level (y3_hhid)

TZA2012HH <- left_join(location, keyHH)
TZA2012HH <- left_join(TZA2012HH, FNS2012)
TZA2012HH <- left_join(TZA2012HH, SOCIO_HH)
TZA2012HH <- left_join(TZA2012HH, income_2012)
TZA2012HH <- left_join(TZA2012HH, HQI)
TZA2012HH <- left_join(TZA2012HH, geo12)
TZA2012HH <- left_join(TZA2012HH, implmt)
TZA2012HH <- left_join(TZA2012HH, lvstock)
TZA2012HH <- left_join(TZA2012HH, areaTotal)
TZA2012HH <- left_join(TZA2012HH, crop_prod_area_rel)
TZA2012HH <- left_join(TZA2012HH, oputHH)
TZA2012HH <- left_join(TZA2012HH, plotHH)
TZA2012HH <- left_join(TZA2012HH, fdum)
TZA2012HH <- left_join(TZA2012HH, vouch)

frequencies(TZA2008HH$maize ,r.digits=1)

rm(SOCIO_HH, location, keyHH, income_2012, HQI, geo12, implmt, areaTotal, lvstock, fdum, vouch, oputHH, plotHH, key)
rm(HQI); rm(ed)
rm(fert2)

TZA2012 <- left_join(location, HH12); rm(location); rm(HH12)
TZA2012 <- left_join(TZA2012, income_2012); rm(income_2012)
TZA2012 <- left_join(TZA2012, key); rm(key)
TZA2012 <- left_join(TZA2012, geo12); rm(geo12)
TZA2012 <- left_join(TZA2012, implmt); rm(implmt)
TZA2012 <- left_join(TZA2012, areaTotal); rm(areaTotal)
TZA2012 <- left_join(TZA2012, lvstock); rm(lvstock)
TZA2012 <- left_join(TZA2012, fdum); rm(fdum)

# joins at the plot level (y3_hhid, plotnum)

TZA2012 <- left_join(TZA2012, oput); rm(oput)
TZA2012 <- left_join(TZA2012, plot); rm(plot)
TZA2012 <- left_join(TZA2012, tc); rm(tc)
TZA2012 <- left_join(TZA2012, lab); rm(lab)
TZA2012 <- left_join(TZA2012, areas); rm(areas)

# -------------------------------------
# Make some new variables
# -------------------------------------

# per hectacre
TZA2012 <- mutate(TZA2012,
                  yld=qty/area_gps,
                  N=N/area_gps,
                  P=P/area_gps,
                  lab=lab/area_gps,
                  pest_q=pest_q/area_gps,
                  asset=value/area_tot
)

TZA2012 <- select(TZA2012, -qty, -value)

# add and rename final variables
TZA2012 <- mutate(TZA2012, surveyyear=2012) %>% rename(hhid2012=y3_hhid)
TZA2012$hhid <-TZA2012$hhid2012

CSI2012 <- readRDS(file.path(dataPath, "CSI2012.rds"))
FCS2012 <- readRDS(file.path(dataPath, "FCS2012.rds"))
DDS2012total <- readRDS(file.path(dataPath, "DDS2012total.rds"))
FVS2012 <- readRDS(file.path(dataPath, "FVS2012.rds"))

#AV sum hybrd to hh level
TZA2012 <- group_by(TZA2012, hhid2012) %>%
  mutate(hybrd1 = max(hybrd)) %>%
  mutate(subseed1 = max(subseed)) %>%
  mutate(subfert1 = max(subfert)) %>%
  ungroup

#AV add maize dummy
TZA2012 <- group_by(TZA2012, hhid2012) %>%
 mutate(m = ifelse(zaocode %in% 11, 1, 0)) %>%
 mutate(m = max(m)) %>%
 ungroup



TZA2012 <- left_join(TZA2012, CSI2012); rm(CSI2012)
TZA2012 <- left_join(TZA2012, FCS2012); rm(FCS2012)
TZA2012 <- left_join(TZA2012, DDS2012total); rm(DDS2012total)
TZA2012 <- rename(TZA2012, DDS = dietary_diversity_score)
TZA2012 <- left_join(TZA2012, FVS2012); rm(FVS2012)

saveRDS(TZA2012, file.path(dataPath, "TZA2012.rds"))
saveRDS(TZA2012, file.path(dataPath, "/../../TZA2012.rds"))


