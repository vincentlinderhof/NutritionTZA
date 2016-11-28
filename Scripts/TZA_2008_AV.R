#######################################
########## TANZANIA 2008-09 ###########
#######################################

#rm(list=ls() )
if(Sys.info()["user"] == "Tomas"){
    dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA/2008/Data" }
if(Sys.info()["user"] == "linde069"){
    dataPath <- "D:/Analyses/CIMMYT/TZA_Anne/SurveyData/2008/Data"
    setwd("D:/Analyses/CIMMYT/TZA_Anne")}

# check with the command "Sys.info()["user"]" the name of the user
# Anne
#dataPath <- dataPath <- "D:/UserData/verbe038/TZA/2008/Data"
#dataPath <- dataPath <- "C:/Users/afver/OneDrive/Documenten/Removable Disk/verbe038/TZA/2008/Data"

library(haven)
library(stringr)
library(reshape2)
library(dplyr)
detach("package:dplyr", character.only = TRUE)
library("dplyr", character.only = TRUE)
options(scipen=999)

#######################################
############## LOCATION ###############
#######################################

# file containing the zone, region and district

location <- read_dta(file.path(dataPath, "TZNPS1HHDTA_E/SEC_A_T.dta"))%>%
  select(hhid, REGCODE = region, DISCODE = district, rural = rural)
location$rural <- ifelse(location$rural %in% "Rural", 1, 0)
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

saveRDS(location, "Data/Location_2008.rds")

rm(ZONEREGDIS)

#######################################
########### SOCIO/ECONOMIC ############
#######################################

source("Scripts/socioecon_TZA_2008.r")

source("Scripts/income_TZA_2008.r")

#######################################
########### Housing quality ###########
#######################################

# AV: make new variables: HQI Housing Quality Index
source("Scripts/HQI_TZA_2008.r")

#######################################
############### OUTPUT ################
#######################################

oput <- read_dta(file.path(dataPath, "TZNPS1AGDTA_E/SEC_4A.dta")) %>%
  dplyr::select(hhid, plotnum, zaocode, inter_crop=s4aq6,
                harv_area=s4aq8, qty=s4aq15, value=s4aq16, hybrd=s4aq22)

oput$inter_crop <- ifelse(oput$inter_crop %in% 1, 1, 0)
oput$hybrd <- ifelse(oput$hybrd %in% 2, 1, 0)
oput$zaocode <- as.integer(oput$zaocode)

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

oput$maize_harv <- ifelse(oput$zaocode %in% 11, oput$harv_area, 0)
oput$maize_qty  <- ifelse(oput$zaocode %in% 11, oput$qty, 0)
oput$maize_value <- ifelse(oput$zaocode %in% 11, oput$value, 0) 

oput$wheat_harv <- ifelse(oput$zaocode %in% 16, oput$harv_area, 0)
oput$wheat_qty  <- ifelse(oput$zaocode %in% 16, oput$qty, 0)
oput$wheat_value <- ifelse(oput$zaocode %in% 16, oput$value, 0) 

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
            maize        =max(maize),
            wheat        =max(wheat),
            maize_harv   =sum(maize_harv),
            maize_qty    =sum(maize_qty),
            maize_value  =sum(maize_value), 
            wheat_harv   =sum(wheat_harv), 
            wheat_qty    =sum(wheat_qty), 
            wheat_value  =sum(wheat_value) ) 


# for productivity of maize farmers we are only interested
# in the maize farmers, exclude everyone else, and farmers
# who responded they produced zero maize, or did not respond (NA)

oput <- oput[! is.na(oput$qty) & !oput$qty %in% 0, ]
#oput$crop_price <- oput$valu/oput$qty
#oput$valu <- NULL

rm(oput)
rm(list=c("legumes", "cashCropNPerm", "cashCropsPerm",
          "CTR", "fruit", "vegetables"))

#######################################
############# CHEMICAL ################
#######################################

plot <- read_dta(file.path(dataPath, "/TZNPS1AGDTA_E/SEC_3A.dta")) %>%
  select(hhid, plotnum, zaocode=s3aq5code, soil=s3aq7, slope_farmer=s3aq14, irrig=s3aq15, title=s3aq25,
                manure=s3aq37, pest=s3aq49, pest_q=s3aq51_amount, pest_q_unit=s3aq51_measure, 
         fallow_year=s3aq19, fallow=s3aq20, inorg=s3aq43)

plot$zaocode <- as.integer(plot$zaocode)
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

# inorganic fertilizer - note that there is no inorganic fertilizer
# voucher question as in 2010 and 2012 surveys. Farmers are only asked
# about one inorganic fertilizer

fert1 <- read_dta(file.path(dataPath, "TZNPS1AGDTA_E/SEC_3A.dta")) %>%
  dplyr::select(hhid, plotnum, typ=s3aq44, qty=s3aq45, valu=s3aq46)

fert2 <- read_dta(file.path(dataPath, "TZNPS1AGDTA_E/SEC_3B.dta")) %>%
  dplyr::select(hhid, plotnum, typ=s3bq44, qty=s3bq45, valu=s3bq45)

fert1$typ <- as_factor(fert1$typ)
fert2$typ <- as_factor(fert2$typ)


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

# AV: add variable voucher
vouch <- read_dta(file.path(dataPath, "/TZNPS1AGDTA_E/SEC_QNFLOW.dta")) %>%
dplyr::select(hhid, voucher1=s13q11_1, voucher2=s13q11_2, voucher3=s13q11_3) 
vouch$voucher1 <-as.integer(vouch$voucher1)
vouch$voucher2 <-as.integer(vouch$voucher2)
vouch$voucher3 <-as.integer(vouch$voucher3)
                                       
vouch$subfert1 <-ifelse(vouch$voucher1 ==1|vouch$voucher1==2|vouch$voucher2==1|vouch$voucher2==2|vouch$voucher3==1|vouch$voucher3==2, 1, 0)
vouch$subseed1 <-ifelse(vouch$voucher1 ==3|vouch$voucher1==4|vouch$voucher2==3|vouch$voucher2==4|vouch$voucher3==3|vouch$voucher3==4, 1, 0)
vouch <- select(vouch, -voucher1, -voucher2, -voucher3)
# -------------------------------------
# reorganize data so that observations
# on fertilizer type occupy a single row
# fertilizer is unit of observation
# Data on NPK composition from Sheahan et al (2014), Food Policy
# -------------------------------------

#VL:
#conv <- read.csv(file.path(paste0(dataPath,"/../.."), "Fert_comp.csv")) %>%
conv <- read.csv("SurveyData/Other/Fertilizer/Fert_comp.csv")  %>%
  transmute(typ=Fert_type2, n=N_share/100, p=P_share/100) %>%
  filter(typ %in% levels(fert1$typ))

#ZONEREGDIS <- read.csv("SurveyData/Other/Spatial/TZA/ZONEREGDIS.csv")
#D:\Analyses\CIMMYT\TZA_Anne\SurveyData\Other\Fertilizer
fert1 <- left_join(fert1, conv)

# -------------------------------------
# organize fertilizer data for analysis

fert1 <- mutate(fert1,
               Vfert=valu/qty,
               N=qty*n,
               P=qty*p)

fert1$WPn <- fert1$Vfert/fert1$n
fert1 <- select(fert1, -qty, -Vfert, -n, -p, -valu, -typ)

# join back with the rest of the data and set N and P to 0 for NA values
plot <- left_join(plot, fert1) 

#VL: added
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
            
rm(plot)
rm(list=c("conv", "fert1"))


#######################################
############### GEO ###################
#######################################

# read in the household geovariables

geo08 <- read_dta(file.path(dataPath, "TZNPS1_consdta/HH.Geovariables_Y1.dta")) %>%
  select(hhid, lon=lon_modified, lat=lat_modified, dist2town=dist02, dist2market=dist03,
         dist2HQ=dist05, avgTemp=clim01, avgpPrecip=clim03)

#######################################
############### ASSETS ################
#######################################

implmt <- read_dta(file.path(dataPath, "/TZNPS1AGDTA_E/SEC_11_ALL.dta")) %>%
  dplyr::select(hhid, impcode, qty=s11q1, valu=s11q2) %>%
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
POULTRY <- c("CHICKENS", "TURKEYS")
OTHER <- c("RABBITS", "DONKEYS", "HORSES", "DOGS", "OTHER")
#D:\Analyses\CIMMYT\TZA_Anne\SurveyData\2008\Data\TZNPS1AGDTA_E
# read in the data
lvstock <- read_dta(file.path(dataPath, "TZNPS1AGDTA_E/SEC_10A.dta")) %>%
  select(hhid, animal, owned = s10aq2, indigQty = s10aq4_1,
         improvBeefQty = s10aq4_2, improvDairyQty = s10aq4_3 )
lvstock$owned <- ifelse(lvstock$owned %in% 1, 1, 0)
lvstock$animal <- as_factor(lvstock$animal)

# sometimes 9999 has been used instead of NA
lvstock$indigQty <- ifelse(lvstock$indigQty == 9999, NA, lvstock$indigQty)
lvstock$improvBeefQty <- ifelse(lvstock$improvBeefQty == 9999, NA, lvstock$improvBeefQty)
lvstock$improvDairyQty <- ifelse(lvstock$improvDairyQty == 9999, NA, lvstock$improvDairyQty)
lvstock$animal <- gsub(" ", "-", lvstock$animal)
lvstock <- filter(lvstock, !is.na(animal) )

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

# -------------------------------------
# land holdings - in wave 1
# -------------------------------------

# only 25% of the areas are GPS measured.
land <- read_dta(file.path(dataPath, "/TZNPS1AGDTA_E/SEC_2A.dta")) %>%
  rename(area_sr = s2aq4, area_gps = area)

# where a gps measurement is missing, replace
# with farmer estimate

land$area <- ifelse(is.na(land$area_gps), land$area_sr, land$area_gps)
land$area <- ifelse(land$area %in% 0, NA, land$area)

# create variable for ownership of the land
# and join with area information

own <- read_dta(file.path(dataPath, "/TZNPS1AGDTA_E/SEC_3A.dta")) %>%
  dplyr::select(hhid, plotnum, own=s3aq22)
own$own <- ifelse(own$own %in% 1 | own$own %in% 5, 1, 0)

land <- left_join(land, own)

# and add a variable for total land holdings, as
# a form of asset wealth

areaTotal <- group_by(land, hhid) %>%
  summarise(area_tot = sum(area))

rm(own, land)
#######################################
######### Crop production #############
#######################################

source("Scripts/Crop_production_TZA_2008.r")

#######################################
############ Nutrition ################
#######################################

source("Scripts/Nutrition_indicators_TZA_2008.r")

#######################################
############ PANEL KEY ################
#######################################

# key for joing individuals and households across years
#key <- read_dta("C:/Users/afver/OneDrive/Documenten/Removable Disk/verbe038/TZA/2012/Data/NPSY3.PANEL.KEY.dta") %>%
key <- read_dta("SurveyData/2012/Data/NPSY3.PANEL.KEY.dta") %>%
  select(-UPI3) %>% rename(hhid = y1_hhid, hhid2010 = y2_hhid, hhid2012=y3_hhid)
key$hhid <- zap_empty(key$hhid)
key$hhid2010 <- zap_empty(key$hhid2010)
key$hhid2012 <- zap_empty(key$hhid2012)

keyHH <-subset(key, indidy1==1)
rm(key)

#######################################
########### CROSS SECTION #############
#######################################

# joins at the household level (hhid)

TZA2008HH <- left_join(location, keyHH)
TZA2008HH <- left_join(TZA2008HH, FNS2008)
TZA2008HH <- left_join(TZA2008HH, SOCIO_HH)
TZA2008HH <- left_join(TZA2008HH, income_2008)
TZA2008HH <- left_join(TZA2008HH, HQI)
TZA2008HH <- left_join(TZA2008HH, geo08)
TZA2008HH <- left_join(TZA2008HH, implmt)
TZA2008HH <- left_join(TZA2008HH, lvstock)
TZA2008HH <- left_join(TZA2008HH, areaTotal)
TZA2008HH <- left_join(TZA2008HH, crop_prod_area_rel)
TZA2008HH <- left_join(TZA2008HH, oputHH)
TZA2008HH <- left_join(TZA2008HH, plotHH)
TZA2008HH <- left_join(TZA2008HH, fdum)
TZA2008HH <- left_join(TZA2008HH, vouch)

frequencies(TZA2008HH$maize ,r.digits=1)

rm(SOCIO_HH, location, keyHH, income_2008, HQI, geo08, implmt, areaTotal, lvstock, fdum, vouch, oputHH, plotHH, key)
rm(HQI); rm(ed)
rm(fert2)
# joins at the plot level (hhid, plotnum)

#TZA2008 <- left_join(TZA2008, oput); rm(oput)
#TZA2008 <- left_join(TZA2008, plot); rm(plot)
#TZA2008 <- left_join(TZA2008, tc); rm(tc)
#TZA2008 <- left_join(TZA2008, lab); rm(lab)
#TZA2008 <- left_join(TZA2008, land); rm(land)

# -------------------------------------
# Make some new variables
# -------------------------------------

# amend the death and SACCO variables

TZA2008$SACCO <- ifelse(TZA2008$SACCO == 1, 1, 0)
TZA2008$death <- ifelse(TZA2008$death == 1, 1, 0)

# per hectacre
TZA2008 <- mutate(TZA2008,
                  yld=qty/area,
                  N=N/area,
                  P=P/area,
                  lab=lab/area,
                  pest_q=pest_q/area,
                  asset=value/area_tot
)

# -------------------------------------
# Inflate 2011 prices to 2013 prices: assets, fertilizer and maize prices
# using inflation rate for 2011 and 2013. These years were selected as the main part of the survey takes place in these years.
# from world bank:
# http://data.worldbank.org/indicator/FP.CPI.TOTL.ZG/countries/TZ?display=graph
# -------------------------------------

inflation <- read.csv(file.path(paste0(dataPath,"/../.."), "inflation.csv"))
rate2009 <- inflation$inflation[inflation$code=="TZ" & inflation$year==2009]
rate2010 <- inflation$inflation[inflation$code=="TZ" & inflation$year==2010]
rate2011 <- inflation$inflation[inflation$code=="TZ" & inflation$year==2011]
rate2012 <- inflation$inflation[inflation$code=="TZ" & inflation$year==2012]

inflate <- (1 + rate2009)*(1 + rate2010)*(1 + rate2011)*(1 + rate2012)

#*More items to be deflated 
TZA2008HH <- mutate(TZA2008HH,
                  assetI               = asset*inflate,
                  lvstock_value_hh_I   = lvstock_value_hh*inflate,
                  slaughter_value_hh_I = slaughter_value_hh*inflate,
                  rent_value_hh_I      = rent_value_hh*inflate,
                  perm_value_hh_I      = perm_value_hh*inflate,
                  fruit_value_hh_I     = fruit_value_hh*inflate,
                  crop_value_hh_I      = crop_value_hh*inflate,
                  income_I             = income*inflate,
                  crop_price = crop_price*inflate,
                  WPn = WPn*inflate)

TZA2008 <- select(TZA2008, -qty, -value)

# add final variables

TZA2008<- mutate(TZA2008, surveyyear=2008) %>% rename(hhid2008=hhid)
TZA2008$hhid <-TZA2008$hhid2008

#AV sum hybrd to hh level
TZA2008 <- group_by(TZA2008, hhid2008) %>%
  mutate(hybrd1 = max(hybrd)) %>%
  ungroup
rm(list=ls()[!ls() %in% c("TZA2008", "dataPath")])

#AV add maize dummy
TZA2008 <- group_by(TZA2008, hhid2008) %>%
  mutate(m= ifelse(zaocode %in% 11, 1, 0)) %>%
  mutate(m = max(m)) %>%
  ungroup


DDS2008total <- readRDS(file.path(dataPath, "DDS2008total.rds"))
FVS2008 <-readRDS(file.path(dataPath, "FVS2008.rds"))

TZA2008 <- left_join(TZA2008, DDS2008total); rm(DDS2008total)
TZA2008 <- rename(TZA2008, DDS = dietary_diversity_score)
TZA2008 <- left_join(TZA2008, FVS2008); rm(FVS2008)

saveRDS(TZA2008, file.path(dataPath, "/../../TZA2008.rds"))


