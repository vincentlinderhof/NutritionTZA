#######################################
########## TANZANIA 2012-13 ###########
#######################################

# Tom
#dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA/TZA2012/Data"

# Michiel
# dataPath <- ""

# Anne
#dataPath <- dataPath <- "D:/UserData/verbe038/TZA/2012/Data"
dataPath <- dataPath <- "C:/Users/afver/OneDrive/Documenten/Removable Disk/verbe038/TZA/2012/Data"

# Vincent
# dataPath <- ""

library(haven)
library(dplyr)
library(reshape2)

options(scipen=999)

#######################################
############## LOCATION ###############
#######################################

# file containing the zone, region and district

location <- read_dta(file.path(dataPath, "HH_SEC_A.dta")) %>%
  select(y3_hhid, REGCODE = hh_a01_1 , DISCODE = hh_a02_1, rural = y3_rural)
location$rural <- as.integer(location$rural)
location$REGCODE <- as.integer(location$REGCODE)

# match up with the names from the survey (prepared in a seperate file)

ZONEREGDIS <- read.csv(file.path(paste0(dataPath,"/../.."), "ZONEREGDIS.csv"))

# join with household identifications

location <- left_join(location, ZONEREGDIS)

rm(ZONEREGDIS)

#######################################
########### SOCIO/ECONOMIC ############
#######################################

HH12 <- read_dta(file.path(dataPath, "HH_SEC_B.dta")) %>%
  select(y3_hhid, indidy3, status=hh_b05, sex=hh_b02,
         yob=hh_b03_1, age=hh_b04, years=hh_b25)

HH12$years <- as.numeric(HH12$years)
HH12$years <- ifelse(HH12$years %in% 99, HH12$age, HH12$years)
HH12$status <- as_factor(HH12$status)
HH12$sex <- toupper(as_factor(HH12$sex))
HH12$yob <- as.integer(HH12$yob)

# make a new variable cage (cut age) which splits
# individuals according to their age group with
# breaks at 15, 55 and the max age

HH12$cage <- cut(HH12$age, breaks = c(0, 15, 55, max(HH12$age, na.rm=TRUE)),
                 labels=c("0-15", "16-55", "56+"), include.lowest = TRUE, right = TRUE)


#AV: make new variable: household size
SIZE <-
  read_dta(file.path(dataPath, "ConsumptionNPS3.dta")) %>%
  select(y3_hhid, hhsize)
HH12 <- left_join(HH12, SIZE); rm(SIZE)

# AV: make new variable: female headship

FHEAD <- mutate(HH12, fem_head = ifelse(sex == 'FEMALE' & status == 'HEAD', 1, 0)) %>%
  group_by(y3_hhid) %>%
  summarise(fem_head = sum(fem_head))
HH12 <- left_join(HH12, FHEAD); rm(FHEAD)

# AV: make a new variables with number of householdmembers in each age-cathegory

HH12 <- HH12 %>%
  mutate(infants = ifelse(age < 6, 1, ifelse (NA))) %>%
  mutate(children = ifelse(age >5 & age < 15, 1, ifelse(NA))) %>%
  mutate(young_adults = ifelse(age >14 & age < 19, 1, ifelse(NA))) %>%
  mutate(mature_adults = ifelse(age >18 & age <65, 1, ifelse(NA))) %>%
  mutate(older_adults = ifelse(age >64, 1, ifelse(NA)))


Age <-
  select(HH12,y3_hhid, indidy3, hhsize, infants, children, young_adults,mature_adults, older_adults) %>% 
  replace(is.na(.), 0) %>%
  group_by(y3_hhid) %>% 
  summarise(infants = sum(infants), children = sum(children), young_adults = sum(young_adults), mature_adults = sum(mature_adults), older_adults = sum(older_adults))

Age$hhsize <- Age$infants+Age$children+Age$young_adults+Age$mature_adults+Age$older_adults
Age$dependency <- (Age$infants+Age$children+Age$older_adults)/Age$hhsize*100

HH12 <- select(HH12, -infants, -children, -young_adults, -mature_adults, -older_adults, -hhsize)
HH12 <- left_join(HH12, Age); rm(Age)

# AV: make new variables: HQI Housing Quality Index

house<- read_dta(file.path(dataPath, "/HH_SEC_I.dta")) %>%
  select(y3_hhid, tenure=hh_i01, wall=hh_i08,
         roof=hh_i09, cook=hh_i16, light=hh_i17, A=hh_i19, B=hh_i22, C=hh_i12)

house$tenure <- as.integer(house$tenure)
house$wall <- as.integer(house$wall)
house$roof <- as.integer(house$roof)
house$cook <- as.integer(house$cook)
house$light <- as.integer(house$light)
house$A <- as.integer(house$A)
house$B <- as.integer(house$B)
house$C <- as.integer(house$C)

house<-mutate(house,drink1=ifelse(A=='1', 1, 
                                  ifelse(A=='2', 2, 
                                         ifelse(A =='3', 3, 
                                                ifelse(A=='4', 4,
                                                       ifelse(A=='5', 5,
                                                              ifelse(A=='6', 5,
                                                                     ifelse(A=='7', 6,
                                                                            ifelse(A=='8', 7, 
                                                                                   ifelse(A=='9', 7, 
                                                                                          ifelse(A=='10', 8,
                                                                                                 ifelse(A=='11', 8,
                                                                                                        ifelse(A=='12', 9,
                                                                                                               ifelse(A=='13', 10, 
                                                                                                                      ifelse(A=='14', 11, NA)))))))))))))))

house<-mutate(house,drink2=ifelse(B=='1', 1, 
                                  ifelse(B=='2', 2, 
                                         ifelse(B =='3', 3, 
                                                ifelse(B=='4', 4,
                                                       ifelse(B=='5', 5,
                                                              ifelse(B=='6', 5,
                                                                     ifelse(B=='7', 6,
                                                                            ifelse(B=='8', 7, 
                                                                                   ifelse(B=='9', 7, 
                                                                                          ifelse(B=='10', 8,
                                                                                                 ifelse(B=='11', 8,
                                                                                                        ifelse(B=='12', 9,
                                                                                                               ifelse(B=='13', 10, 
                                                                                                                      ifelse(B=='14', 11, NA)))))))))))))))
house <-mutate(house, sew= ifelse(C=='1', 5,
                                  ifelse(C=='2', 1,
                                         ifelse(C==3, 1,
                                                ifelse(C==4, 2,
                                                       ifelse(C==5, 4,
                                                              ifelse(C==6, 3,
                                                                     ifelse(C==7, 3,
                                                                            ifelse(C==8, 4, NA)))))))))

house <- (select (house, -A, -B, -C))

house$tenure <- factor(house$tenure, levels=1:6, labels=c('own', 'empl_sub', 'empl_free', 'rent', 'free', 'nomad'))
house$wall <- factor(house$wall, levels=1:7, labels=c('poles', 'poles_mud_stone', 'mud', 'mud_brick', 'brick', 'concrete', 'other'))
house$roof <- factor(house$roof, levels=1:7, labels=c('grass_leaves', 'mud_grass', 'concrete', 'CGI', 'asbestos', 'tiles', 'other'))
house$cook <- factor(house$cook, levels=1:8, labels=c('firewood', 'paraffin', 'electricity', 'gas', 'charcoal', 'animal_residual', 'biogas', 'other'))
house$light <- factor(house$light, levels=1:9, labels=c('electricity', 'solar', 'gas', 'biogas', 'lampoil', 'candle', 'firewood', 'priv_generator', 'other'))
house$drink1 <- factor(house$drink1, levels=1:11, labels=c('pipe_inside', 'pipe_outside', 'tap_public', 'neighbour', 'vendor', 'truck_tanker', 'well_pump', 'well_nopump', 'surface', 'rain', 'other'))
house$drink2 <- factor(house$drink2, levels=1:11, labels=c('pipe_inside', 'pipe_outside', 'tap_public', 'neighbour', 'vendor', 'truck_tanker', 'well_pump', 'well_nopump', 'surface', 'rain', 'other'))
house$sew <- factor(house$sew, levels=1:5, labels=c('no_toilet', 'flush_toilet', 'pit_latrine', 'VIP', 'other'))

HQI <- house
HQI <- mutate(HQI, tenure1 = ifelse(tenure=='own', 3, 
                                    ifelse(tenure=='empl_sub'|tenure=='empl_free'|tenure=='rent', 2,
                                           ifelse(tenure=='free', 1, 
                                                  ifelse(tenure=='nomad', 0, 0)))))

HQI <- mutate(HQI, wall1 = ifelse(wall=='poles', 1,
                                  ifelse(wall=='poles_mud_stone', 2,
                                         ifelse(wall=='mud', 3,
                                                ifelse(wall=='mud_brick', 4,
                                                       ifelse(wall=='brick', 5,
                                                              ifelse(wall=='concrete', 6, 
                                                                     ifelse(wall=='other', 3, 0))))))))
HQI <- mutate(HQI, roof1 = ifelse(roof=='grass_leaves', 1,
                                  ifelse(roof=='mud_grass'|roof=='CGI'|roof=='other', 2,
                                         ifelse(roof=='concrete'|roof=='tiles', 4,
                                                ifelse(roof=='asbestos', 3, 0)))))

HQI <- mutate(HQI, cook1 = ifelse(cook=='firewood', 1,
                                  ifelse(cook=='paraffin'|cook=='biogas', 4,
                                         ifelse(cook=='electricity', 6,
                                                ifelse(cook=='gas', 5,
                                                       ifelse(cook=='charcoal'|cook=='other', 3,
                                                              ifelse(cook=='animal_residual', 2, 0)))))))



HQI <- mutate(HQI, light1 = ifelse(light=='electricity'|light=='priv_generator', 4,
                                   ifelse(light=='solar'|light=='gas'|light=='biogas', 3,
                                          ifelse(light=='lampoil'| light=='other', 2,
                                                 ifelse(light=='candle'|light=='firewood', 1,0)))))

HQI <- mutate(HQI, drink11 = ifelse(drink1=='pipe_inside', 7,
                                    ifelse(drink1=='pipe_outside', 6,
                                           ifelse(drink1=='tap_public', 5,
                                                  ifelse(drink1=='neighbour'|drink1=='surface'|drink1=='rain', 2,
                                                         ifelse(drink1=='vendor'|drink1=='truck_tanker', 1,
                                                                ifelse(drink1=='well_pump'|drink1=='other', 4, 
                                                                       ifelse(drink1=='well_nopump', 3, 0))))))))

HQI <- mutate(HQI, sew1 = ifelse(sew=='no_toilet', 1,
                                 ifelse(sew=='flush_toilet', 4,
                                        ifelse(sew=='pit_latrine'|sew=='other', 2, 
                                               ifelse(sew=='VIP', 2, 0)))))


HQI$hqi <- HQI$tenure1/3+HQI$wall1/6+HQI$roof1/4+HQI$cook1/6+HQI$light1/4+HQI$drink11/7+HQI$sew1/4
saveRDS(HQI, file.path(dataPath, "/../../HQI2012.rds"))

HQI <- select(HQI, y3_hhid, hqi)
house <- left_join(house, HQI)

HH12 <-left_join(HH12, house)
rm(house)
# -------------------------------------
# education of household members and sum
# of education of all household members
# between the ages of 15 and 55

ed <- read_dta(file.path(dataPath, "HH_SEC_C.dta")) %>%
  select(y3_hhid, indidy3, ed_any=hh_c03, start=hh_c04, end=hh_c08)

ed$ed_any <- as_factor(ed$ed_any) # ever went to school
ed <- mutate(ed, ed_any= ifelse(ed_any == 'YES', 'Yes',
                                ifelse(ed_any == 'NO', 'No', NA)))

ed$end <- as.integer(as.character(ed$end))
ed$end <- ifelse(ed$end %in% 9999, NA, ed$end)

# join with HH12 dataframe
HH12 <- left_join(HH12, ed)
HH12$education <- HH12$end - (HH12$yob + HH12$start)
HH12$education <- ifelse(HH12$ed_any %in% "NO", 0, HH12$education)
HH12 <- select(HH12, -start, -end, -yob)

# remove negative years of education (56 obs)
HH12$education <- ifelse(HH12$education < 0, NA, HH12$education)

# summarise the data: get sum of education
# of household members 15-55 and number of
# household members 15:55
HH12_x <- group_by(HH12, y3_hhid) %>%
  summarise(education1555=sum(education[cage %in% "16-55"], na.rm=T),
            N1555=sum(cage %in% "16-55"))
HH12 <- left_join(HH12, HH12_x); rm(HH12_x)

# -------------------------------------
# death in the family
# -------------------------------------

death <- read_dta(file.path(dataPath, "HH_SEC_S.dta")) 
death$personid <- zap_empty(death$personid)
death <- transmute(death, y3_hhid, death=ifelse(!is.na(personid), 1, 0))

# -------------------------------------
# membership to a credit group
# -------------------------------------

credit <- read_dta(file.path(dataPath, "HH_SEC_O2.dta")) 
credit$personid <- zap_empty(credit$personid)
credit <- transmute(credit, y3_hhid, SACCO=ifelse(!is.na(personid), 1, 0))

HH12 <- left_join(HH12, death) 
HH12 <- left_join(HH12, credit) 
HH12 <- HH012 %>%
  mutate(SACCO = ifelse(is.na(SACCO),0,SACCO)) %>%
  mutate(death= ifelse(is.na(death), 0,death))
rm(ed, credit, death)

#######################################
############### OUTPUT ################
#######################################

oput <- read_dta(file.path(dataPath, "AG_SEC_4A.dta")) %>%
  select(y3_hhid, plotnum, zaocode, inter_crop=ag4a_04,
         harv_area=ag4a_21, qty=ag4a_28, valu=ag4a_29, hybrd=ag4a_08, subseed=ag4a_11)

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

oput_x <- group_by(oput, y3_hhid, plotnum) %>%
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
oput <- select(oput, -valu)

rm(list=c("legumes", "cashCropNPerm", "cashCropsPerm",
          "CTR", "fruit", "vegetables"))

#######################################
############# CHEMICAL ################
#######################################

plot <- read_dta(file.path(dataPath, "AG_SEC_3A.dta")) %>%
  select(y3_hhid, plotnum, zaocode=ag3a_07_2,  soil=ag3a_10, slope_farmer=ag3a_17, irrig=ag3a_18, title=ag3a_28, 
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

# two questions on fallow - make sure they match up correctly
# fallow value of 98 means subject did not know how long plot
# was left fallow
plot$fallow_year <- ifelse(plot$fallow_year %in% 98, NA, plot$fallow_year)
plot$fallow <- ifelse(plot$fallow_year %in% 0, 0, plot$fallow )
plot$fallow <- ifelse(is.na(plot$fallow_year), NA, plot$fallow)
plot <- dplyr::select(plot, -fallow_year, -pest_q_unit)

fert1 <- read_dta(file.path(dataPath, "AG_SEC_3A.dta")) %>%
  dplyr::select(y3_hhid, plotnum, typ=ag3a_48, qty=ag3a_49, vouch=ag3a_50, valu=ag3a_51)

fert2 <- read_dta(file.path(dataPath, "AG_SEC_3A.dta")) %>%
  dplyr::select(y3_hhid, plotnum, typ=ag3a_55, qty=ag3a_56, vouch=ag3a_57, valu=ag3a_58)

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
fdum <- fert1 %>% group_by(y3_hhid, season) %>%
  summarise(DAP = max(DAP), UREA = max(UREA), TSP = max(TSP), CAN = max(CAN), SA = max(SA), generic_NPK = max(generic_NPK), MRP = max(MRP))

fert2 <- mutate(fert2, DAP = ifelse(typ =='DAP', 1, 0) %>% replace(is.na(.), 0))
fert2 <- mutate(fert2, UREA = ifelse(typ == 'UREA', 1, 0)%>% replace(is.na(.), 0))
fert2 <- mutate(fert2, TSP = ifelse(typ =='TSP', 1, 0)%>% replace(is.na(.), 0))
fert2 <- mutate(fert2, CAN = ifelse(typ=='CAN', 1, 0)%>% replace(is.na(.), 0))
fert2 <- mutate(fert2, SA = ifelse(typ == 'SA', 1, 0)%>% replace(is.na(.), 0))
fert2 <- mutate(fert2, generic_NPK = ifelse(typ =='generic NPK (TZA)', 1, 0)%>% replace(is.na(.), 0))
fert2 <- mutate(fert2, MRP = ifelse(typ == 'MRP', 1, 0)%>% replace(is.na(.), 0))
fert2$season <- 'short'       

fdum2 <- fert2 %>% group_by(y3_hhid, season) %>%
  summarise(DAP = max(DAP), UREA = max(UREA), TSP = max(TSP), CAN = max(CAN), SA = max(SA), generic_NPK = max(generic_NPK), MRP = max(MRP))

fdum <- rbind(fdum, fdum2)

fdum <- group_by(fdum, y3_hhid) %>%
  summarise(DAP = max(DAP), UREA = max(UREA), TSP = max(TSP), CAN = max(CAN), SA = max(SA), generic_NPK = max(generic_NPK), MRP = max(MRP))
rm(fdum2)

fdum <- fdum %>% group_by(y3_hhid) %>%
  summarise(DAP = max(DAP), UREA = max(UREA), TSP = max(TSP), CAN = max(CAN), SA = max(SA), generic_NPK = max(generic_NPK), MRP = max(MRP))


fert1 <- select(fert1, -DAP, -UREA, -TSP, -CAN, -SA, -generic_NPK, -MRP)
fert2 <- select(fert2, -DAP, -UREA, -TSP, -CAN, -SA, -generic_NPK, -MRP)

# -------------------------------------
# reorganize data so that observations
# on fertilizer type occupy a single row
# fertilizer is unit of observation
# Data on NPK composition from Sheahan et al (2014), Food Policy
# -------------------------------------

conv <- read.csv(file.path(paste0(dataPath,"/../.."), "Fert_comp.csv")) %>%
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
  group_by(y3_hhid, plotnum) %>%
  summarise(N=sum(Qn, na.rm=TRUE),
            WPnnosub=sum((Qn/N)*Pn, na.rm=TRUE)) %>%
  dplyr::select(-N) %>%
  mutate(WPnnosub = replace(WPnnosub, WPnnosub==0, NA))


fertsub   <- filter(fert, vouch==1) %>%
  group_by(y3_hhid, plotnum) %>%
  summarise(N=sum(Qn, na.rm=TRUE),
            WPnsub=sum((Qn/N)*Pn, na.rm=TRUE)) %>%
  dplyr::select(-N) %>% 
  mutate(WPnsub = replace(WPnsub, WPnsub==0, NA))

fertmix <- filter(fert, vouch %in% c(0,1)) %>%
  group_by(y3_hhid, plotnum) %>%
  summarise(N=sum(Qn, na.rm=TRUE),
            P=sum(Qp, na.rm=TRUE),
            WPn=sum((Qn/N)*Pn, na.rm=TRUE)) %>%
  mutate(WPn = replace(WPn, WPn==0, NA))

# join back with the rest of the data and set N and P to 0 for NA values
fert <- select(fert, y3_hhid, vouch) %>%
  group_by(y3_hhid) %>%
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
############### LABOUR ################
#######################################

lab <- read_dta(file.path(dataPath, "AG_SEC_3A.dta")) %>%
  dplyr::select(y3_hhid, plotnum, ag3a_72_id1:ag3a_74_16 )

# calculate the total labour days for hired and family labour.
bad <- grep( "ag3a_72_id", names( lab ) )
lab <- lab[, -bad]

bad <- names( lab )[( length( lab )-15 ):length( lab )][seq( from=4, to=16, by=4 )]
lab <- lab[, -which( names( lab ) %in% bad )]; rm(bad)

lab <- transmute( lab, y3_hhid, plotnum,
                  fam_lab_days=rowSums( lab[, 3:30], na.rm=TRUE ),
                  hir_lab_days=rowSums( lab[, 32:ncol( lab )], na.rm=TRUE ) )

lab <- transmute(lab, y3_hhid, plotnum, lab=fam_lab_days + hir_lab_days)

# doesn't make sense to have 0 labour on a plot so set values to zero
lab$lab <- ifelse(lab$lab %in% 0, NA, lab$lab)

#######################################
############### ASSETS ################
#######################################

implmt <- read_dta(file.path(dataPath, "AG_SEC_11.dta")) %>%
  dplyr::select(y3_hhid, itemname, qty=ag11_01, valu=ag11_02) %>%
  filter(!qty %in% 0, !is.na(qty), !valu %in% 0, !is.na(valu)) %>%
  transmute(y3_hhid, valu=qty*valu) %>%
  group_by(y3_hhid) %>%
      summarise(value=sum(valu))

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
  select(y3_hhid, animal = lvstckid, owned = lf02_01,
         indigQty = lf02_04_1, improvQty = lf02_04_2)
lvstock$owned <- ifelse(lvstock$owned %in% 1, 1, 0)
lvstock$animal <- as_factor(lvstock$animal)

# remove white space
lvstock$animal <- gsub(" ", "-", lvstock$animal)
lvstock <- filter(lvstock, !is.na(animal) )

# count the number of animals of each class a household owns
lvstock_x <- select(lvstock, y3_hhid, animal, indigQty, improvQty) %>%
  melt(id = c("y3_hhid", "animal")) %>%
  group_by(y3_hhid, animal) %>%
  mutate(class = ifelse(animal %in% LR, "LR",
                        ifelse(animal %in% SR, "SR", 
                               ifelse(animal %in% PIGS, "PIGS_",
                                      ifelse(animal %in% POULTRY, "POULTRY",
                                             ifelse(animal %in% OTHER, "OTHER_")))))) %>%
  group_by(y3_hhid, class) %>%
  summarise(n=sum(value, na.rm=TRUE)) %>%
  dcast(y3_hhid ~ class)

# count the number of each animal a household owns
lvstock_y <- select(lvstock, y3_hhid, animal, indigQty, improvQty) %>%
  melt(id = c("y3_hhid", "animal")) %>%
  group_by(y3_hhid, animal) %>%
  summarise(n=sum(value, na.rm=TRUE)) %>%
  dcast(y3_hhid ~ animal)

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
  select(y3_hhid, lon=lon_dd_mod, lat=lat_dd_mod, dist2Rd=dist01,
         dist2town=dist02, dist2market=dist03, dist2HQ=dist05, avgTemp=clim01,
         avgpPrecip=clim03)

#######################################
############### AREAs #################
#######################################

areas <- read.csv(file.path(dataPath, "/areas_w3.csv")) %>%
  select(y3_hhid, plotnum,
                area_farmer=area.est,
                area_gps=gps_imputed)
areas$y3_hhid <- as.character(areas$y3_hhid)
areas$plotnum <- as.character(areas$plotnum)
areas$area_gps <- ifelse(areas$area_gps %in% 0, NA, areas$area_gps)

# 2012 areas are in acres, change to hecacres in line
# with the 2010 data
areas$area_gps <- areas$area_gps*0.404686 # from wikipedia

# plot ownership
own <- read_dta(file.path(dataPath, "AG_SEC_3A.dta")) %>%
  dplyr::select(y3_hhid, plotnum, own=ag3a_25)
own$own <- ifelse(own$own %in% 1 | own$own %in% 5, 1, 0)

areas <- left_join(areas, own); rm(own)

# total farm holdings
areaTotal <- group_by(areas, y3_hhid) %>%
  summarise(area_tot = sum(area_gps))

#######################################
########## TRANSPORT COSTS ############
#######################################

tc <- read_dta(file.path(dataPath, "AG_SEC_5A.dta")) %>%
  select(y3_hhid, zaocode, trans=ag5a_18, trans_dist=ag5a_19, trans_cost=ag5a_22)
tc$zaocode <- as.integer(tc$zaocode)

tc$trans <- ifelse(tc$trans %in% 1, 1, 0)

#######################################
############ PANEL KEY ################
#######################################

# key for joing individuals and households across years
key <- read_dta(file.path(dataPath, "NPSY3.PANEL.KEY.dta")) %>%
  select(-UPI3) %>% rename(hhid2008 = y1_hhid, hhid2010=y2_hhid)
key$hhid2008 <- zap_empty(key$hhid2008)
key$hhid2010 <- zap_empty(key$hhid2010)
key$y3_hhid <- zap_empty(key$y3_hhid)

#######################################
########### CROSS SECTION #############
#######################################

# joins at the household level (y3_hhid)

TZA2012 <- left_join(location, HH12); rm(location); rm(HH12)
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

TZA2012 <- left_join(TZA2012, CSI2012); rm(CSI2012)
TZA2012 <- left_join(TZA2012, FCS2012); rm(FCS2012)
TZA2012 <- left_join(TZA2012, DDS2012total); rm(DDS2012total)
TZA2012 <- rename(TZA2012, DDS = dietary_diversity_score)
TZA2012 <- left_join(TZA2012, FVS2012); rm(FVS2012)

saveRDS(TZA2012, file.path(dataPath, "TZA2012.rds"))
saveRDS(TZA2012, file.path(dataPath, "/../../TZA2012.rds"))


