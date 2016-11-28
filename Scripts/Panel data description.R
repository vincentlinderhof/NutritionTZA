#######################################
########## TANZANIA         ###########
#######################################

if(Sys.info()["user"] == "Tomas"){
  dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA/2012/Data" }
if(Sys.info()["user"] == "linde069"){
  dataPath <- "D:/Analyses/CIMMYT/NutritionTZA/SurveyData/2012/Data"
  setwd("D:/Analyses/CIMMYT/NutritionTZA")}
# Anne
#dataPath <- dataPath <- "D:/UserData/verbe038/TZA/2012/Data"
#dataPath <- dataPath <- "C:/Users/afver/OneDrive/Documenten/Removable Disk/verbe038/TZA/2012/Data"

# -------------------------------------
#
# Combining the production data
# How many maize growers are there in the LSMS-ISA surveys 2009/2010, 2010/2011 and 2011/2012?
# How many households are there in the panel survey?
# How many households in the (panel) survey do grow maize and grow maize continuously?
#
# -------------------------------------

library(tidyr)
library(dplyr)
library(haven)

# Read location data and construct a dummy variable that indicates participation in 2008/2009
#Location2008 <- readRDS("Data/Location_2008.rds") 
Location2008 <- readRDS("Data/Location_2008.rds") %>%
  dplyr::select(hhid2008=hhid, REGCODE08=REGCODE, DISTCODE08=DISCODE, REGNAME08=REGNAME,
                DISTNAME08=DISNAME, rural08=rural)
Location2008$respin08 <- 1* (Location2008$rural08 >=0 ) 

# Read location data and construct a dummy variable that indicates participation in 2010/2011
Location2010 <- readRDS("Data/Location_2010.rds") %>%
  dplyr::select(hhid2010=hhid, REGCODE10=REGCODE, REGNAME10=REGNAME,
                DISNAME10=DISNAME, rural10=rural)
Location2010$respin10 <- 1* (Location2010$rural10 >=0 ) 

# Read location data and construct a dummy variable that indicates participation in 2012/2013
Location2012 <- readRDS("Data/Location_2012.rds") %>%
  dplyr::select(hhid2012=hhid, REGCODE12=REGCODE, REGNAME12=REGNAME,
                DISNAME12=DISNAME, rural12=rural)
Location2012$respin12 <- 1* (Location2012$rural12 >=0 ) 

# Frequency tables for rural-urban participation in a particular survey
Deducer::frequencies(Location2008$rural08)
Deducer::frequencies(Location2010$rural10)
Deducer::frequencies(Location2012$rural12)

# key for joing individuals and households across years
key <- read_dta(file.path(dataPath, "NPSY3.PANEL.KEY.dta")) %>%
  dplyr::select(-UPI3) %>% rename(hhid2008 = y1_hhid, hhid2010=y2_hhid, hhid2012=y3_hhid)

key$head2008 <- 0
key$head2008 <- ifelse(key$indidy1==1,1,0)
key$head2008 <- ifelse(is.na(key$indidy1),0,key$head2008)

key$head2010 <- 0
key$head2010 <- ifelse(key$indidy2==1,1,0)
key$head2010 <- ifelse(is.na(key$indidy2),0,key$head2010)

key$head2012 <- 0
key$head2012 <- ifelse(key$indidy3==1,1,0)
key$head2012 <- ifelse(is.na(key$indidy3),0,key$head2012)

key$headcode <- 1*(key$head2008==1)+10*(key$head2010==1) + 100*(key$head2012==1)
Deducer::frequencies(key$headcode)
Deducer::frequencies(key$head2008)
Deducer::frequencies(key$head2010)
Deducer::frequencies(key$head2012)

HHpanel <- subset(key, headcode>0)


HHpanel2 <- left_join(HHpanel, Location2008)
HHpanel2 <- left_join(HHpanel2, Location2010)
HHpanel2 <- left_join(HHpanel2, Location2012)

Deducer::frequencies(HHpanel2$indidy1)
Deducer::frequencies(HHpanel2$indidy2)
Deducer::frequencies(HHpanel2$indidy3)

HHpanel2$respin08 <- ifelse(HHpanel2$indidy1>1, 2, HHpanel2$respin08)
HHpanel2$respin10 <- ifelse(HHpanel2$indidy2>1, 2, HHpanel2$respin10)
HHpanel2$respin12 <- ifelse(HHpanel2$indidy3>1, 2, HHpanel2$respin12)

HHpanel2$rural08u <- HHpanel2$rural08*(HHpanel2$indidy1==1)*(HHpanel2$respin08==1)
HHpanel2$rural10u <- HHpanel2$rural10*(HHpanel2$indidy2==1)*(HHpanel2$respin10==1)
HHpanel2$rural12u <- HHpanel2$rural12*(HHpanel2$indidy3==1)*(HHpanel2$respin12==1)

Deducer::frequencies(HHpanel2$respin08)
Deducer::frequencies(HHpanel2$respin10)
Deducer::frequencies(HHpanel2$respin12)

Deducer::frequencies(HHpanel2$rural08u)
Deducer::frequencies(HHpanel2$rural10u)
Deducer::frequencies(HHpanel2$rural12u)

table(HHpanel2$respin08, HHpanel2$rural08)
table(HHpanel2$respin10, HHpanel2$rural10)
table(HHpanel2$respin12, HHpanel2$rural12)

#keyHH <-subset(key, indidy3==1)
#rm(key)

# Read crop production data for households (see crop_production_UGA_20**.r) and link it to the location data (see above)
# Construct cross section of households included in all three LSMS surveys in UGA
Crop_prod_area_2008 <- readRDS("Data/Crop_prod_area_rel_2008.RDS") %>%
  select(hhid2008=hhid, CCNP_area_08=CCNP_area_rel, CCP_area_08=CCP_area_rel, CTR_area_08=CTR_area_rel,
         fruit_area_08=fruit_area_rel, leg_area_08=leg_area_rel, other_area_08=other_area_rel,
         veg_area_08=veg_area_rel, maize_area_08=maize_area_rel, wheat_area_08=wheat_area_rel)
Crop_prod_area_2008$D_maize_2008 <- ifelse(Crop_prod_area_2008$maize_area_08>0,1,0) 

descriptive.table(vars = d(CCNP_area_08, CCP_area_08, CTR_area_08,
                           fruit_area_08, leg_area_08, other_area_08,
                           veg_area_08, maize_area_08, wheat_area_08, D_maize_2008), data= Crop_prod_area_2008, 
                  func.names = c("Mean", "St. Deviation", "Min", "Max", "Valid N"))
Crop_prod_area_2008 <- Crop_prod_area_2008[ c("hhid2008","D_maize_2008")]
Crop_prod_area_2008$D_maize_2008 <- ifelse(is.na(Crop_prod_area_2008$D_maize_2008),0,Crop_prod_area_2008$D_maize_2008) 

HHpanel2 <- dplyr::left_join(HHpanel2, Crop_prod_area_2008)

#RES: the descriptives below take into account the observations of no-head household in particular years. 
descriptive.table(vars = d(CCNP_area_08, CCP_area_08, CTR_area_08,
                           fruit_area_08, leg_area_08, other_area_08,
                           veg_area_08, maize_area_08, wheat_area_08, D_maize_2008), data= HHpanel2, 
                  func.names = c("Mean", "St. Deviation", "Min", "Max", "Valid N"))


Crop_prod_area_2010 <- readRDS("Data/Crop_prod_area_rel_2010.RDS") %>%
  select(hhid2010=hhid, CCNP_area_10=CCNP_area_rel, CCP_area_10=CCP_area_rel, CTR_area_10=CTR_area_rel,
         fruit_area_10=fruit_area_rel, leg_area_10=leg_area_rel, other_area_10=other_area_rel,
         veg_area_10=veg_area_rel, maize_area_10=maize_area_rel, wheat_area_10=wheat_area_rel)
Crop_prod_area_2010$D_maize_2010 <- ifelse(Crop_prod_area_2010$maize_area_10>0,1,0) 

descriptive.table(vars = d(CCNP_area_10, CCP_area_10, CTR_area_10,
                           fruit_area_10, leg_area_10, other_area_10,
                           veg_area_10, maize_area_10, wheat_area_10, D_maize_2010), data= Crop_prod_area_2010, 
                  func.names = c("Mean", "St. Deviation", "Min", "Max", "Valid N"))

Crop_prod_area_2010 <- Crop_prod_area_2010[ c("hhid2010","D_maize_2010")]
Crop_prod_area_2010$D_maize_2010 <- ifelse(is.na(Crop_prod_area_2010$D_maize_2010),0,Crop_prod_area_2010$D_maize_2010) 

#HHpanel2 <- HHpanel2[order(HHpanel2$hhid2010),]
HHpanel2 <- left_join(HHpanel2, Crop_prod_area_2010)
#summary(Crop_prod_area_2010)

descriptive.table(vars = d(CCNP_area_10, CCP_area_10, CTR_area_10,
                           fruit_area_10, leg_area_10, other_area_10,
                           veg_area_10, maize_area_10, wheat_area_10, D_maize_2010), data= HHpanel2, 
                  func.names = c("Mean", "St. Deviation", "Min", "Max", "Valid N"))

#summary(HHpanel2$Crop_prod_area_2010)
Crop_prod_area_2012 <- readRDS("Data/Crop_prod_area_rel_2012.RDS") %>%
  select(hhid2012=hhid, CCNP_area_12=CCNP_area_rel, CCP_area_12=CCP_area_rel, CTR_area_12=CTR_area_rel,
         fruit_area_12=fruit_area_rel, leg_area_12=leg_area_rel, other_area_12=other_area_rel,
         veg_area_12=veg_area_rel, maize_area_12=maize_area_rel, wheat_area_12=wheat_area_rel)
Crop_prod_area_2012$D_maize_2012 <- ifelse(Crop_prod_area_2012$maize_area_12>0,1,0) 

descriptive.table(vars = d(CCNP_area_12, CCP_area_12, CTR_area_12,
                           fruit_area_12, leg_area_12, other_area_12,
                           veg_area_12, maize_area_12, wheat_area_12, D_maize_2012), data= Crop_prod_area_2012, 
                  func.names = c("Mean", "St. Deviation", "Min", "Max", "Valid N"))

Crop_prod_area_2012 <- Crop_prod_area_2012[ c("hhid2012","D_maize_2012")]
Crop_prod_area_2012$D_maize_2012 <- ifelse(is.na(Crop_prod_area_2012$D_maize_2012),0,Crop_prod_area_2012$D_maize_2012) 

HHpanel2 <- dplyr::left_join(HHpanel2, Crop_prod_area_2012, by="hhid2012")


descriptive.table(vars = d(CCNP_area_12, CCP_area_12, CTR_area_12,
                           fruit_area_12, leg_area_12, other_area_12,
                           veg_area_12, maize_area_12, wheat_area_12, D_maize_2012), data= HHpanel2, 
                  func.names = c("Mean", "St. Deviation", "Min", "Max", "Valid N"))

HHpanel2$D_maize_2012 <- Crop_prod_area_2012$D_maize_2012*(HHpanel2$D_maize_2012==Crop_prod_area_2012$D_maize_2012)

HH2008 <- left_join(Location2008, Crop_prod_area_2008)
detach("package:dplyr", unload=TRUE)
library(dplyr)
HH2010 <- left_join(Location2010, Crop_prod_area_2010)
HH2012 <- left_join(Location2012, Crop_prod_area_2012)


# Construct cross section of all households included in one of the three LSMS surveys in TZA
#HHpanel2 <- dplyr::full_join(Location2009, Location2010, by = "HHID")
#HHpanel2 <- dplyr::full_join(HHpanel2, Location2011, by = "HHID")
rm(Location2008, Location2010, Location2012)
rm(Crop_prod_area_2008, Crop_prod_area_2010, Crop_prod_area_2012)

# Construct three full participation variables (1=participation in a particular year, 0=no participation)
HHpanel2$respin08 <- ifelse(is.na(HHpanel2$respin08), 0, HHpanel2$respin08)
HHpanel2$respin10 <- ifelse(is.na(HHpanel2$respin10), 0, HHpanel2$respin10)
HHpanel2$respin12 <- ifelse(is.na(HHpanel2$respin12), 0, HHpanel2$respin12)

# Frequency tables of participation 
Deducer::frequencies(HHpanel2$rural08)
Deducer::frequencies(HHpanel2$rural10)
Deducer::frequencies(HHpanel2$rural12)

# Construct a participation in the panel +1=2009, +10=2010 and +100=2011
HHpanel2$respin_code <- HHpanel2$respin09 *1 + HHpanel2$respin10 *10 +HHpanel2$respin11 *100
Deducer::frequencies(HHpanel2$respin_code)

# Construct a participation in the panel (rural=2, urban=1, and no participation=0)
HHpanel2$rural09 <- HHpanel2$rural09+1
HHpanel2$rural10 <- HHpanel2$rural10+1
HHpanel2$rural11 <- HHpanel2$rural11+1
HHpanel2$rural09 <- ifelse(is.na(HHpanel2$rural09), 0, HHpanel2$rural09)
HHpanel2$rural10 <- ifelse(is.na(HHpanel2$rural10), 0, HHpanel2$rural10)
HHpanel2$rural11 <- ifelse(is.na(HHpanel2$rural11), 0, HHpanel2$rural11)

HHpanel2$rural_code <- HHpanel2$rural09 + HHpanel2$rural10 *10 +HHpanel2$rural11 *100
Deducer::frequencies(HHpanel2$rural_code)

# Construct a maizegrowers in the surveys (maize grower=1, non-maize grower=0)
HHpanel2$D_maize_09 <- ifelse(is.na(HHpanel2$maize_area_09), 0, 1)
HHpanel2$D_maize_10 <- ifelse(is.na(HHpanel2$maize_area_10), 0, 1)
HHpanel2$D_maize_11 <- ifelse(is.na(HHpanel2$maize_area_11), 0, 1)
HHpanel2$maize_code <- HHpanel2$D_maize_09 + HHpanel2$D_maize_10 *10 +HHpanel2$D_maize_11 *100
Deducer::frequencies(HHpanel2$maize_code)
table(HHpanel2$respin_code,HHpanel2$maize_code)

