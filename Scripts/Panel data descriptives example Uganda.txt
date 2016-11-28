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

if(Sys.info()["user"] == "Tomas"){ 
  dataPath <- "C:/Users/Tomas/Documents/LEI/data/UGA/2009_10/Data" }
if(Sys.info()["user"] == "linde069"){
  dataPath <- "D:/Analyses/CIMMYT/NutritionUGA/SurveyData/2009_10/Data"
  setwd("D:/Analyses/CIMMYT/NutritionUGA")
}

# Read location data and construct a dummy variable that indicates participation in 2009/2010
Location2009 <- readRDS("Data/Location_2009.rds") %>%
  dplyr::select(HHID, REGCODE09=REGCODE, DISTCODE09=DISTCODE, REGNAME09=REGNAME,
                DISTNAME09=DISTNAME, rural09=rural)
Location2009$respin09 <- 1* (Location2009$rural09 >=0 ) 

# Read location data and construct a dummy variable that indicates participation in 2010/2011
Location2010 <- readRDS("Data/Location_2010.rds") %>%
  dplyr::select(HHID, REGCODE10=REGCODE, REGNAME10=REGNAME,
                DISNAME10=DISNAME, rural10=rural)
Location2010$respin10 <- 1* (Location2010$rural10 >=0 ) 

# Read location data and construct a dummy variable that indicates participation in 2011/2012
Location2011 <- readRDS("Data/Location_2011.rds") %>%
  dplyr::select(HHID, REGCODE11=REGCODE, REGNAME11=REGNAME,
                DISNAME11=DISNAME, rural11=rural)
Location2011$respin11 <- 1* (Location2011$rural11 >=0 ) 

# Frequency tables for rural-urban participation in a particular survey
Deducer::frequencies(Location2009$rural09)
Deducer::frequencies(Location2010$rural10)
Deducer::frequencies(Location2011$rural11)

# Read crop production data for households (see crop_production_UGA_20**.r) and link it to the location data (see above)
Crop_prod_area_2009 <- readRDS("Data/Crop_prod_area_rel_2009.RDS") %>%
  select(HHID, CCNP_area_09=CCNP_area_rel, CCP_area_09=CCP_area_rel, CTR_area_09=CTR_area_rel,
  fruit_area_09=fruit_area_rel, leg_area_09=leg_area_rel, other_area_09=other_area_rel,
  veg_area_09=veg_area_rel, maize_area_09=maize_area_rel, wheat_area_09=wheat_area_rel)
Location2009 <- dplyr::left_join(Location2009, Crop_prod_area_2009)

Crop_prod_area_2010 <- readRDS("Data/Crop_prod_area_rel_2010.RDS") %>%
  select(HHID, CCNP_area_10=CCNP_area_rel, CCP_area_10=CCP_area_rel, CTR_area_10=CTR_area_rel,
         fruit_area_10=fruit_area_rel, leg_area_10=leg_area_rel, other_area_10=other_area_rel,
         veg_area_10=veg_area_rel, maize_area_10=maize_area_rel, wheat_area_10=wheat_area_rel)
Location2010 <- dplyr::left_join(Location2010, Crop_prod_area_2010)

Crop_prod_area_2011 <- readRDS("Data/Crop_prod_area_rel_2011.RDS") %>%
  select(HHID, CCNP_area_11=CCNP_area_rel, CCP_area_11=CCP_area_rel, CTR_area_11=CTR_area_rel,
         fruit_area_11=fruit_area_rel, leg_area_11=leg_area_rel, other_area_11=other_area_rel,
         veg_area_11=veg_area_rel, maize_area_11=maize_area_rel, wheat_area_11=wheat_area_rel)
Crop_prod_area_2011$HHID <- as.character(Crop_prod_area_2011$HHID )
Location2011 <- dplyr::left_join(Location2011, Crop_prod_area_2011)

# Construct cross section of households included in all three LSMS surveys in UGA
HHpanel <-left_join(Location2009, Location2010)
HHpanel <-left_join(HHpanel, Location2011)

# Construct cross section of all households included in one of the three LSMS surveys in UGA
HHpanel2 <- dplyr::full_join(Location2009, Location2010, by = "HHID")
HHpanel2 <- dplyr::full_join(HHpanel2, Location2011, by = "HHID")
rm(Location2009, Location2010, Location2011)
rm(Crop_prod_area_2009, Crop_prod_area_2010, Crop_prod_area_2011)

# Construct three full participation variables (1=participation in a particular year, 0=no participation)
HHpanel2$respin09 <- ifelse(is.na(HHpanel2$respin09), 0, HHpanel2$respin09)
HHpanel2$respin10 <- ifelse(is.na(HHpanel2$respin10), 0, HHpanel2$respin10)
HHpanel2$respin11 <- ifelse(is.na(HHpanel2$respin11), 0, HHpanel2$respin11)

# Frequency tables of participation 
Deducer::frequencies(HHpanel2$rural09)
Deducer::frequencies(HHpanel2$rural10)
Deducer::frequencies(HHpanel2$rural11)

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



