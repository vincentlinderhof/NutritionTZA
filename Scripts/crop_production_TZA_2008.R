# -------------------------------------
#' crop production variables:
#'  Tanzania 2008
#'  There is a grand total of 5 possible
#'  area measurements that could be used
#'  
#'     1. harvested area (farmer reported)
#'     2. plot area (farmer reported)
#'     3. plot area (gps reported, 25% of plots)
#'     4. plot area (farmer + gps measured)
#'     5. relative area (explained below)
#'
#'   In addition there are several crop
#'   groups to consider. In this file I 
#'   consider 7 crop groups
#'   
#'      1. fruit
#'      2. CCP: Cash Crop Permanent
#'      3. CCNP: Cash Crop Non Permanent
#'      4. veg: vegetable
#'      5. leg: legumes
#'      6. CTR: Cereals Tubers and Roots
#'      
#'   In addition both maize and wheat have
#'   their own group.
#'   
#'   Output: tbd
# -------------------------------------

# -------------------------------------
# load packages and set working directory
# -------------------------------------

library(tidyr)

library(haven)
library(foreign)

detach("package:dplyr", character.only = TRUE)
library("dplyr", character.only = TRUE)


if(Sys.info()["user"] == "Tomas"){
  dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA/2008/Data"
} else {
  dataPath <- "D:/Analyses/CIMMYT/TZA_Anne/SurveyData/2008/Data"
}

# -------------------------------------
# read in crop information - crop level
# -------------------------------------

# read in data and select key variables
crop_prod <- read_dta(file.path(dataPath, "TZNPS1AGDTA_E/SEC_4A.dta")) %>%
    select(hhid, plotnum, crop_code=zaocode, harv_area=s4aq8,
                crop_qty_harv=s4aq15)

# use crop_code as an integer. 
crop_prod$crop_code <- as.integer(crop_prod$crop_code)

# harv area is in acres -> change to hectares
crop_prod$harv_area <- crop_prod$harv_area*0.404686

# -------------------------------------
# read in the area information
# only 25% of the areas are GPS measured
# in 2008
# --------------------------------------

land <- read_dta(file.path(dataPath, "/TZNPS1AGDTA_E/SEC_2A.dta")) %>%
  rename(area_farmer = s2aq4, area_gps = area)

# measurements are in acres -> change to hectacres
land$area_gps <- land$area_gps*0.404686
land$area_farmer <- land$area_farmer*0.404686

# make a single area combining gps, non-gps and mixed
land$area_mix <- ifelse(is.na(land$area_gps), land$area_farmer, land$area_gps)
land$area_mix <- ifelse(land$area_mix %in% 0, NA, land$area_mix)

# -------------------------------------
# join land information with the crop
# production variables
# -------------------------------------

crop_prod <- left_join(crop_prod, land); rm(land)

# -------------------------------------
#' make a "relative" area variable.
#' because gps and farmer reported areas
#' are made at the plot level we do not
#' have an accurate measurement of the
#' area planted for a particular crop
#' or crop group. Instead we have the
#' farmer reported harvest area and the
#' farmer reported plot area. Here I
#' create a variable that assumes farmers
#' can work out "relative" areas, but not
#' absolute/actual areas, and multiply this
#' relative area by the gps measured area
#' for the plot. This hopefully yields a
#' more accurate and continous area
#' measurement. However, because on 25% of
#' the plot areas in 2008 were measured by
#' gps, this only makes sense 25% of the
#' time.
# -------------------------------------

crop_prod$area_rel <- crop_prod$harv_area/crop_prod$area_farmer*crop_prod$area_mix

# -------------------------------------
#' make a variable to record which food 
#' group each crop belongs to:
#' 
#'      1. fruit
#'      2. CCP: Cash Crop Permanent
#'      3. CCNP: Cash Crop Non Permanent
#'      4. veg: vegetable
#'      5. leg: legumes
#'      6. CTR: Cereals Tubers and Roots
# -------------------------------------

fruit <- c(70:74, 76:85, 97:99, 67, 38, 39)
CCP <- c(53:61, 63:66, 18, 34, 21, 75, 44:46) 
CTR <- c(12:15, 17, 22:27) 
CCNP <- c(50, 51, 53, 62, 19) 
veg <- c(86:96, 100, 101)
leg <- c(31, 32, 33, 35, 36, 37, 41, 42, 43, 47, 48)
maize <- c(11)
wheat <- c(16)
other <- c(fruit, CCP, CTR, CCNP, veg, leg, maize, wheat)

# get a variable with the crop group
crop_prod$type <- character(nrow(crop_prod))
crop_prod <- mutate(crop_prod,
            type=ifelse(crop_code %in% fruit, "fruit", type),
            type=ifelse(crop_code %in% CCP, "CCP", type),
            type=ifelse(crop_code %in% CTR, "CTR", type),
            type=ifelse(crop_code %in% CCNP, "CCNP", type),
            type=ifelse(crop_code %in% veg, "veg", type),
            type=ifelse(crop_code %in% leg, "leg", type),
            type=ifelse(crop_code %in% maize, "maize", type), # maize has crop code 11
            type=ifelse(crop_code %in% wheat, "wheat", type), # wheat has crop code 16
            type=ifelse(!crop_code %in% other, "other", type)) 

# -------------------------------------
#' finally make 5 dataframes corresponding
#' to each of the possible area measurements
#' that can be used to create a total area
#' per food group variable. These are:
#' 
#'     1. harvested area (farmer reported)
#'     2. plot area (farmer reported)
#'     3. plot area (gps reported, 25% of plots)
#'     4. plot area (farmer + gps measured)
#'     5. relative area 
# -------------------------------------

# 1. harvested area (farmer reported)
crop_prod_v <- select(crop_prod, hhid, type, harv_area)
crop_prod_harv_area <- group_by(crop_prod_v, hhid, type) %>%
  summarise_each(funs(sum)) %>% spread(key = type, value = harv_area) 
names(crop_prod_harv_area) <- paste0(names(crop_prod_harv_area), "_harv_area")
names(crop_prod_harv_area)[1] <- "hhid"

# 2. plot area (farmer reported) 
crop_prod_w <- select(crop_prod, hhid, type, area_farmer)
crop_prod_area_farmer <- group_by(crop_prod_w, hhid, type) %>%
  summarise_each(funs(sum)) %>% spread(key = type, value = area_farmer) 
names(crop_prod_area_farmer) <- paste0(names(crop_prod_area_farmer), "_area_farmer")
names(crop_prod_area_farmer)[1] <- "hhid"

# 3. plot area (gps reported, 25% of plots)
crop_prod_x <- select(crop_prod, hhid, type, area_gps)
crop_prod_area_gps <- group_by(crop_prod_x, hhid, type) %>%
  summarise_each(funs(sum)) %>% spread(key = type, value = area_gps) 
names(crop_prod_area_gps) <- paste0(names(crop_prod_area_gps), "_area_gps")
names(crop_prod_area_gps)[1] <- "hhid"

# 4. plot area (farmer + gps measured)
crop_prod_y <- select(crop_prod, hhid, type, area_mix)
crop_prod_area_mix <- group_by(crop_prod_y, hhid, type) %>%
  summarise_each(funs(sum)) %>% spread(key = type, value = area_mix) 
names(crop_prod_area_mix) <- paste0(names(crop_prod_area_mix), "_area_mix")
names(crop_prod_area_mix)[1] <- "hhid"

# 5. relative area 
crop_prod_z <- select(crop_prod, hhid, type, area_rel)
crop_prod_area_rel <- group_by(crop_prod_z, hhid, type) %>%
  summarise_each(funs(sum)) %>% spread(key = type, value = area_rel) 
names(crop_prod_area_rel) <- paste0(names(crop_prod_area_rel), "_area_rel")
names(crop_prod_area_rel)[1] <- "hhid"

rm(crop_prod_v, crop_prod_w, crop_prod_x,
   crop_prod_y, crop_prod_z)

library(Deducer)
# Total crop area farmer reported
crop_prod_x <-crop_prod_area_farmer
crop_prod_x$CCNP_area_farmer   <- ifelse(is.na(crop_prod_x$CCNP_area_farmer>0) ,0,crop_prod_x$CCNP_area_farmer)
crop_prod_x$CCP_area_farmer    <- ifelse(is.na(crop_prod_x$CCP_area_farmer>0)  ,0,crop_prod_x$CCP_area_farmer)
crop_prod_x$CTR_area_farmer    <- ifelse(is.na(crop_prod_x$CTR_area_farmer>0)  ,0,crop_prod_x$CTR_area_farmer)
crop_prod_x$fruit_area_farmer  <- ifelse(is.na(crop_prod_x$fruit_area_farmer>0),0,crop_prod_x$fruit_area_farmer)
crop_prod_x$leg_area_farmer    <- ifelse(is.na(crop_prod_x$leg_area_farmer>0)  ,0,crop_prod_x$leg_area_farmer)
crop_prod_x$veg_area_farmer    <- ifelse(is.na(crop_prod_x$veg_area_farmer>0)  ,0,crop_prod_x$veg_area_farmer)
crop_prod_x$maize_area_farmer  <- ifelse(is.na(crop_prod_x$maize_area_farmer>0),0,crop_prod_x$maize_area_farmer)
crop_prod_x$wheat_area_farmer  <- ifelse(is.na(crop_prod_x$wheat_area_farmer>0),0,crop_prod_x$wheat_area_farmer)
crop_prod_x$other_area_farmer  <- ifelse(is.na(crop_prod_x$other_area_farmer>0),0,crop_prod_x$other_area_farmer)

crop_prod_x$total_area_farmer <- crop_prod_x$CCNP_area_farmer    + crop_prod_x$CCP_area_farmer + 
                                   crop_prod_x$CTR_area_farmer   + crop_prod_x$fruit_area_farmer +
                                   crop_prod_x$leg_area_farmer   + crop_prod_x$veg_area_farmer + 
                                   crop_prod_x$maize_area_farmer + crop_prod_x$wheat_area_farmer + 
                                   crop_prod_x$other_area_farmer
descriptive.table(vars = d(total_area_farmer), data= crop_prod_x, 
                  func.names = c("Mean", "St. Deviation", "Min", "Max", "Valid N"))

crop_prod_x$total_area_farmer <- ifelse(crop_prod_x$total_area_farmer == 0, NA, crop_prod_x$total_area_farmer)
descriptive.table(vars = d(total_area_farmer), data= crop_prod_x, 
                  func.names = c("Mean", "St. Deviation", "Min", "Max", "Valid N"))
crop_prod_x <- crop_prod_x[ c("hhid", "total_area_farmer") ]
crop_prod_area_farmer <- left_join(crop_prod_area_farmer, crop_prod_x)

#Total area by GPS registration (25% of the households!)
crop_prod_x <-crop_prod_area_gps
crop_prod_x$CCNP_area_gps   <- ifelse(is.na(crop_prod_x$CCNP_area_gps>0) ,0,crop_prod_x$CCNP_area_gps)
crop_prod_x$CCP_area_gps    <- ifelse(is.na(crop_prod_x$CCP_area_gps>0)  ,0,crop_prod_x$CCP_area_gps)
crop_prod_x$CTR_area_gps    <- ifelse(is.na(crop_prod_x$CTR_area_gps>0)  ,0,crop_prod_x$CTR_area_gps)
crop_prod_x$fruit_area_gps  <- ifelse(is.na(crop_prod_x$fruit_area_gps>0),0,crop_prod_x$fruit_area_gps)
crop_prod_x$leg_area_gps    <- ifelse(is.na(crop_prod_x$leg_area_gps>0)  ,0,crop_prod_x$leg_area_gps)
crop_prod_x$veg_area_gps    <- ifelse(is.na(crop_prod_x$veg_area_gps>0)  ,0,crop_prod_x$veg_area_gps)
crop_prod_x$maize_area_gps  <- ifelse(is.na(crop_prod_x$maize_area_gps>0),0,crop_prod_x$maize_area_gps)
crop_prod_x$wheat_area_gps  <- ifelse(is.na(crop_prod_x$wheat_area_gps>0),0,crop_prod_x$wheat_area_gps)
crop_prod_x$other_area_gps  <- ifelse(is.na(crop_prod_x$other_area_gps>0),0,crop_prod_x$other_area_gps)

crop_prod_x$total_area_gps <- crop_prod_x$CCNP_area_gps + crop_prod_x$CCP_area_gps+ 
  crop_prod_x$CTR_area_gps   + crop_prod_x$fruit_area_gps +
  crop_prod_x$leg_area_gps   + crop_prod_x$other_area_gps + 
  crop_prod_x$maize_area_gps + crop_prod_x$wheat_area_gps + 
  crop_prod_x$veg_area_gps
descriptive.table(vars = d(total_area_gps), data= crop_prod_x, 
                  func.names = c("Mean", "St. Deviation", "Min", "Max", "Valid N"))

crop_prod_x$total_area_gps <- ifelse(crop_prod_x$total_area_gps == 0, NA, crop_prod_x$total_area_gps)

descriptive.table(vars = d(total_area_gps), data= crop_prod_x, 
                  func.names = c("Mean", "St. Deviation", "Min", "Max", "Valid N"))

crop_prod_x <- crop_prod_x[ c("hhid", "total_area_gps") ]
crop_prod_area_farmer <- left_join(crop_prod_area_gps, crop_prod_x)

#Total area mix of farm reported and GPS registration
crop_prod_x <-crop_prod_area_mix
crop_prod_x$CCNP_area_mix   <- ifelse(is.na(crop_prod_x$CCNP_area_mix>0) ,0,crop_prod_x$CCNP_area_mix)
crop_prod_x$CCP_area_mix    <- ifelse(is.na(crop_prod_x$CCP_area_mix>0)  ,0,crop_prod_x$CCP_area_mix)
crop_prod_x$CTR_area_mix    <- ifelse(is.na(crop_prod_x$CTR_area_mix>0)  ,0,crop_prod_x$CTR_area_mix)
crop_prod_x$fruit_area_mix  <- ifelse(is.na(crop_prod_x$fruit_area_mix>0),0,crop_prod_x$fruit_area_mix)
crop_prod_x$leg_area_mix    <- ifelse(is.na(crop_prod_x$leg_area_mix>0)  ,0,crop_prod_x$leg_area_mix)
crop_prod_x$veg_area_mix    <- ifelse(is.na(crop_prod_x$veg_area_mix>0)  ,0,crop_prod_x$veg_area_mix)
crop_prod_x$maize_area_mix  <- ifelse(is.na(crop_prod_x$maize_area_mix>0),0,crop_prod_x$maize_area_mix)
crop_prod_x$wheat_area_mix  <- ifelse(is.na(crop_prod_x$wheat_area_mix>0),0,crop_prod_x$wheat_area_mix)
crop_prod_x$other_area_mix  <- ifelse(is.na(crop_prod_x$other_area_mix>0),0,crop_prod_x$other_area_mix)

crop_prod_x$total_area_mix <- crop_prod_x$CCNP_area_mix+crop_prod_x$CCP_area_mix + 
  crop_prod_x$CTR_area_mix   + crop_prod_x$fruit_area_mix +
  crop_prod_x$leg_area_mix   + crop_prod_x$maize_area_mix + 
  crop_prod_x$wheat_area_mix + crop_prod_x$other_area_mix + 
  crop_prod_x$veg_area_mix
descriptive.table(vars = d(total_area_mix), data= crop_prod_x, 
                  func.names = c("Mean", "St. Deviation", "Min", "Max", "Valid N"))

crop_prod_x$total_area_mix <- ifelse(crop_prod_x$total_area_mix == 0, NA, crop_prod_x$total_area_mix)

descriptive.table(vars = d(total_area_mix), data= crop_prod_x, 
                  func.names = c("Mean", "St. Deviation", "Min", "Max", "Valid N"))

crop_prod_x <- crop_prod_x[ c("hhid", "total_area_gps") ]
crop_prod_area_farmer <- left_join(crop_prod_area_gps, crop_prod_x)

crop_prod_area_gps$total_area_gps <- crop_prod_area_gps$CCNP_area_gps  + crop_prod_area_gps$CCP_area_gps + 
                                     crop_prod_area_gps$CTR_area_gps   + crop_prod_area_gps$fruit_area_gps +
                                     crop_prod_area_gps$leg_area_gps   + crop_prod_area_gps$maize_area_gps +
                                     crop_prod_area_gps$wheat_area_gps + crop_prod_area_gps$other_area_gps + 
                                     crop_prod_area_gps$veg_area_gps

crop_prod_area_mix$total_area_mix <- crop_prod_area_mix$CCNP_area_mix + crop_prod_area_mix$CCP_area_mix+ 
  crop_prod_area_mix$CTR_area_mix   + crop_prod_area_mix$fruit_area_mix +
  crop_prod_area_mix$leg_area_mix   + crop_prod_area_mix$maize_area_mix +
  crop_prod_area_mix$wheat_area_mix + crop_prod_area_mix$other_area_mix + 
  crop_prod_area_mix$veg_area_mix

crop_prod_area_rel$total_area_rel <- crop_prod_area_rel$CCNP_area_rel+crop_prod_area_rel$CCP_area_rel+ 
  crop_prod_area_rel$CTR_area_rel + crop_prod_area_rel$fruit_area_rel+
  crop_prod_area_rel$leg_area_rel + crop_prod_area_rel$maize_area_rel+
  crop_prod_area_rel$wheat_area_rel + crop_prod_area_rel$other_area_rel+ 
  crop_prod_area_rel$veg_area_rel

crop_prod_harv_area$total_harv_area <- crop_prod_harv_area$CCNP_harv_area + crop_prod_harv_area$CCP_harv_area+ 
  crop_prod_harv_area$CTR_harv_area   + crop_prod_harv_area$fruit_harv_area +
  crop_prod_harv_area$leg_harv_area   + crop_prod_harv_area$maize_harv_area +
  crop_prod_harv_area$wheat_harv_area + crop_prod_harv_area$other_harv_area + 
  crop_prod_harv_area$veg_harv_area



descriptive.table(vars = d(CCNP_area_farmer, CCP_area_farmer, CTR_area_farmer, fruit_area_farmer,
                           leg_area_farmer, veg_area_farmer, other_area_farmer, total_area_farmer,
                           maize_area_farmer, wheat_area_farmer), data= crop_prod_area_farmer, 
                  func.names = c("Mean", "St. Deviation", "Min", "Max", "Valid N"))

descriptive.table(vars = d(CCNP_area_gps, CCP_area_gps, CTR_area_gps, fruit_area_gps,
                           leg_area_gps, veg_area_gps, other_area_gps, total_area_gps,
                           maize_area_gps, wheat_area_gps), data= crop_prod_area_gps, 
                  func.names = c("Mean", "St. Deviation", "Min", "Max", "Valid N"))

descriptive.table(vars = d(CCNP_area_mix, CCP_area_mix, CTR_area_mix, fruit_area_mix,
                           leg_area_mix, veg_area_mix, other_area_mix, total_area_mix,
                           maize_area_mix, wheat_area_mix), data= crop_prod_area_mix, 
                  func.names = c("Mean", "St. Deviation", "Min", "Max", "Valid N"))

descriptive.table(vars = d(CCNP_area_rel, CCP_area_rel, CTR_area_rel, fruit_area_rel,
                           leg_area_rel, veg_area_rel, other_area_rel, total_area_rel,
                           maize_area_rel, wheat_area_rel), data= crop_prod_area_rel, 
                  func.names = c("Mean", "St. Deviation", "Min", "Max", "Valid N"))

descriptive.table(vars = d(CCNP_harv_area, CCP_harv_area, CTR_harv_area, fruit_harv_area,
                           leg_harv_area, veg_harv_area, other_harv_area, total_harv_area,
                           maize_harv_area, wheat_harv_area), data= crop_prod_harv_area, 
                  func.names = c("Mean", "St. Deviation", "Min", "Max", "Valid N"))

TZA2008HH <- left_join(TZA2008HH, crop_prod_area_rel) 
rm(CCNP, CCP,CTR,fruit, leg, other, veg, maize, wheat)

saveRDS(crop_prod_harv_area,   "Data/Crop_prod_harv_area_2008.rds")
saveRDS(crop_prod_area_farmer, "Data/Crop_prod_area_farmer_2008.rds")
saveRDS(crop_prod_area_gps,    "Data/Crop_prod_area_gps_2008.rds")
saveRDS(crop_prod_area_rel,    "Data/Crop_prod_area_rel_2008.rds")
saveRDS(crop_prod_area_mix,    "Data/Crop_prod_area_mix_2008.rds")


rm(TZA2008HH_x)
rm("crop_prod_area_farmer","crop_prod_area_farmer","crop_prod_area_gps", "crop_prod_harv_area" )
rm("crop_prod", "crop_prod_area_mix", "crop_prod_x")

