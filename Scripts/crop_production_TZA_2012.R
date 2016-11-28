# -------------------------------------
#' crop production variables:
#'  Tanzania 2012
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
library(dplyr)
library(haven)

#if(Sys.info()["user"] == "Tomas"){
#  dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA/2012/Data"
#} else {
#  dataPath <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/TZA/2012/Data"
#}

# -------------------------------------
# read in crop information - crop level
# -------------------------------------

# read in data and select key variables
crop_prod <- read_dta(file.path(dataPath, "AG_SEC_4A.dta")) %>%
  dplyr::select(hhid=y3_hhid, plotnum, crop_code=zaocode, harv_area=ag4a_21,
         crop_qty_harv=ag4a_28)

# use crop_code as an integer. 
crop_prod$crop_code <- as.integer(crop_prod$crop_code)

# harv area is in acres -> change to hectares
crop_prod$harv_area <- crop_prod$harv_area*0.404686

# -------------------------------------
#' read in the area information
# --------------------------------------

land <- read.csv(file.path(dataPath, "/areas_w3.csv")) %>%
  select(hhid=y3_hhid, plotnum, area_farmer=area.est, area_gps=gps_imputed)

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

land$y3_hhid <- as.character(land$y3_hhid)
land$plotnum <- as.character(land$plotnum)
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
#' measurement. 
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
                    type=ifelse(crop_code %in% wheat, "wheat", type),
                    type=ifelse(!crop_code %in% other, "other", type)) # wheat has crop code 16

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

saveRDS(crop_prod_harv_area,   "Data/Crop_prod_harv_area_2012.rds")
saveRDS(crop_prod_area_farmer, "Data/Crop_prod_area_farmer_2012.rds")
saveRDS(crop_prod_area_gps,    "Data/Crop_prod_area_gps_2012.rds")
saveRDS(crop_prod_area_rel,    "Data/Crop_prod_area_rel_2012.rds")
saveRDS(crop_prod_area_mix,    "Data/Crop_prod_area_mix_2012.rds")

rm(crop_prod_v, crop_prod_w, crop_prod_x,
   crop_prod_y, crop_prod_z)
