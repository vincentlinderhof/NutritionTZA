# -------------------------------------------------------
# --- THIS FILES IS SOURCED IN TZA_2010.r ---------------
# -------------------------------------------------------
# Tom
# dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA/2010/Data"

# LEI Path
# dataPath <- "W:/LEI/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/TZA/2010/Data"

# Vincent at home

#if(Sys.info()["user"] == "Tomas"){
#  dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA/2010/Data"
#} else {
#  dataPath <- "D:/Analyses/CIMMYT/NutritionTZA/SurveyData/2010/Data"
#}
#setwd("D:/Analyses/CIMMYT/NutritionTZA")

# load packages
library("haven")
library("stringr")
library("reshape2")
library("dplyr", lib.loc="C:/Program Files/R/R-3.3.1/library")
library("markdown")
library(tidyr) # Necessary for spread function in mutate command
library(Deducer) # necessary for descriptives.tables

options(scipen=999)

# ***************************************************************************************************
#Creation of DDS and FVS
# ***************************************************************************************************

FOOD2010 <- read_dta(file.path(dataPath, "/TZNPS2HH3DTA/HH_SEC_K1.dta")) %>%
  dplyr::select(hhid=y2_hhid, itemcode, hh_k01_2)

# How food items are connected to food groups, See FAO (2013) 
# 2-13       100       cereals = mean(cereals, na.rm = TRUE),
# 14-20      200       rootsandtubers = mean(rootsandtubers, na.rm = TRUE),
# 21-23      300       vegetables = mean(vegetables, na.rm=TRUE),
# 24         400       pulsesandnuts = mean(pulsesandnuts, na.rm=TRUE),
# 25-28      500       fruits = mean(fruits, na.rm=TRUE),
# 29-31      600       meat = mean(meat, na.rm=TRUE),
# 32-35      700       eggs = mean(eggs, na.rm=TRUE),
# 36-38      800       fishandseafood= mean(fishandseafood, na.rm=TRUE),
# 39-48      900       milkandmilkproducts= mean(milkandmilkproducts, na.rm=TRUE),
# 48-50     1000       oilsandfats=mean(oilsandfats, na.rm=TRUE),
# 50-53     1100       sugar=mean(sugar, na.rm=TRUE),
# 53-60     1200       condiments=mean(condiments, na.rm=TRUE))

# Construct dummy variables for food items
NUTR2010 <-
  mutate(FOOD2010, countv1 = ifelse(hh_k01_2 == 1, 1, ifelse(NA))) %>%
  group_by(hhid) %>%
  spread(itemcode, countv1) %>%
#  select( -hh_k01_2) %>%
  filter (! duplicated(hhid)) %>%
  replace(is.na(.), 0)

# Remove skq1 and skcode variables which are useless at household level
NUTR2010 <-NUTR2010[-c(2)]

# Columns correspond to list of food items!
# sum fooditems into 12 foodgroups for FVS: columns correspond to list of food items!
NUTR2010$cereals             <- 1*(rowSums(NUTR2010[2:13]) > 0)
NUTR2010$rootsandtubers      <- 1*(rowSums(NUTR2010[14:20]) > 0)
NUTR2010$vegetables          <- 1*(rowSums(NUTR2010[29:31]) > 0)
NUTR2010$pulsesandnuts       <- 1*(rowSums(NUTR2010[24:28]) > 0)
NUTR2010$fruits              <- 1*(rowSums(NUTR2010[32:34]) > 0)
NUTR2010$meat                <- 1*(rowSums(NUTR2010[36:41]) > 0)
NUTR2010$eggs                <- 1*(rowSums(NUTR2010[42]) > 0)
NUTR2010$fishandseafood      <- 1*(rowSums(NUTR2010[43:45]) > 0)
NUTR2010$milkandmilkproducts <- 1*(rowSums(NUTR2010[46:48]) > 0)
NUTR2010$oilsandfats         <- 1*(rowSums(NUTR2010[49:50]) > 0)
NUTR2010$sugar               <- 1*(rowSums(NUTR2010[21:23])+rowSums(NUTR2010[35]) > 0)
NUTR2010$condiments          <- 1*(rowSums(NUTR2010[51:60]) > 0)

#Alcoholic beverages included in FOOD group 12, see FAO (2013)!
# Colums        group item 
# 2-13          100   cereals 
# 14-20         200   rootsandtubers
# 19-21         300   vegetables
# 24-28         400   pulsesandnuts
# 32-34         500   fruits
# 36-41         600   meat
# 42            700   eggs
# 43-45         800   fishandseafood
# 46-48         900   milkandmilkproducts
# 49-50         1000  oilsandfats
# 21-23, 35     1100  sugar
# 51-60         1200  condiments

# Construction of DDS 
NUTR2010 <- mutate(NUTR2010, 
                   DDS =  cereals + 
                     rootsandtubers +
                     vegetables +
                     pulsesandnuts +
                     fruits +
                     meat +
                     eggs +
                     fishandseafood +
                     milkandmilkproducts +
                     oilsandfats +
                     sugar +
                     condiments )

# Construction of FVS
NUTR2010$FVS <- rowSums(NUTR2010[2:60])

# Construction of household database with FVS and DDS variable
by_hhid <- group_by(NUTR2010, hhid)

# Remove the coolums with food items variables
by_hhid <- by_hhid[ -c(2:59) ]

#DDS2008total <-
#  summarise (by_hhid,
#             cereals = mean(cereals, na.rm = TRUE),
#             rootsandtubers = mean(rootsandtubers, na.rm = TRUE),
#             vegetables = mean(vegetables, na.rm=TRUE),
#             pulsesandnuts = mean(pulsesandnuts, na.rm=TRUE),
#             fruits = mean(fruits, na.rm=TRUE),
#             meat = mean(meat, na.rm=TRUE),
#             eggs = mean(eggs, na.rm=TRUE),
#             fishandseafood= mean(fishandseafood, na.rm=TRUE),
#             milkandmilkproducts= mean(milkandmilkproducts, na.rm=TRUE),
#             oilsandfats=mean(oilsandfats, na.rm=TRUE),
#             sugar=mean(sugar, na.rm=TRUE),
#             condiments=mean(condiments, na.rm=TRUE))

#saveRDS(by_hhid, file="Data/Nutrition indicators TZA 2010.Rda")

# descriptives of food group dummy variables and FVS and DDS
descriptive.table(vars = d(cereals, rootsandtubers, vegetables, pulsesandnuts, fruits, meat, eggs, fishandseafood,
                           milkandmilkproducts, oilsandfats, sugar,condiments, DDS, FVS),data= by_hhid, 
                  func.names = c("Mean","St. Deviation", "Min", "Max", "Skew","Valid N"))

# Histograms of nutrition indicators: DDS
hist(by_hhid$DDS, freq = FALSE, ylim = c(0, 0.2), xlab="DDS", ylab="%", main="Freguency of DDS in 2010")

# Histograms of nutrition indicators: FVS
hist(by_hhid$FVS, freq = FALSE, ylim = c(0, 0.1), xlab="FVS", ylab="%", main="Freguency of FVS in 2010")

# calculation of correlation coefficent of DDS and FVS
myvars <- c("DDS", "FVS")
by_hhidsub <- by_hhid[myvars]
cor(by_hhidsub, use="all.obs", method="pearson")
rm(by_hhidsub, myvars)

# Simple Scatterplot of DDS and FVS
plot(by_hhid$DDS, by_hhid$FVS, main="Coherence between DDS and FVS in 2010", 
     xlab="DDS ", ylab="FVS ", pch=19) 

FNS2010 <- subset(by_hhid, select=c(hhid,DDS,FVS))


# ***************************************************************************************************
#Construction of FCS
# ***************************************************************************************************

FCS_data  <- read_dta(file.path(dataPath, "/TZNPS2HH2DTA/HH_SEC_K2.dta")) %>%
                 dplyr::select(hhid=y2_hhid, itemcode, item=hh_k08_2, ndays=hh_k08_3)         


FCS_data$Cereals      <- ifelse(FCS_data$ndays>0 & FCS_data$itemcode=="A", FCS_data$ndays, 0)
FCS_data$Roots_tubers <- ifelse(FCS_data$ndays>0 & FCS_data$itemcode=="B", FCS_data$ndays, 0)
FCS_data$Pulses_nuts  <- ifelse(FCS_data$ndays>0 & FCS_data$itemcode=="C", FCS_data$ndays, 0)
FCS_data$Vegetables   <- ifelse(FCS_data$ndays>0 & FCS_data$itemcode=="D", FCS_data$ndays, 0)
FCS_data$Meat_fish    <- ifelse(FCS_data$ndays>0 & FCS_data$itemcode=="E", FCS_data$ndays, 0)
FCS_data$Fruits       <- ifelse(FCS_data$ndays>0 & FCS_data$itemcode=="F", FCS_data$ndays, 0)
FCS_data$Milk         <- ifelse(FCS_data$ndays>0 & FCS_data$itemcode=="G", FCS_data$ndays, 0)
FCS_data$Fats_oils    <- ifelse(FCS_data$ndays>0 & FCS_data$itemcode=="H", FCS_data$ndays, 0)
FCS_data$Sugars       <- ifelse(FCS_data$ndays>0 & FCS_data$itemcode=="I", FCS_data$ndays, 0)
FCS_data$Condiments   <- ifelse(FCS_data$ndays>0 & FCS_data$itemcode=="J", FCS_data$ndays, 0)

FCS2010 <- group_by(FCS_data, hhid) %>%
  summarize(Cereals      = sum(Cereals), 
            Roots_tubers = sum(Roots_tubers),
            Pulses_nuts  = sum(Pulses_nuts),
            Vegetables   = sum(Vegetables),
            Meat_fish    = sum(Meat_fish),
            Fruits       = sum(Fruits),
            Milk         = sum(Milk),
            Fats_oils    = sum(Fats_oils),
            Sugars       = sum(Sugars),
            Condiments   = sum(Condiments)  ) 

FCS2010$FCSw <- 
  FCS2010$Cereals*2 + 
  FCS2010$Roots_tubers*2 + 
  FCS2010$Pulses_nuts*3 + 
  FCS2010$Vegetables*1 + 
  FCS2010$Fruits*1 + 
  FCS2010$Milk*4 + 
  FCS2010$Meat_fish*4 + 
  FCS2010$Fats_oils*1 +
  FCS2010$Sugars*0.5 + 
  FCS2010$Condiments*0

FCS2010$FCSu <- 
  FCS2010$Cereals + 
  FCS2010$Roots_tubers + 
  FCS2010$Pulses_nuts + 
  FCS2010$Vegetables + 
  FCS2010$Fruits + 
  FCS2010$Milk + 
  FCS2010$Meat_fish + 
  FCS2010$Fats_oils +
  FCS2010$Sugars + 
  FCS2010$Condiments

#  RM(Food2010,NUTR2010)

# descriptives of food groups (FCS) and FCS weighted and unweighted
descriptive.table(vars = d(Cereals, Roots_tubers, Pulses_nuts, Vegetables, Fruits, Meat_fish,
                           Milk, Fats_oils, Sugars, Condiments, FCSw, FCSu),data= FCS2010, 
                  func.names = c("Mean","St. Deviation", "Min", "Max", "Skew","Valid N"))

#Maximum of Meat_fish is 9!

# Histograms of nutrition indicators: weighted FCS
hist(FCS2010$FCSw, freq = FALSE, ylim = c(0, 0.02), xlab="FCSw", ylab="%", main="Frequency of FCS (weighted) in 2010")

# Histograms of nutrition indicators: unweighted FCS
hist(FCS2010$FCSu, freq = FALSE, ylim = c(0, 0.05), xlab="FCSw", ylab="%", main="Frequency of FCS (unweighted) in 2010")

# calculation of correlation coefficent of DDS and FVS
myvars <- c("FCSw", "FCSu")
FCS2010sub <- FCS2010[myvars]
cor(FCS2010sub, method="pearson")
rm(FCS2010sub, myvars)

myvars <- c("hhid", "FCSw", "FCSu")
FCS2010 <- FCS2010[myvars]

FNS2010 <- left_join(FNS2010, FCS2010)


# ***************************************************************************************************
#Construction of CSI
# ***************************************************************************************************
CSI2010 <- read_dta(file.path(dataPath, "TZNPS2HH1DTA/HH_SEC_I1.dta")) %>%
  dplyr::select(hhid=y2_hhid, hh_i01, hh_i02_1, hh_i02_2, hh_i02_3, hh_i02_4, hh_i02_5, hh_i02_6, 
                hh_i02_7, hh_i02_8)
#*FOOD2010 <- read_dta(file.path(dataPath, "/TZNPS2HH3DTA/HH_SEC_K1.dta"))

#D:\Analyses\CIMMYT\NutritionTZA\SurveyData\2010\Data\TZNPS2HH1DTA
#D:\Analyses\CIMMYT\NutritionTZA\SurveyData\2010\Data\TZNPS2HH1DTA

descriptive.table(vars = d(hh_i02_1, hh_i02_2, hh_i02_3, hh_i02_4, hh_i02_5, hh_i02_6, 
                           hh_i02_7, hh_i02_8),data= CSI2010, 
                  func.names = c("Mean","St. Deviation", "Min", "Max", "Skew","Valid N"))

#CSI2010 <- 
#  subset(HH_SEC_I1_2010, select=c(hhid, hh_i01, hh_i02_1, hh_i02_2, hh_i02_3, hh_i02_4, hh_i02_5, hh_i02_6, 
#                                   hh_i02_7, hh_i02_8)) %>% na.omit()

CSI2010$CSI <- CSI2010$hh_i02_1*1 + 
  CSI2010$hh_i02_2*1 + 
  CSI2010$hh_i02_3*1 + 
  CSI2010$hh_i02_4*1 + 
  CSI2010$hh_i02_5*3 + 
  CSI2010$hh_i02_6*2 + 
  CSI2010$hh_i02_7*0 + 
  CSI2010$hh_i02_8*4

descriptive.table(vars = d(hh_i02_1, hh_i02_2, hh_i02_3, hh_i02_4, hh_i02_5, hh_i02_6, 
                           hh_i02_7, hh_i02_8, CSI),data= CSI2010, 
                  func.names = c("Mean","St. Deviation", "Min", "Max", "Skew","Valid N"))

myvars <- c("hhid", "CSI")
CSI2010<- CSI2010[myvars]
rm(myvars)
#CSI2010<- mutate(CSI2010, surveyyear=2010) %>% rename(hhid2010=y2_hhid)


#saveRDS(CSI2010, "Data/CSI2010.rds")
FNS2010 <- left_join(FNS2010, CSI2010)

