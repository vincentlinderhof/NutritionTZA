# Tom
# dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA/2010/Data"

# LEI Path
# dataPath <- "W:/LEI/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/TZA/2010/Data"

# Vincent at home

if(Sys.info()["user"] == "Tomas"){
  dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA/2012/Data"
} else {
  dataPath <- "D:/Analyses/CIMMYT/NutritionTZA/SurveyData/2012/Data"
}
setwd("D:/Analyses/CIMMYT/NutritionTZA")

# load packages
library("haven")
library("stringr")
library("reshape2")
library("dplyr", lib.loc="C:/Program Files/R/R-3.3.1/library")
library("markdown")

options(scipen=999)

# ***************************************************************************************************
#Creation of DDS and FVS
# ***************************************************************************************************

FOOD2012 <- read_dta(file.path(dataPath, "HH_SEC_J1.dta"))
FOOD2012 <- subset(FOOD2012, select=c(y3_hhid, itemcode, hh_j01))

library(tidyr) # Necessary for spread function in mutate command
# Construct dummy variables for food items
NUTR2012 <-
  mutate(FOOD2012, count = ifelse(hh_j01 == 1, 1, ifelse(NA))) %>%
  group_by(y3_hhid) %>%
  spread(itemcode, count) %>%
  select(-hh_j01 ) %>%
  filter (! duplicated(y3_hhid)) %>%
  replace(is.na(.), 0)

NUTR2012 <- subset(NUTR2012, select= -c(hh_j01))

#dataPath <- "D:/UserData/verbe038/data tza/2012/Data/"
#dataPath 


# Columns correspond to list of food items!
NUTR2012$cereals             <- 1*(rowSums(NUTR2012[2:13]) > 0)
NUTR2012$rootsandtubers      <- 1*(rowSums(NUTR2012[14:20]) > 0)
NUTR2012$vegetables          <- 1*(rowSums(NUTR2012[29:31]) > 0)
NUTR2012$pulsesandnuts       <- 1*(rowSums(NUTR2012[24:28]) > 0)
NUTR2012$fruits              <- 1*(rowSums(NUTR2012[32:34]) > 0)
NUTR2012$meat                <- 1*(rowSums(NUTR2012[36:41]) > 0)
NUTR2012$eggs                <- 1*(rowSums(NUTR2012[42]) > 0)
NUTR2012$fishandseafood      <- 1*(rowSums(NUTR2012[43:45]) > 0)
NUTR2012$milkandmilkproducts <- 1*(rowSums(NUTR2012[46:48]) > 0)
NUTR2012$oilsandfats         <- 1*(rowSums(NUTR2012[49:50]) > 0)
NUTR2012$sugar               <- 1*(rowSums(NUTR2012[21:23])+rowSums(NUTR2012[35]) > 0)
NUTR2012$condiments          <- 1*(rowSums(NUTR2012[51:60]) > 0)
#Alcoholic beverages included in FOOD group 12, Ssee FAO (2013)!
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

NUTR2012 <- mutate(NUTR2012, 
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

NUTR2012$FVS <- rowSums(NUTR2012[2:60])
#rm(FOOD2008, NUTR2008)

by_hhid2012 <- group_by(NUTR2012, NUTR2012$y3_hhid)
by_hhid2012 <- by_hhid2012[ -c(2:59) ]

saveRDS(by_hhid2012, file="Data/Nutrition indicators TZA 2012.Rda")

library(Deducer)
descriptive.table(vars = d(cereals, rootsandtubers, vegetables, pulsesandnuts, fruits, meat, eggs, fishandseafood,
                           milkandmilkproducts, oilsandfats, sugar,condiments, DDS, FVS),data= by_hhid2012, 
                  func.names = c("Mean","St. Deviation", "Min", "Max", "Skew","Valid N"))

# Histograms of nutrition indicators
hist(by_hhid2012$DDS, freq = FALSE, ylim = c(0, 0.2), xlab="DDS", ylab="%", main="Freguency of DDS in 2012")

hist(by_hhid2012$FVS, freq = FALSE, ylim = c(0, 0.1), xlab="FVS", ylab="%", main="Freguency of FVS in 2012")

myvars <- c("DDS", "FVS")
by_hhidsub <- by_hhid2012[myvars]
cor(by_hhidsub, use="all.obs", method="pearson")
rm(by_hhidsub, myvars)


# Simple Scatterplot
plot(by_hhid2012$DDS, by_hhid2012$FVS, main="Coherence between DDS and FVS in 2012", 
     xlab="DDS ", ylab="FVS ", pch=19) 

# ***************************************************************************************************
#Construction of FCS
# ***************************************************************************************************

HH_SEC_J3_2012 <- read_dta(file.path(dataPath, "HH_SEC_J3.dta"))
FCS2012 <- subset(HH_SEC_J3_2012, select=c(y3_hhid, itemcode, hh_j09_3))

FCS2012 <- group_by(HH_SEC_J3_2012, y3_hhid) %>% 
  na.omit() %>%
  #  select(y2_hhid, itemcode, hh_k08_3) %>%
  spread(itemcode, hh_j09_3) %>% 
rename(cereals=1, rootsandtubers=2, pulsesandnuts=3, vegetables=4, meatandfish=5, fruits=6, milkproducts=7, fatsandoils=8, sugar=9, condiments=10)
  #  mutate(AB = A+B) %>%
  #  select(-A, -B) %>%
  #  mutate(AB=replace(AB, AB>=7, 7)) %>%
#  rename(main_staples=A, less_staples=B, pulses_nuts=C, vegetables=D, meat_fish=E, fruits=F, milk=G, oil=H, sugar=I, condiments=J)

FCS2012$cereals        <- 1*(FCS2012$itemcode == 1)*FCS2012$hh_j09_3
FCS2012$rootsandtubers <- 1*(FCS2012$itemcode == 2)*FCS2012$hh_j09_3
FCS2012$pulsesandnuts  <- 1*(FCS2012$itemcode == 3)*FCS2012$hh_j09_3
FCS2012$vegetables     <- 1*(FCS2012$itemcode == 4)*FCS2012$hh_j09_3
FCS2012$meatandfish    <- 1*(FCS2012$itemcode == 5)*FCS2012$hh_j09_3
FCS2012$fruits         <- 1*(FCS2012$itemcode == 6)*FCS2012$hh_j09_3
FCS2012$milkproducts   <- 1*(FCS2012$itemcode == 7)*FCS2012$hh_j09_3
FCS2012$fatsandoils    <- 1*(FCS2012$itemcode == 8)*FCS2012$hh_j09_3
FCS2012$sugar          <- 1*(FCS2012$itemcode == 9)*FCS2012$hh_j09_3
FCS2012$condiments     <- 1*(FCS2012$itemcode == 10)*FCS2012$hh_j09_3

#NUTR2012$cereals             <- 1*(rowSums(NUTR2012[2:13]) > 0)
#NUTR2012$rootsandtubers      <- 1*(rowSums(NUTR2012[14:20]) > 0)
#NUTR2012$vegetables          <- 1*(rowSums(NUTR2012[29:31]) > 0)
#NUTR2012$pulsesandnuts       <- 1*(rowSums(NUTR2012[24:28]) > 0)
#NUTR2012$fruits              <- 1*(rowSums(NUTR2012[32:34]) > 0)
#NUTR2012$meat                <- 1*(rowSums(NUTR2012[36:41]) > 0)
#NUTR2012$eggs                <- 1*(rowSums(NUTR2012[42]) > 0)
#NUTR2012$fishandseafood      <- 1*(rowSums(NUTR2012[43:45]) > 0)
#NUTR2012$milkandmilkproducts <- 1*(rowSums(NUTR2012[46:48]) > 0)
#NUTR2012$oilsandfats         <- 1*(rowSums(NUTR2012[49:50]) > 0)
#NUTR2012$sugar               <- 1*(rowSums(NUTR2012[21:23])+rowSums(NUTR2012[35]) > 0)
#NUTR2012$condiments          <- 1*(rowSums(NUTR2012[51:60]) > 0)

#summary(FCS2012$itemcode)  
# remove hh_j09_3 and itemcode because these variables cannot be aggregated
FCS2012 <- FCS2012[ -c(2, 3) ]

FCS2012 <- aggregate(FCS2012, by=list(FCS2012$y3_hhid), FUN=max )

FCS2012$FCSw <- 
  FCS2012$cereals*2 + 
  FCS2012$rootsandtubers*2 + 
  FCS2012$pulsesandnuts*3 + 
  FCS2012$vegetables*1 + 
  FCS2012$fruits*1 + 
  FCS2012$milkproducts*4 + 
  FCS2012$meatandfish*4 + 
  FCS2012$sugar*0.5 + 
  FCS2012$condiments*0

FCS2012$FCSu <- 
  FCS2012$cereals + 
  FCS2012$rootsandtubers + 
  FCS2012$pulsesandnuts + 
  FCS2012$vegetables + 
  FCS2012$fruits + 
  FCS2012$milkproducts + 
  FCS2012$meatandfish + 
  FCS2012$sugar + 
  FCS2012$condiments

#  RM(Food2010,NUTR2010)

descriptive.table(vars = d(cereals, rootsandtubers, vegetables, pulsesandnuts, fruits, meatandfish,
                           milkproducts, fatsandoils, sugar,condiments, FCSw, FCSu),data= FCS2012, 
                  func.names = c("Mean","St. Deviation", "Min", "Max", "Skew","Valid N"))

# Histograms of nutrition indicators
hist(FCS2012$FCSw, freq = FALSE, ylim = c(0, 0.02), xlab="FCSw", ylab="%", main="Frequency of FCS (weighted) in 2012")

hist(FCS2012$FCSu, freq = FALSE, ylim = c(0, 0.05), xlab="FCSu", ylab="%", main="Frequency of FCS (unweighted) in 2012")

myvars <- c("FCSw", "FCSu")
FCS2012sub <- FCS2012[myvars]
cor(FCS2012sub, use="all.obs", method="pearson")
rm(FCS2012sub, myvars)


# ***************************************************************************************************
#Construction of CSI
# ***************************************************************************************************

HH_SEC_H_2012<- read_dta(file.path(dataPath, "HH_SEC_H.dta"))
CSI2012 <- subset(HH_SEC_H_2012, select=c(y3_hhid, hh_h01, hh_h02_1, hh_h02_2, hh_h02_3, hh_h02_4, hh_h02_5, hh_h02_6, hh_h02_7, hh_h02_8))

descriptive.table(vars = d(hh_h02_1, hh_h02_2, hh_h02_3, hh_h02_4, hh_h02_5, hh_h02_6, 
                           hh_h02_7, hh_h02_8),data= HH_SEC_H_2012, 
                  func.names = c("Mean","St. Deviation", "Min", "Max", "Skew","Valid N"))

CSI2012$CSI <- 
  CSI2012$hh_h02_1*1 + 
  CSI2012$hh_h02_2*1 + 
  CSI2012$hh_h02_3*1 + 
  CSI2012$hh_h02_4*1 + 
  CSI2012$hh_h02_5*3 + 
  CSI2012$hh_h02_6*2 + 
  CSI2012$hh_h02_7*0 + 
  CSI2012$hh_h02_8*4

descriptive.table(vars = d(hh_h02_1, hh_h02_2, hh_h02_3, hh_h02_4, hh_h02_5, hh_h02_6, 
                           hh_h02_7, hh_h02_8, CSI),data= CSI2012, 
                  func.names = c("Mean","St. Deviation", "Min", "Max", "Skew","Valid N"))

CSI2012<- select(CSI2012, y3_hhid, CSI)
CSI2012<- mutate(CSI2012, surveyyear=2012) %>% rename(hhid2012=y3_hhid)

  
  