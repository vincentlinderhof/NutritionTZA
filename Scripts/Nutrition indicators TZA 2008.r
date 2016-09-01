# Tom
# dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA/2008/Data"

# LEI Path
# dataPath <- "W:/LEI/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/TZA/2008/Data"

# Vincent at home

if(Sys.info()["user"] == "Tomas"){
  dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA/2008/Data"
} else {
  dataPath <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/TZA/2008/Data"
}

# load packages
library("haven")
library("stringr")
library("reshape2")
library("dplyr")
library("markdown")

options(scipen=999)

setwd("D:/Models/CIMMYT/TNZ")

#Creation of DDS and FVS

FOOD2008 <- read_dta(file.path(dataPath, "TZNPS1HHDTA_E/SEC_K1.dta"))
FOOD2008 <- subset(FOOD2008, select=c(hhid, skcode, skq1))


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
NUTR2008 <-
  mutate(FOOD2008, count = ifelse(skq1 == 1, 1, ifelse(NA))) %>%
  group_by(hhid) %>%
  spread(skcode, count) %>%
  filter (! duplicated(hhid)) %>%
  replace(is.na(.), 0)

# Remove skq1 and skcode variables which are useless at household level
NUTR2008 <-NUTR2008[-c(2,3)]

# Columns correspond to list of food items!
NUTR2008$cereals             <- 1*(rowSums(NUTR2008[2:13]) > 0)
NUTR2008$rootsandtubers      <- 1*(rowSums(NUTR2008[14:20]) > 0)
NUTR2008$vegetables          <- 1*(rowSums(NUTR2008[29:31]) > 0)
NUTR2008$pulsesandnuts       <- 1*(rowSums(NUTR2008[24:28]) > 0)
NUTR2008$fruits              <- 1*(rowSums(NUTR2008[32:34]) > 0)
NUTR2008$meat                <- 1*(rowSums(NUTR2008[36:41]) > 0)
NUTR2008$eggs                <- 1*(rowSums(NUTR2008[42]) > 0)
NUTR2008$fishandseafood      <- 1*(rowSums(NUTR2008[43:45]) > 0)
NUTR2008$milkandmilkproducts <- 1*(rowSums(NUTR2008[46:48]) > 0)
NUTR2008$oilsandfats         <- 1*(rowSums(NUTR2008[49:50]) > 0)
NUTR2008$sugar               <- 1*(rowSums(NUTR2008[21:23])+rowSums(NUTR2008[35]) > 0)
NUTR2008$condiments          <- 1*(rowSums(NUTR2008[51:60]) > 0)
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

NUTR2008 <- mutate(NUTR2008, 
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

NUTR2008$FVS <- rowSums(NUTR2008[2:60])
#rm(FOOD2008, NUTR2008)

by_hhid <- group_by(NUTR2008, hhid)
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

saveRDS(by_hhid, file="Data/Nutrition indicators TZA 2008.Rda")


descriptive.table(vars = d(cereals, rootsandtubers, vegetables, pulsesandnuts, fruits, meat, eggs, fishandseafood,
                           milkandmilkproducts, oilsandfats, sugar,condiments, DDS, FVS),data= by_hhid, 
                  func.names = c("Mean","St. Deviation", "Min", "Max", "Valid N"))

# Histograms of nutrition indicators
hist(by_hhid$DDS, freq = FALSE, ylim = c(0, 0.2), xlab="DDS", ylab="%", main="Freguency of DDS in 2008")

hist(by_hhid$FVS, freq = FALSE, ylim = c(0, 0.1), xlab="FVS", ylab="%", main="Freguency of FVS in 2008")

myvars <- c("DDS", "FVS")
by_hhidsub <- by_hhid[myvars]
cor(by_hhidsub, use="all.obs", method="pearson")
rm(by_hhidsub, myvars)


# Simple Scatterplot
plot(by_hhid$DDS, by_hhid$FVS, main="Coherence between DDS and FVS in 2008", 
     xlab="DDS ", ylab="FVS ", pch=19) 

