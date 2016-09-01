# This script produces the modified household dietary diversity score as in :
#"Farm production diversity is associated with greater household dietary diversity in Malawi: Findings from nationally representative data" by Jones et al. (2014)

# for the wave of 2008

SEC_K1 <- read.dta("D:/UserData/verbe038/data tza/2008/Data/SEC_K1.dta")
DDS2008 <- select(SEC_K1, hhid, skcode, skq1)

DDS2008  <-
  mutate(DDS2008, cereals = ifelse(skcode == 'Rice (paddy)'  & skq1 == 'Yes'
                              | skcode == 'Rice (husked)'  & skq1 == 'Yes'
                              | skcode == 'Maize (green, cob)'  & skq1 == 'Yes'
                              | skcode =='Maize (grain)'  & skq1 == 'Yes'
                              | skcode == 'Maize (flour)'  & skq1 == 'Yes'
                              | skcode == 'Millet and sorghum (grain)'  & skq1 == 'Yes'
                              | skcode == 'Millet and sorghum (flour)'  & skq1 == 'Yes'
                              | skcode == 'Wheat, barley grain and other cereals'  & skq1 == 'Yes'
                              | skcode == 'Bread'  & skq1 == 'Yes'
                              | skcode == 'Buns, cakes and biscuts'  & skq1 == 'Yes'
                              | skcode == 'Macaroini, spaghetti'  & skq1 == 'Yes'
                              | skcode == 'Other cereal products'  & skq1 == 'Yes', 1,
                              ifelse (NA))) %>%
  mutate(rootsandtubers = ifelse(skcode == 'Cassava fresh' & skq1 == 'Yes'
                              | skcode == 'Sweet potatoes' & skq1 == 'Yes'
                              | skcode =='Yams/cocoyams' & skq1 == 'Yes'
                              | skcode == 'Irish potatoes' & skq1 == 'Yes'
                              | skcode == 'Cooking bananas, plantains' & skq1 == 'Yes'
                              | skcode == 'Other starches' & skq1 == 'Yes', 1, 
                              ifelse (NA))) %>%
  mutate(vegetables = ifelse(skcode == 'Onions, tomatoes, carrots and green pepper, other viungo' & skq1 == 'Yes'
                              | skcode == 'Spinach, cabbage and other green vegetables' & skq1 == 'Yes'
                              | skcode == 'Canned, dried and wild vegetables' & skq1 == 'Yes', 1, 
                              ifelse (NA))) %>%
  mutate(pulsesandnuts = ifelse(skcode == 'Peas, beans, lentils and other pulses' & skq1 == 'Yes'
                              | skcode == 'Groundnuts in shell/shelled' & skq1 == 'Yes'
                              | skcode == 'Coconuts (mature/immature)' & skq1 == 'Yes'
                              | skcode == 'Cashew, almonds and other nuts' & skq1 == 'Yes'
                              | skcode == 'Seeds and products from nuts/seeds (excl. cooking oil)' & skcode == 'Yes', 1, 
                              ifelse (NA))) %>%
  mutate(fruits = ifelse(skcode == 'Ripe bananas' & skq1 == 'Yes'
                              | skcode == 'Citrus fruits (oranges, lemon, tangarines, etc.)' & skq1 == 'Yes'
                              | skcode == 'Mangoes, avocadoes and other fruits' & skq1 == 'Yes'
                              , 1, 
                             ifelse (NA))) %>%
  mutate(meat  = ifelse(skcode == 'Beef including minced sausage' & skq1 == 'Yes'
                              | skcode == 'Pork including sauages and bacon' & skq1 == 'Yes'
                              | skcode == 'Chicken and other poultry' & skq1 == 'Yes'
                              | skcode == 'Wild birds and insects' & skq1 == 'Yes'
                              | skcode == 'Goat meat' & skq1 == 'Yes'
                              | skcode == 'Other domestic/wild meat products' & skq1 == 'Yes'
                              , 1, 
                              ifelse (NA))) %>%
  mutate(eggs = ifelse(skcode == 'Eggs' & skq1 == 'Yes'
                              , 1,
                              ifelse (NA))) %>%
  mutate(fishandseafood = ifelse(skcode == 'Fresh fish and seafood (including dagaa)' & skq1 == 'Yes'
                              | skcode == 'Dried/salted/canned fish and seafood (incl. dagaa)' & skq1 == 'Yes'
                              | skcode == 'Package fish' & skq1 == 'Yes'
                              , 1, 
                              ifelse (NA))) %>%
  mutate(milkandmilkproducts = ifelse(skcode == 'Fresh milk' & skq1 == 'Yes'
                              | skcode == 'Milk products (like cream, cheese, yoghurt etc)' & skq1 == 'Yes'
                              | skcode == 'Canned milk/milk powder' & skq1 == 'Yes'
                              , 1, 
                              ifelse (NA))) %>% 
  mutate(oilsandfats = ifelse(skcode == 'Cooking oil' & skq1 == 'Yes'
                              | skcode == 'Butter, margarine, ghee and other fat products' & skq1 == 'Yes'
                              , 1, 
                              ifelse (NA))) %>%
  mutate(sugar  = ifelse(skcode == 'Sugar' & skq1 == 'Yes'
                              | skcode == 'Sugarcane' & skq1 == 'Yes'
                              | skcode == 'Sweets' & skq1 == 'Yes'
                              | skcode == 'Honey, syrups, jams, marmalade, jellies, canned fruits' & skq1 == 'Yes'
                              , 1, 
                         ifelse (NA))) %>%
  mutate(condiments  = ifelse(skcode == 'Other spices' & skq1 == 'Yes'
                              | skcode == 'Tea dry' & skq1 == 'Yes'
                              | skcode == 'Coffee and cocoa' & skq1 == 'Yes'
                              | skcode == 'Other raw materals for drinks' & skq1 == 'Yes'
                              | skcode == 'Bottled/canned soft drinks (soda, juice, water)' & skq1 == 'Yes'
                              | skcode == 'Prepared tea/coffee'
                              | skcode == 'Salt' & skq1 == 'Yes'
                              , 1, 
                              ifelse (NA)))

by_hhid <- group_by(DDS2008, hhid)
DDS2008total <-
  summarise (by_hhid,
             cereals = mean(cereals, na.rm = TRUE),
             rootsandtubers = mean(rootsandtubers, na.rm = TRUE),
             vegetables = mean(vegetables, na.rm=TRUE),
             pulsesandnuts = mean(pulsesandnuts, na.rm=TRUE),
             fruits = mean(fruits, na.rm=TRUE),
             meat = mean(meat, na.rm=TRUE),
             eggs = mean(eggs, na.rm=TRUE),
             fishandseafood= mean(fishandseafood, na.rm=TRUE),
             milkandmilkproducts= mean(milkandmilkproducts, na.rm=TRUE),
             oilsandfats=mean(oilsandfats, na.rm=TRUE),
             sugar=mean(sugar, na.rm=TRUE),
             condiments=mean(condiments, na.rm=TRUE))

DDS2008total[is.na(DDS2008total)] <- 0

DDS2008total$dietary_diversity_score <- 
  DDS2008total$cereals + 
  DDS2008total$rootsandtubers + 
  DDS2008total$vegetables + 
  DDS2008total$pulsesandnuts + 
  DDS2008total$fruits + 
  DDS2008total$meat + 
  DDS2008total$eggs + 
  DDS2008total$fishandseafood + 
  DDS2008total$milkandmilkproducts +
  DDS2008total$oilsandfats + 
  DDS2008total$sugar + 
  DDS2008total$condiments

DDS2008total<- select(DDS2008total, hhid, dietary_diversity_score)
DDS2008total<-mutate(DDS2008total, surveyyear=2008) %>% rename(hhid2008=hhid)

dataPath <- "D:/UserData/verbe038/TZA/2008/Data"
saveRDS(DDS2008total, file.path(dataPath, "DDS2008total.rds"))

rm(DDS2008total, DDS2008, SEC_K1)

# for the wave of 2010


HH_SEC_K1 <- read.dta("D:/UserData/verbe038/data tza/2010/Data/HH_SEC_K1.dta")
DDS2010 <- select(HH_SEC_K1, y2_hhid, itemcode, hh_k01_2)

DDS2010 <- 
  mutate(DDS2010, cereals = ifelse(itemcode == 'Rice (paddy)' & hh_k01_2 == 'Yes'
                              | itemcode == 'Rice (husked)'& hh_k01_2 == 'Yes'
                              | itemcode == 'Maize (green, cob)' & hh_k01_2 == 'Yes'
                              | itemcode == 'Maize (grain)' & hh_k01_2 == 'Yes'
                              | itemcode == 'Maize (flour)' & hh_k01_2 == 'Yes'
                              | itemcode == 'Millet and sorghum (grain)'& hh_k01_2 == 'Yes'
                              | itemcode == 'Millet and sorghum (flour)' & hh_k01_2 == 'Yes'
                              | itemcode == 'Wheat, barley grain and other cereals' & hh_k01_2 == 'Yes'
                              | itemcode == 'Bread' & hh_k01_2 == 'Yes'
                              | itemcode == 'Buns, cakes and biscuts' & hh_k01_2 == 'Yes'
                              | itemcode == 'Macaroini, spaghetti' & hh_k01_2 == 'Yes'
                              | itemcode == 'Other cereal products' & hh_k01_2 == 'Yes', 1, 
                              ifelse(NA))) %>%
  mutate(rootsandtubers = ifelse(itemcode == 'Cassava fresh' & hh_k01_2 == 'Yes'
                              | itemcode == 'Sweet potatoes' & hh_k01_2 == 'Yes'
                              | itemcode =='Yams/cocoyams' & hh_k01_2 == 'Yes'
                              | itemcode == 'Irish potatoes' & hh_k01_2 == 'Yes'
                              | itemcode == 'Cooking bananas, plantains' & hh_k01_2 == 'Yes'
                              | itemcode == 'Other starches' & hh_k01_2 == 'Yes', 1, 
                              ifelse (NA))) %>%
  mutate(vegetables = ifelse(itemcode == 'Onions, tomatoes, carrots and green pepper, other viungo' & hh_k01_2 == 'Yes'
                              | itemcode == 'Spinach, cabbage and other green vegetables' & hh_k01_2 == 'Yes'
                              | itemcode == 'Canned, dried and wild vegetables' & hh_k01_2 == 'Yes', 1, 
                              ifelse (NA))) %>%
  mutate(pulsesandnuts = ifelse(itemcode == 'Peas, beans, lentils and other pulses' & hh_k01_2 == 'Yes'
                              | itemcode == 'Groundnuts in shell/shelled' & hh_k01_2 == 'Yes'
                              | itemcode == 'Cashew, almonds and other nuts' & hh_k01_2 == 'Yes'
                              | itemcode == 'Seeds and products from nuts/seeds (excl. cooking oil)' & hh_k01_2 == 'Yes', 1, 
                              ifelse (NA))) %>%
  mutate(fruits = ifelse(itemcode == 'Ripe bananas' & hh_k01_2 == 'Yes'
                              | itemcode == 'Citrus fruits (oranges, lemon, tangarines, etc.)' & hh_k01_2 == 'Yes'
                              | itemcode == 'Coconuts (mature/immature)' & hh_k01_2 == 'Yes'
                              | itemcode == 'Mangoes, avocadoes and other fruits' & hh_k01_2 == 'Yes'
                              , 1, 
                              ifelse (NA))) %>%
  mutate(meat  = ifelse(itemcode == 'Beef including minced sausage' & hh_k01_2 == 'Yes'
                              | itemcode == 'Pork including sauages and bacon' & hh_k01_2 == 'Yes'
                              | itemcode == 'Chicken and other poultry' & hh_k01_2 == 'Yes'
                              | itemcode == 'Wild birds and insects' & hh_k01_2 == 'Yes'
                              | itemcode == 'Goat meat' & hh_k01_2 == 'Yes'
                              | itemcode == 'Other domestic/wild meat products' & hh_k01_2 == 'Yes'
                              , 1, 
                              ifelse (NA))) %>%
  mutate(eggs = ifelse(itemcode == 'Eggs' & hh_k01_2 == 'Yes'
                              , 1,
                              ifelse (NA))) %>%
  mutate(fishandseafood = ifelse(itemcode == 'Fresh fish and seafood (including dagaa)' & hh_k01_2 == 'Yes'
                              | itemcode == 'Dried/salted/canned fish and seafood (incl. dagaa)' & hh_k01_2 == 'Yes'
                              | itemcode == 'Package fish' & hh_k01_2 == 'Yes'
                              , 1, 
                              ifelse (NA))) %>%
  mutate(milkandmilkproducts = ifelse(itemcode == 'Fresh milk' & hh_k01_2 == 'Yes'
                              | itemcode == 'Milk products (like cream, cheese, yoghurt etc)' & hh_k01_2 == 'Yes'
                              | itemcode == 'Canned milk/milk powder' & hh_k01_2 == 'Yes'
                              , 1, 
                              ifelse (NA))) %>%
  mutate(oilsandfats = ifelse(itemcode == 'Cooking oil' & hh_k01_2 == 'Yes'
                              | itemcode == 'Butter, margarine, ghee and other fat products' & hh_k01_2 == 'Yes'
                              , 1, 
                              ifelse (NA))) %>%
  mutate(sugar  = ifelse(itemcode == 'Sugar' & hh_k01_2 == 'Yes'
                              | itemcode == 'Sugarcane' & hh_k01_2 == 'Yes'
                              | itemcode == 'Sweets' & hh_k01_2 == 'Yes'
                              | itemcode == 'Honey, syrups, jams, marmalade, jellies, canned fruits' & hh_k01_2 == 'Yes'
                              , 1, 
                         ifelse (NA))) %>%
  mutate(condiments  = ifelse(itemcode == 'Other spices' & hh_k01_2 == 'Yes'
                              | itemcode == 'Tea dry' & hh_k01_2 == 'Yes'
                              | itemcode == 'Coffee and cocoa' & hh_k01_2 == 'Yes'
                              | itemcode == 'Other raw materals for drinks' & hh_k01_2 == 'Yes'
                              | itemcode == 'Bottled/canned soft drinks (soda, juice, water)' & hh_k01_2 == 'Yes'
                              | itemcode == 'Prepared tea, coffee' & hh_k01_2 == 'Yes'
                              | itemcode == 'Salt' & hh_k01_2 == 'Yes'
                              , 1, 
                              ifelse (NA)))

by_hhid <- group_by(DDS2010, y2_hhid)
DDS2010total <- summarise (by_hhid, 
                           cereals = mean(cereals, na.rm = TRUE), 
                           rootsandtubers = mean(rootsandtubers, na.rm = TRUE), 
                           vegetables = mean(vegetables, na.rm=TRUE), 
                           pulsesandnuts = mean(pulsesandnuts, na.rm=TRUE), 
                           fruits = mean(fruits, na.rm=TRUE), 
                           meat = mean(meat, na.rm=TRUE), eggs = 
                             mean(eggs, na.rm=TRUE), 
                           fishandseafood= mean(fishandseafood, na.rm=TRUE), 
                           milkandmilkproducts= mean(milkandmilkproducts, na.rm=TRUE), 
                           oilsandfats=mean(oilsandfats, na.rm=TRUE), 
                           sugar=mean(sugar, na.rm=TRUE), 
                           condiments=mean(condiments, na.rm=TRUE))

DDS2010total[is.na(DDS2010total)] <- 0

DDS2010total$dietary_diversity_score <- 
  DDS2010total$cereals + 
  DDS2010total$rootsandtubers + 
  DDS2010total$vegetables + 
  DDS2010total$pulsesandnuts + 
  DDS2010total$fruits + 
  DDS2010total$meat + 
  DDS2010total$eggs + 
  DDS2010total$fishandseafood + 
  DDS2010total$oilsandfats + 
  DDS2010total$sugar + 
  DDS2010total$condiments

DDS2010total<- select(DDS2010total, y2_hhid, dietary_diversity_score)
DDS2010total<-mutate(DDS2010total, surveyyear=2010) %>% rename(hhid2010=y2_hhid)

dataPath <- "D:/UserData/verbe038/TZA/2010/Data"
saveRDS(DDS2010total, file.path(dataPath, "DDS2010total.rds"))

rm(DDS2010total, DDS2010, HH_SEC_K1)

# for the wave of 2012

#dataPath <- "D:/UserData/verbe038/data tza/2012/Data/"
#dataPath 

HH_SEC_J1 <- read.dta("D:/UserData/verbe038/data tza/2012/Data/HH_SEC_J1.dta")
DDS2012 <- HH_SEC_J1 %>%
  select(y3_hhid, itemcode, hh_j01) %>%
  mutate(cereals = ifelse(itemcode == 'RICE (PADDY)' & hh_j01 =='YES'
                               | itemcode == 'RICE (HUSKED)'& hh_j01 =='YES'
                               | itemcode == 'MAIZE (GREEN, COB)' & hh_j01 =='YES'
                               | itemcode == 'MAIZE (GRAIN)' & hh_j01 =='YES'
                               | itemcode == 'MAIZE (FLOUR)' & hh_j01 =='YES'
                               | itemcode == 'MILLET AND SORGHUM (GRAIN)' & hh_j01 =='YES'
                               | itemcode == 'MILLET AND SORGHUM (FLOUR)' & hh_j01 =='YES'
                               | itemcode == 'WHEAT, BARLEY GRAIN AND OTHER CEREALS' & hh_j01 =='YES'
                               | itemcode == 'BREAD' & hh_j01 =='YES'
                               | itemcode == 'BUNS, CAKES AND BISCUITS' & hh_j01 =='YES'
                               | itemcode == 'MACARONI, SPAGHETTI' & hh_j01 =='YES'
                               | itemcode == 'OTHER CEREAL PRODUCTS' & hh_j01 =='YES', 1, 
                               ifelse(NA))) %>%
       mutate(rootsandtubers = ifelse(itemcode == 'CASSAVA FRESH' & hh_j01 == 'YES'
                               | itemcode == 'CASSAVA DRY/FLOUR' & hh_j01 == 'YES'
                               | itemcode =='SWEET POTATOES' & hh_j01 == 'YES'
                               | itemcode == 'YAMS/COCOYAMS' & hh_j01 == 'YES'
                               | itemcode == 'COOKING BANANAS, PLANTAINS' & hh_j01 == 'YES' 
                               | itemcode == 'IRISH POTATOES' & hh_j01 == 'YES'
                               | itemcode == 'OTHER STARCHES' & hh_j01 == 'YES', 1, 
                                      ifelse (NA))) %>%
        mutate(vegetables = ifelse(itemcode == 'ONIONS, TOMATOES, CARROTS AND GREEN PEPPER, OTHER VIUNGO' & hh_j01 == 'YES'
                               | itemcode == 'SPINACH, CABBAGE AND OTHER GREEN VEGETABLES' & hh_j01 == 'YES'
                               | itemcode == 'CANNED, DRIED AND WILD VEGETABLES' & hh_j01 == 'YES', 1, 
                                  ifelse (NA))) %>%
        mutate(pulsesandnuts = ifelse(itemcode == 'PEAS, BEANS, LENTILS AND OTHER PULSES' & hh_j01 == 'YES'
                               | itemcode == 'GROUNDNUTS IN SHELL/SHELLED' & hh_j01 == 'YES'
                               | itemcode == 'COCONUTS (MATURE/IMMATURE)' & hh_j01 == 'YES'
                               | itemcode == 'CASHEW, ALMONDS AND OTHER NUTS' & hh_j01 == 'YES'
                               | itemcode == 'SEEDS AND PRODUCTS FROM NUTS/SEEDS (EXCL. COOKING OIL)' & hh_j01 == 'YES', 1, 
                                     ifelse (NA))) %>%
        mutate(fruits = ifelse(itemcode == 'RIPE BANANAS' & hh_j01 == 'YES'
                               | itemcode == 'CITRUS FRUITS (ORANGES, LEMON, TANGERINES, ETC.)' & hh_j01 == 'YES'
                               | itemcode == 'MANGOES, AVOCADOES AND OTHER FRUITS' & hh_j01 == 'YES'
                               , 1, 
                               ifelse (NA))) %>%
        mutate(meat  = ifelse(itemcode == 'BEEF INCLUDING MINCED SAUSAGE' & hh_j01 == 'YES'
                               | itemcode == 'PORK INCLUDING SAUSAGES AND BACON' & hh_j01 == 'YES'
                               | itemcode == 'CHICKEN AND OTHER POULTRY' & hh_j01 == 'YES'
                               | itemcode == 'WILD BIRDS AND INSECTS' & hh_j01 == 'YES'
                               | itemcode == 'GOAT MEAT' & hh_j01 == 'YES'
                               | itemcode == 'OTHER DOMESTIC/WILD MEAT PRODUCTS' & hh_j01 == 'YES'
                               , 1, 
                               ifelse (NA))) %>%
        mutate(eggs = ifelse(itemcode == 'EGGS' & hh_j01 == 'YES'
                            , 1,
                            ifelse (NA))) %>%
        mutate(fishandseafood = ifelse(itemcode == 'FRESH FISH AND SEAFOOD (INCLUDING DAGAA)' & hh_j01 == 'YES'
                               | itemcode == 'DRIED/SALTED/CANNED FISH AND SEAFOOD (INCL. DAGAA)' & hh_j01 == 'YES'
                               | itemcode == 'PACKAGE FISH' & hh_j01 == 'YES'
                               , 1, 
                              ifelse (NA))) %>%
        mutate(milkandmilkproducts = ifelse(itemcode == 'FRESH MILK' & hh_j01 == 'YES'
                               | itemcode == 'MILK PRODUCTS (LIKE CREAM, CHEESE, YOGHURT ETC)' & hh_j01 == 'YES'
                               | itemcode == 'CANNED MILK/MILK POWDER' & hh_j01 == 'YES'
                               , 1, 
                               ifelse (NA))) %>%
        mutate(oilsandfats = ifelse(itemcode == 'COOKING OIL' & hh_j01 == 'YES'
                               | itemcode == 'BUTTER, MARGARINE, GHEE AND OTHER FAT PRODUCTS' & hh_j01 == 'YES'
                               , 1, 
                               ifelse (NA))) %>%
        mutate(sugar  = ifelse(itemcode == 'SUGAR' & hh_j01 == 'YES'
                               | itemcode == 'SUGARCANE' & hh_j01 == 'YES'
                               | itemcode == 'SWEETS' & hh_j01 == 'YES'
                               | itemcode == 'HONEY, SYRUPS, JAMS, MARMALADE, JELLIES, CANNED FRUITS' & hh_j01 == 'YES'
                               , 1, 
                               ifelse (NA))) %>%
        mutate(condiments  = ifelse(itemcode == 'OTHER SPICES' & hh_j01 == 'YES'
                               | itemcode == 'TEA DRY' & hh_j01 == 'YES'
                               | itemcode == 'COFFEE AND COCOA' & hh_j01 == 'YES'
                               | itemcode == 'OTHER RAW MATERIALS FOR DRINKS' & hh_j01 == 'YES'
                               | itemcode == 'BOTTLED/CANNED SOFT DRINKS (SODA, JUICE, WATER)' & hh_j01 == 'YES'
                               | itemcode == 'PREPARED TEA, COFFEE' & hh_j01 == 'YES'
                               | itemcode == 'SALT' & hh_j01 == 'YES'
                               , 1, 
                               ifelse (NA)))

by_hhid <- group_by(DDS2012, y3_hhid)
DDS2012total <- summarise (by_hhid, 
                       cereals = mean(cereals, na.rm = TRUE), 
                       rootsandtubers = mean(rootsandtubers, na.rm = TRUE), 
                       vegetables = mean(vegetables, na.rm=TRUE), 
                       pulsesandnuts = mean(pulsesandnuts, na.rm=TRUE), 
                       fruits = mean(fruits, na.rm=TRUE), 
                       meat = mean(meat, na.rm=TRUE), 
                       eggs = mean(eggs, na.rm=TRUE), 
                       fishandseafood= mean(fishandseafood, na.rm=TRUE), 
                       milkandmilkproducts= mean(milkandmilkproducts, na.rm=TRUE), 
                       oilsandfats=mean(oilsandfats, na.rm=TRUE), 
                       sugar=mean(sugar, na.rm=TRUE), 
                       condiments=mean(condiments, na.rm=TRUE))

DDS2012total[is.na(DDS2012total)] <- 0

DDS2012total$dietary_diversity_score <- 
  DDS2012total$cereals + 
  DDS2012total$rootsandtubers + 
  DDS2012total$vegetables + 
  DDS2012total$pulsesandnuts + 
  DDS2012total$fruits + 
  DDS2012total$meat + 
  DDS2012total$eggs + 
  DDS2012total$fishandseafood + 
  DDS2012total$milkandmilkproducts +
  DDS2012total$oilsandfats + 
  DDS2012total$sugar + 
  DDS2012total$condiments

DDS2012total<- select(DDS2012total, y3_hhid, dietary_diversity_score)
DDS2012total<-mutate(DDS2012total, surveyyear=2012) %>% rename(hhid2012=y3_hhid)

dataPath <- "D:/UserData/verbe038/TZA/2012/Data"
saveRDS(DDS2012total, file.path(dataPath, "DDS2012total.rds"))

rm(DDS2012total, DDS2012, HH_SEC_J1, by_hhid)


