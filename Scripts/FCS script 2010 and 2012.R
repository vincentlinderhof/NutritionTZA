# Build FCS for waves 2 and 3, based on instruction WFP 
# URL: http://documents.wfp.org/stellent/groups/public/documents/manual_guide_proced/wfp197216.pdf

library(foreign)
library(dplyr)
library(tidyr)


HH_SEC_K2_2010 <- read.dta("D:/UserData/verbe038/data tza/2010/Data/HH_SEC_K2.dta")
HH_SEC_J3_2012 <- read.dta("D:/UserData/verbe038/data tza/2012/Data/HH_SEC_J3.dta")

# compute Food Consumption Score of wave 2010-2011
FCS2010 <- group_by(HH_SEC_K2_2010, y2_hhid) %>% 
  na.omit() %>%
  select(y2_hhid, itemcode, hh_k08_3) %>%
  spread(itemcode, hh_k08_3) %>% 
  mutate(AB = A+B) %>%
  select(-A, -B) %>%
  mutate(AB=replace(AB, AB>=7, 7)) %>%
  rename(main_staples=AB, pulses_nuts=C, vegetables=D, meat_fish=E, fruits=F, milk=G, oil=H, sugar=I, condiments=J)

FCS2010$FCS <- 
  FCS2010$main_staples*2 + 
  FCS2010$pulses_nuts*3 + 
  FCS2010$vegetables*1 + 
  FCS2010$fruits*1 + 
  FCS2010$milk*4 + 
  FCS2010$meat_fish*4 + 
  FCS2010$sugar*0.5 + 
  FCS2010$condiments*0

FCS2010<- select(FCS2010, y2_hhid, FCS)
FCS2010<- mutate(FCS2010, surveyyear=2010) %>% rename(hhid2010=y2_hhid)

TZA2010 <- left_join(TZA2010, FCS2010); rm(FCS2010)

# compute Food Consumption Score of wave 2012-2013
FCS2012 <- group_by(HH_SEC_J3_2012, y3_hhid, itemcode) %>% 
  na.omit() %>%
  select(y3_hhid, itemcode, hh_j09_3) %>%
  spread(itemcode, hh_j09_3) %>%
  rename(A=`A. CEREALS, GRAINS AND CEREAL PRODUCTS`, 
         B= `B. ROOTS, TUBERS, AND PLANTAINS`, 
         pulses_nuts=`C. NUTS AND PULSES`,
         vegetables=`D. VEGETABLES`,
         meat_fish=`E. MEAT, FISH AND ANIMAL PRODUCTS`,
         fruits=`F. FRUITS`, 
         milk=`G. MILK/MILK PRODUCTS`,
         oil=`H. FATS/OIL`,
         sugar=`I. SUGAR/SUGAR PRODUCTS/HONEY`,
         condiments=`J. SPICES/CONDIMENTS`) %>%
  mutate(AB=A+B) %>%
  select(-A, -B) %>%
  mutate(AB=replace(AB, AB>=7, 7)) %>%
  rename(main_staples=AB)

FCS2012$FCS <- 
  FCS2012$main_staples*2 + 
  FCS2012$pulses_nuts*3 + 
  FCS2012$vegetables*1 + 
  FCS2012$fruits*1 + 
  FCS2012$milk*4 + 
  FCS2012$meat_fish*4 + 
  FCS2012$sugar*0.5 + 
  FCS2012$condiments*0

FCS2012<- select(FCS2012, y3_hhid, FCS)
FCS2012<- mutate(FCS2012, surveyyear=2012) %>% rename(hhid2012=y3_hhid)

TZA2012 <- left_join(TZA2012, FCS2012); rm(FCS2012)

