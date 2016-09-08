# This script produces the modified food variety score

library(dplyr)
library(foreign)
options(dplyr.print_max = 1e9)
library(tidyr)

# for the wave of 2008

SEC_K1 <- read.dta("D:/UserData/verbe038/data tza/2008/Data/SEC_K1.dta")

FVS2008 <- select(SEC_K1, hhid, skcode, skq1)
FVS2008 <-
  mutate(FVS2008, count = ifelse(skq1 == 'Yes', 1, ifelse(NA))) %>%
  group_by(hhid) %>%
  spread(skcode, count) %>%
  select(-skq1) %>%
  filter (! duplicated(hhid)) %>%
  replace(is.na(.), 0)

FVS2008$FVS <- rowSums(FVS2008[2:60])

FVS2008<- select(FVS2008, hhid, FVS)
FVS2008<- mutate(FVS2008, surveyyear=2008) %>% rename(hhid2008=hhid)

TZA2008 <- left_join(TZA2008, FVS2008); rm(FVS2008)

# for the wave of 2010
HH_SEC_K1 <- read.dta("D:/UserData/verbe038/data tza/2010/Data/HH_SEC_K1.dta")

FVS2010 <- select(HH_SEC_K1, y2_hhid, itemcode, hh_k01_2)
FVS2010 <-
  mutate(FVS2010, count = ifelse(hh_k01_2 == 'Yes', 1, ifelse(NA))) %>%
  group_by(y2_hhid) %>%
  spread(itemcode, count) %>%
  select(-hh_k01_2) %>%
  filter (! duplicated(y2_hhid)) %>%
  replace(is.na(.), 0)

FVS2010$FVS <- rowSums(FVS2010[2:60])

FVS2010<- select(FVS2010, y2_hhid, FVS)
FVS2010<- mutate(FVS2010, surveyyear=2010) %>% rename(hhid2010=y2_hhid)

TZA2010 <- left_join(TZA2010, FVS2010); rm(FVS2010)

# for the wave of 2012
HH_SEC_J1 <- read.dta("D:/UserData/verbe038/data tza/2012/Data/HH_SEC_J1.dta")

FVS2012 <- select(HH_SEC_J1, y3_hhid, itemcode, hh_j01)
FVS2012 <-
  mutate(FVS2012, count = ifelse(hh_j01 == 'YES', 1, ifelse(NA))) %>%
  group_by(y3_hhid) %>%
  spread(itemcode, count) %>%
  select(-hh_j01) %>%
  filter (! duplicated(y3_hhid)) %>%
  replace(is.na(.), 0)

FVS2012$FVS <- rowSums(FVS2012[2:60])

FVS2012<- select(FVS2012, y3_hhid, FVS)
FVS2012<- mutate(FVS2012, surveyyear=2012) %>% rename(hhid2012=y3_hhid)

TZA2012 <- left_join(TZA2012, FVs2012); rm(FVS2012)
