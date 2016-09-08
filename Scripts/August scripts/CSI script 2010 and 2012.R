# Compute CSI based on http://documents.wfp.org/stellent/groups/public/documents/manual_guide_proced/wfp211058.pdf
# WFP report, page 13


library(foreign)
library(dplyr)

HH_SEC_I1_2010 <- read.dta("D:/UserData/verbe038/data tza/2010/Data/HH_SEC_I1.dta")
HH_SEC_H_2012<- read.dta("D:/UserData/verbe038/data tza/2012/Data/HH_SEC_H.dta")

#CSI 2010

CSI2010 <- 
  select(HH_SEC_I1_2010, y2_hhid, hh_i01, hh_i02_1, hh_i02_2, hh_i02_3, hh_i02_4, hh_i02_5, hh_i02_6, hh_i02_7, hh_i02_8) %>%
  na.omit()

CSI2010$CSI <- 
  CSI2010$hh_i02_1*1 + 
  CSI2010$hh_i02_2*1 + 
  CSI2010$hh_i02_3*1 + 
  CSI2010$hh_i02_4*1 + 
  CSI2010$hh_i02_5*3 + 
  CSI2010$hh_i02_6*2 + 
  CSI2010$hh_i02_7*0 + 
  CSI2010$hh_i02_8*4

CSI2010<- select(CSI2010, y2_hhid, CSI)
CSI2010<- mutate(CSI2010, surveyyear=2010) %>% rename(hhid2010=y2_hhid)
TZA2010 <- left_join(TZA2010, CSI2010); rm(CSI2010)

#csi 2012

CSI2012 <- 
  select(HH_SEC_H_2012, y3_hhid, hh_h01, hh_h02_1, hh_h02_2, hh_h02_3, hh_h02_4, hh_h02_5, hh_h02_6, hh_h02_7, hh_h02_8)

CSI2012$CSI <- 
  CSI2012$hh_h02_1*1 + 
  CSI2012$hh_h02_2*1 + 
  CSI2012$hh_h02_3*1 + 
  CSI2012$hh_h02_4*1 + 
  CSI2012$hh_h02_5*3 + 
  CSI2012$hh_h02_6*2 + 
  CSI2012$hh_h02_7*0 + 
  CSI2012$hh_h02_8*4

CSI2012<- select(CSI2012, y3_hhid, CSI)
CSI2012<- mutate(CSI2012, surveyyear=2012) %>% rename(hhid2012=y3_hhid)

TZA2012 <- left_join(TZA2012, CSI2012); rm(CSI2012)
