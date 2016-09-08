# percentage of households that have consumed each food item, for each wave, to txt. file

library(dplyr)
library(foreign)
#open data
SEC_K1 <- read.dta("//scomp0851/verbe038$/AppData/FolderRedirection/Desktop/data tza/2008/Data/SEC_K1.dta")
HH_SEC_K1 <- read.dta("//scomp0851/verbe038$/AppData/FolderRedirection/Desktop/data tza/2010/Data/HH_SEC_K1.dta")
HH_SEC_J1 <- read.dta("//scomp0851/verbe038$/AppData/FolderRedirection/Desktop/data tza/2010/Data/HH_SEC_J1.dta")

options(dplyr.print_max = 1e9)
sink("tp.txt")

2008

SEC_K1 %>%
  group_by(skcode) %>% 
  summarise(PERCENTAGE_YES= sprintf('%f%%', 100*sum(skq1=='Yes')/n()), 
            PERCENTAGE_NO = sprintf('%f%%', 100*sum(skq1=='No')/n()), number = n())

2010

HH_SEC_K1 %>%
  filter(!is.na(hh_k01_2)) %>%
  group_by(itemcode) %>%
  summarise(PERCENTAGE_YES= sprintf('%f%%', 100*sum(hh_k01_2=='Yes')/n()), 
            PERCENTAGE_NO = sprintf('%f%%', 100*sum(hh_k01_2=='No')/n()), number = n())
2012

HH_SEC_J1 %>%
  group_by(itemcode) %>% 
  summarise(PERCENTAGE_YES= sprintf('%f%%', 100*sum(hh_j01=='YES')/n()), 
            PERCENTAGE_NO = sprintf('%f%%', 100*sum(hh_j01=='NO')/n()), number = n())


sink(NULL)

read.dta()

