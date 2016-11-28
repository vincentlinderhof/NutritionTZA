dataPath <- "C:/Users/afver/OneDrive/Documenten/Removable Disk/verbe038/TZA"
setwd("C:/Users/afver/OneDrive/Documenten/Removable Disk/a.Thesis/Outputs")

library(haven)
library(stringr)
library(reshape2)
library(dplyr)
library(stargazer)
library(plm)
library(systemfit)
library(pglm)


Data2010_2012maize <-readRDS(file.path(dataPath, "Data2010_2012maize"))
Data2010_2012maize <- select(Data2010_2012maize, hhid2008, hhid2010, hhid2012, surveyyear, status, hybrd1, fert1 , fem_head , ed_any , years, hhsize , dependency ,
                             income, off_farm_income_hh, area_tot  , asset , hqi ,
                             TLU , SACCO , ZONE, central, eastern, lake, northern, southern, southern_highlands, western, zanzibar, 
                             avgpPrecip , avgTemp , dist2town , dist2market, DDS, FVS, FCS, CSI, subseed1, subfert1, dishybrd1, disfert1, m)
Data2010_2012maize <- unique(Data2010_2012maize)

DDS <- lm(DDS ~ hybrd1 + fert1 + hybrd1*fert1 + fem_head + ed_any+ years+hhsize + dependency + log(income+1)+ log(off_farm_income_hh+1) +
               log(area_tot)  + log(asset) + log(TLU+1)+ hqi +
               SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear), data=Data2010_2012maize)

FVS <- lm(FVS ~ hybrd1 + fert1 + hybrd1*fert1 +fem_head + ed_any+ hhsize + dependency + log(income+1)+ log(off_farm_income_hh+1) +
            log(area_tot)  + log(asset) + log(TLU+1)+ hqi +
            SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear), data=Data2010_2012maize)

FCS <- lm(FCS ~ hybrd1 + fert1 + hybrd1*fert1 + fem_head + ed_any+ hhsize + dependency + log(income+1)+ log(off_farm_income_hh+1) +
            log(area_tot)  + log(asset) + log(TLU+1)+ hqi +
            SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear), data=Data2010_2012maize)

CSI <- lm(CSI ~ hybrd1 + fert1 + hybrd1*fert1+fem_head + ed_any+ hhsize + dependency + log(income+1)+ log(off_farm_income_hh+1) +
            log(area_tot)  + log(asset) + log(TLU+1)+ hqi +
            SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear), data=Data2010_2012maize)

             
stargazer(DDS, FVS, FCS, CSI, out="final.htm") 
rm(DDS, FVS, FCS, CSI)
             