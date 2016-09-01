dataPath <- "C:/Users/afver/OneDrive/Documenten/Removable Disk/verbe038/TZA"

library(haven)
library(stringr)
library(reshape2)
library(dplyr)
library(stargazer)
library(plm)
library(systemfit)
library(pglm)

all_maize2008 <- readRDS(file.path(dataPath, "all_maize2008"))
all_maize2010 <- readRDS(file.path(dataPath, "all_maize2010"))
all_maize2012 <- readRDS(file.path(dataPath, "all_maize2012"))
all_maize <- readRDS(file.path(dataPath, "all_maize"))

#All waves
rDDSfull <- lm(dietary_diversity_score ~ hybrd1 + fert1 + hybrd1*fert1+ hybrd1*fem_head + hybrd1*log(TLU+1) + fem_head + ed_any + hhsize + dependency +
                 log(area_tot)  + log(asset) + hqi +
                 log(TLU+1) + SACCO + avgpPrecip + avgTemp + dist2town + dist2market, data=all_maize)

#wave 2008
rDDS2008 <- lm(dietary_diversity_score ~ hybrd1 + fert1 + fem_head + ed_any + hhsize + dependency +
                 log(area_tot)  + log(asset) + hqi +
                 log(TLU+1) + SACCO + avgpPrecip + avgTemp + dist2town + dist2market, data=all_maize2008)

#wave 2010
rDDS2010 <- lm(dietary_diversity_score ~ hybrd1 + fert1 + fem_head + ed_any + hhsize + dependency +
                 log(area_tot)  + log(asset) + hqi +
                 log(TLU+1) + SACCO + avgpPrecip + avgTemp + dist2town + dist2market, data=all_maize2010)

#wave 2012
rDDS2012 <- lm(dietary_diversity_score ~ hybrd1 + fert1 + fem_head + ed_any + hhsize + dependency +
                 log(area_tot)  + log(asset) + hqi +
                 log(TLU+1) + SACCO + avgpPrecip + avgTemp + dist2town + dist2market, data=all_maize2012)

#All waves
rFVSfull <- lm(FVS ~ hybrd1 + fert1 + hybrd1*log(asset) + hybrd1*log(TLU+1) + fem_head + ed_any + hhsize + dependency +
                 log(area_tot)  + log(asset) + hqi +
                 log(TLU+1) + SACCO + avgpPrecip + avgTemp + dist2town + dist2market, data=all_maize)

#wave 2008
rFVS2008 <- lm(FVS ~ hybrd1 + fert1 + fem_head + ed_any + hhsize + dependency +
                 log(area_tot)  + log(asset) + hqi +
                 log(TLU+1) + SACCO + avgpPrecip + avgTemp + dist2town + dist2market, data=all_maize2008)

#wave 2010
rFVS2010 <- lm(FVS ~ hybrd1 + fert1 + fem_head + ed_any + hhsize + dependency +
                 log(area_tot)  + log(asset) + hqi +
                 log(TLU+1) + SACCO + avgpPrecip + avgTemp + dist2town + dist2market, data=all_maize2010)

#wave 2012
rFVS2012 <- lm(FVS ~ hybrd1 + fert1 + fem_head + ed_any + hhsize + dependency +
                 log(area_tot)  + log(asset) + hqi +
                 log(TLU+1) + SACCO + avgpPrecip + avgTemp + dist2town + dist2market, data=all_maize2012)
#wave 2010
rFCS2010 <- lm(FCS ~ hybrd1 + fert1 + fem_head + ed_any + hhsize + dependency +
                 log(area_tot)  + log(asset) + hqi +
                 log(TLU+1) + SACCO + avgpPrecip + avgTemp + dist2town + dist2market, data=Data2010maize)
#wave 2012
rFCS2012 <- lm(FCS ~ hybrd1 + fert1 + fem_head + ed_any + hhsize + dependency +
                 log(area_tot)  + log(asset) + hqi +
                 log(TLU+1) + SACCO + avgpPrecip + avgTemp + dist2town + dist2market, data=Data2012maize)
#all waves
rFCSfull <- lm(FCS ~ hybrd1 + hybrd1*log(TLU+1)+ fert1 + fem_head + ed_any + hhsize + dependency +
                 log(area_tot)  + log(asset) + hqi +
                 log(TLU+1) + SACCO + avgpPrecip + avgTemp + dist2town + dist2market, data=Data2010_2012maize)

#wave 2010
rCSI2010 <- lm(CSI ~ hybrd1 + fert1 + fem_head + ed_any + hhsize + dependency +
                 log(area_tot)  + log(asset) + hqi +
                 log(TLU+1) + SACCO + avgpPrecip + avgTemp + dist2town + dist2market, data=Data2010maize)

#wave 2012
rCSI2012 <- lm(CSI ~ hybrd1 + fert1 + fem_head + ed_any + hhsize + dependency +
                 log(area_tot)  + log(asset) + hqi +
                 log(TLU+1) + SACCO + avgpPrecip + avgTemp + dist2town + dist2market, data=Data2012maize)

#wave 2010 2012
rCSIfull <- lm(CSI ~ hybrd1 + fert1 + fem_head + ed_any + hhsize + dependency +
                 log(area_tot)  + log(asset) + hqi +
                 log(TLU+1) + SACCO + avgpPrecip + avgTemp + dist2town + dist2market, data=Data2010_2012maize)

#stargazer(rDDS2008, rDDS2010, rDDS2012, rDDSfull, rFVS2008, rFVS2010, rFVS2012, rFVSfull, rFCS2010, rFCS2012, rFCSfull, rCSI2010, rCSI2012, rCSIfull, type="html", out="rDDS3.txt", diagnostics =TRUE) 
stargazer(rDDSfull, rFVSfull, rFCSfull, rCSIfull, type="html", out="regtry4.htm", diagnostics =TRUE)
rm(rDDS2008, rDDS2010, rDDS2012, rDDSfull, rFVS2008, rFVS2010, rFVS2012, rFVSfull, rFCS2010, rFCS2012, rFCSfull, rCSI2010, rCSI2012, rCSIfull)

