library(AER)
library(systemfit)
library(ivpack)
library(stargazer)
library(woe)

dataPath <- "C:/Users/afver/OneDrive/Documenten/Removable Disk/verbe038/TZA"
all_maize <-readRDS(file.path(dataPath, "all_maize"))
Data2010_2012maize <-readRDS(file.path(dataPath, "Data2010_2012maize"))



FVSo <- lm(FVS ~ hybrd1 + hybrd1* fert1 + fert1 + fem_head + ed_any+years+ hhsize + dependency +
             log(area_tot)  + log(asset) + hqi +
             log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear), data=all_maize)

FVS1 <- lm(hybrd1 ~ subseed1+ subfert1 +subseed1*subfert1 + fem_head + ed_any+years+ hhsize + dependency +
             log(area_tot)  + log(asset) + hqi +
             log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear), data=all_maize)

FVS2 <- lm(fert1 ~ subfert1 +subseed1+ subseed1*subfert1 + fem_head + ed_any+years+ hhsize + dependency +
             log(area_tot)  + log(asset) + hqi +
             log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear), data=all_maize)

FVSi <- ivreg(FVS ~ hybrd1 + hybrd1* fert1 + fert1 + fem_head + ed_any+years+ hhsize + dependency +
                log(area_tot)  + log(asset) + hqi +
                log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear)|
                fem_head + ed_any+years+ hhsize + dependency +
                log(area_tot)  + log(asset) + hqi +
                log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear) +subseed1 + subseed1*subfert1+subfert1, data=all_maize)

 
#Each Year
FVS2008o <- lm(FVS ~ hybrd1 + hybrd1*hqi + fert1 + fem_head + ed_any+years+ hhsize + dependency +
                 log(area_tot)  + log(asset) + hqi +
                 log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear), data=all_maize2008)

FVS20081 <- lm(hybrd1 ~ subseed1+ subfert1 +subseed1*subfert1 + fem_head + ed_any+years+ hhsize + dependency +
                 log(area_tot)  + log(asset) + hqi +
                 log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear), data=all_maize2008)

FVS20082 <- lm(fert1 ~ subfert1 +subseed1+ subseed1*subfert1 + fem_head + ed_any+years+ hhsize + dependency +
                 log(area_tot)  + log(asset) + hqi +
                 log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear), data=all_maize2008)

FVS2008i <- ivreg(FVS ~ hybrd1 + hybrd1*hqi + fert1 + fem_head + ed_any+years+ hhsize + dependency +
                    log(area_tot)  + log(asset) + hqi +
                    log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear)|
                    fem_head + ed_any+years+ hhsize + dependency +
                    log(area_tot)  + log(asset) + hqi +
                    log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear) +subseed1 + subseed1*subfert1+subfert1, data=all_maize2008)
FVS2010o <- lm(FVS ~ hybrd1 + hybrd1*hqi + fert1 + fem_head + ed_any+years+ hhsize + dependency +
                 log(area_tot)  + log(asset) + hqi +
                 log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear), data=all_maize2010)

FVS20101 <- lm(hybrd1 ~ subseed1+ subfert1 +subseed1*subfert1 + fem_head + ed_any+years+ hhsize + dependency +
                 log(area_tot)  + log(asset) + hqi +
                 log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear), data=all_maize2010)

FVS20102 <- lm(fert1 ~ subfert1 +subseed1+ subseed1*subfert1 + fem_head + ed_any+years+ hhsize + dependency +
                 log(area_tot)  + log(asset) + hqi +
                 log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear), data=all_maize2010)

FVS2010i <- ivreg(FVS ~ hybrd1 + fert1 + fem_head + ed_any+years+ hhsize + dependency +
                    log(area_tot)  + log(asset) + hqi +
                    log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear)|
                    fem_head + ed_any+years+ hhsize + dependency +
                    log(area_tot)  + log(asset) + hqi +
                    log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear) +subseed1 + subfert1, data=all_maize2010)

FVS2012o <- lm(FVS ~ hybrd1 + hybrd1*hqi + fert1 + fem_head + ed_any+years+ hhsize + dependency +
                 log(area_tot)  + log(asset) + hqi +
                 log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear), data=all_maize2012)

FVS20121 <- lm(hybrd1 ~ subseed1+ subfert1 +subseed1*subfert1 + fem_head + ed_any+years+ hhsize + dependency +
                 log(area_tot)  + log(asset) + hqi +
                 log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear), data=all_maize2012)

FVS20122 <- lm(fert1 ~ subfert1 +subseed1+ subseed1*subfert1 + fem_head + ed_any+years+ hhsize + dependency +
                 log(area_tot)  + log(asset) + hqi +
                 log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear), data=all_maize2012)

FVS2012i <- ivreg(FVS ~ hybrd1 + fert1 + fem_head + ed_any+years+ hhsize + dependency +
                    log(area_tot)  + log(asset) + hqi +
                    log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear)|
                    fem_head + ed_any+years+ hhsize + dependency +
                    log(area_tot)  + log(asset) + hqi +
                    log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear) +subseed1 + subseed1*subfert1+subfert1, data=all_maize2012)

stargazer(FVS2008o, FVS20081, FVS20082, FVS2008i, FVS2010o, FVS20101, FVS20102, FVS2010i, FVS2012o, FVS20121, FVS20122, FVS2012i, type="html", out='IVFVSyear.htm', model.numbers = FALSE)

stargazer(FVSo, FVS1, FVS2, FVSi, type="html", out='IVFVS.htm', model.numbers = FALSE, column.labels = c("OLS","Stage 1","Stage 1", "Stage 2"), 
          covariate.labels = c("Hybrid seed use", "Fertiliser use", "Received subsidised seeds", "Received subsidised fertiliser", 
                               "Female headship", "Education head", "Years in community", "Household size", "Dependency ratio", "Ln total land size", "Ln total farm assets", "Housing quality index", 
                               "Ln tropical livestock unit", "Credit- or saving group",
                               "Eastern zone", "Lake zone", "Northern zone", "Southern zone", "Southern Highlands", "Western zone", "Zanzibar",
                               "Total annual rainfall", "Average annual temperature", "Distance to nearest town", "Distance to nearest market", "Surveyyear 2010", "Surveyyear 2012", "Hybrid:Fertiliser", "Subsidised seeds:Subsidised fertiliser", "Subsidised fertiliser:Subsidised seeds"),
          dep.var.labels = c("FVS", "Hybrid seed", "Fertiliser", "FVS"))

summary(FVS2008i, diagnostics=TRUE)
summary(FVS2010i, diagnostics=TRUE)
summary(FVS2012i, diagnostics=TRUE)
summary(FVSi, diagnostics=TRUE)
