library(AER)
library(systemfit)
library(ivpack)
library(stargazer)
library(woe)

dataPath <- "C:/Users/afver/OneDrive/Documenten/Removable Disk/verbe038/TZA"
all_maize <-readRDS(file.path(dataPath, "all_maize"))
Data2010_2012maize <-readRDS(file.path(dataPath, "Data2010_2012maize"))



DDSo <- lm(DDS ~ hybrd1 + hybrd1* fert1 + fert1 + fem_head + ed_any+years+ hhsize + dependency +
             log(area_tot)  + log(asset) + hqi +
             log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear), data=all_maize)

DDS1 <- lm(hybrd1 ~ subseed1+ subfert1 +subseed1* subfert1 + fem_head + ed_any+years+ hhsize + dependency +
             log(area_tot)  + log(asset) + hqi +
             log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear), data=all_maize)

DDS2 <- lm(fert1 ~ subfert1 +subseed1+ subseed1* subfert1 + fem_head + ed_any+years+ hhsize + dependency +
             log(area_tot)  + log(asset) + hqi +
             log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear), data=all_maize)

DDSi <- ivreg(DDS ~ hybrd1 + fert1 + fem_head + ed_any+years+ hhsize + dependency +
                log(area_tot)  + log(asset) + hqi +
                log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear)|
                fem_head + ed_any+years+ hhsize + dependency +
                log(area_tot)  + log(asset) + hqi +
                log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear) +subseed1 +subfert1, data=all_maize)

Test <- ivreg(DDS ~ hybrd1 + fert1 + fem_head + ed_any+ hhsize + dependency +
                log(area_tot)  + log(asset) + hqi +
                SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear)|
                fem_head + ed_any+ hhsize + dependency +
                log(area_tot)  + log(asset) + hqi +
                SACCO + factor(ZONE) + avgpPrecip
              + avgTemp + dist2town + dist2market+factor(surveyyear) +subseed1 +subfert1, data=all_maize)

summary(Test, diagnostics=TRUE)

stargazer(DDSo, DDS1, DDS2, DDSi, type="html", out='IVDDS.htm', model.numbers = FALSE, column.labels = c("OLS","Stage 1","Stage 1", "Stage 2"), 
          covariate.labels = c("Hybrid seed use", "Fertiliser use", "Received subsidised seeds", "Received subsidised fertiliser", 
                               "Female headship", "Education head", "Years in community", "Household size", "Dependency ratio", "Ln total land size", "Ln total farm assets", "Housing quality index", 
                               "Ln tropical livestock unit", "Credit- or saving group",
                               "Eastern zone", "Lake zone", "Northern zone", "Southern zone", "Southern Highlands", "Western zone", "Zanzibar",
                               "Total annual rainfall", "Average annual temperature", "Distance to nearest town", "Distance to nearest market", "Surveyyear 2010", "Surveyyear 2012", "Hybrid:Fertiliser", "Subsidised seeds:Subsidised fertiliser", "Subsidised fertiliser:Subsidised seeds")
          , dep.var.labels = c("DDS", "Hybrid seed", "Fertiliser", "DDS"))
stargazer(DDSo, DDS1, DDS2, DDSi, type="html", out='IVDDS.htm', model.numbers = FALSE, column.labels = c("OLS","Stage 1","Stage 1", "Stage 2"), 
          covariate.labels = c("Hybrid seed use", "Fertiliser use", "Received subsidised seeds", "Received subsidised fertiliser", 
                               "Female headship", "Education head", "Years in community", "Household size", "Dependency ratio", "Ln total land size", "Ln total farm assets", "Housing quality index", 
                               "Ln tropical livestock unit", "Credit- or saving group",
                               "Eastern zone", "Lake zone", "Northern zone", "Southern zone", "Southern Highlands", "Western zone", "Zanzibar",
                               "Total annual rainfall", "Average annual temperature", "Distance to nearest town", "Distance to nearest market", "Surveyyear 2010", "Surveyyear 2012", "Hybrid:Fertiliser", "Subsidised seeds:Subsidised fertiliser", "Subsidised fertiliser:Subsidised seeds"), dep.var.labels = c("DDS", "Hybrid seed", "Fertiliser", "DDS"))


summary(DDSi, diagnostics=TRUE)

#Each Year
DDS2008o <- lm(DDS ~ hybrd1 + hybrd1* fert1 + fert1 + fem_head + ed_any+years+ hhsize + dependency +
             log(area_tot)  + log(asset) + hqi +
             log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear), data=all_maize2008)

DDS20081 <- lm(hybrd1 ~ subseed1+ subfert1 +subseed1* subfert1 + fem_head + ed_any+years+ hhsize + dependency +
             log(area_tot)  + log(asset) + hqi +
             log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear), data=all_maize2008)

DDS20082 <- lm(fert1 ~ subfert1 +subseed1+ subseed1* subfert1 + fem_head + ed_any+years+ hhsize + dependency +
             log(area_tot)  + log(asset) + hqi +
             log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear), data=all_maize2008)

DDS2008i <- ivreg(DDS ~ hybrd1 + hybrd1* fert1 + fert1 + fem_head + ed_any+years+ hhsize + dependency +
                log(area_tot)  + log(asset) + hqi +
                log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear)|
                fem_head + ed_any+years+ hhsize + dependency +
                log(area_tot)  + log(asset) + hqi +
                log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear) +subseed1 + subseed1* subfert1+subfert1, data=all_maize2008)
DDS2010o <- lm(DDS ~ hybrd1 + hybrd1* fert1 + fert1 + fem_head + ed_any+years+ hhsize + dependency +
                 log(area_tot)  + log(asset) + hqi +
                 log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear), data=all_maize2010)

DDS20101 <- lm(hybrd1 ~ subseed1+ subfert1 +subseed1* subfert1 + fem_head + ed_any+years+ hhsize + dependency +
                 log(area_tot)  + log(asset) + hqi +
                 log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear), data=all_maize2010)

DDS20102 <- lm(fert1 ~ subfert1 +subseed1+ subseed1* subfert1 + fem_head + ed_any+years+ hhsize + dependency +
                 log(area_tot)  + log(asset) + hqi +
                 log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear), data=all_maize2010)

DDS2010i <- ivreg(DDS ~ hybrd1 + fert1 + fem_head + ed_any+years+ hhsize + dependency +
                    log(area_tot)  + log(asset) + hqi +
                    log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear)|
                    fem_head + ed_any+years+ hhsize + dependency +
                    log(area_tot)  + log(asset) + hqi +
                    log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear) +subseed1 + subfert1, data=all_maize2010)

DDS2012o <- lm(DDS ~ hybrd1 + hybrd1* fert1 + fert1 + fem_head + ed_any+years+ hhsize + dependency +
                 log(area_tot)  + log(asset) + hqi +
                 log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear), data=all_maize2012)

DDS20121 <- lm(hybrd1 ~ subseed1+ subfert1 +subseed1* subfert1 + fem_head + ed_any+years+ hhsize + dependency +
                 log(area_tot)  + log(asset) + hqi +
                 log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear), data=all_maize2012)

DDS20122 <- lm(fert1 ~ subfert1 +subseed1+ subseed1* subfert1 + fem_head + ed_any+years+ hhsize + dependency +
                 log(area_tot)  + log(asset) + hqi +
                 log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear), data=all_maize2012)

DDS2012i <- ivreg(DDS ~ hybrd1 + hybrd1* fert1 + fert1 + fem_head + ed_any+years+ hhsize + dependency +
                    log(area_tot)  + log(asset) + hqi +
                    log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear)|
                    fem_head + ed_any+years+ hhsize + dependency +
                    log(area_tot)  + log(asset) + hqi +
                    log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear) +subseed1 + subseed1* subfert1+subfert1, data=all_maize2012)
 
stargazer(DDS2008o, DDS20081, DDS20082, DDS2008i, DDS2010o, DDS20101, DDS20102, DDS2010i, DDS2012o, DDS20121, DDS20122, DDS2012i, type="html", out='IVDDSyear.htm', model.numbers = FALSE, 
column.labels = c("OLS 2008","Stage 1 2008","Stage 1 2008", "Stage 2 2008", "OLS 2010","Stage 1 2010","Stage 1 2010", "Stage 2 2010", "OLS 2012","Stage 1 2012","Stage 1 2012", "Stage 2 2012"))



stargazer(DDS2008o, DDS20081, DDS20082, DDS2008i, type="html", out='IVDDS2008.htm', model.numbers = FALSE, column.labels = c("OLS","Stage 1","Stage 1", "Stage 2"))
stargazer(DDS2010o, DDS20101, DDS20102, DDS2010i, type="html", out='IVDDS2010.htm', model.numbers = FALSE, column.labels = c("OLS","Stage 1","Stage 1", "Stage 2"))

summary(DDS2008i, diagnostics=TRUE)
summary(DDS2010i, diagnostics=TRUE)
summary(DDS2012i, diagnostics=TRUE)