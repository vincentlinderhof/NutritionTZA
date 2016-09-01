library(AER)
library(systemfit)
library(ivpack)
library(stargazer)
library(woe)

dataPath <- "C:/Users/afver/OneDrive/Documenten/Removable Disk/verbe038/TZA"
all_maize <-readRDS(file.path(dataPath, "all_maize"))
Data2010_2012maize <-readRDS(file.path(dataPath, "Data2010_2012maize"))



FCSo <- lm(FCS ~ hybrd1 +  fert1 + fem_head + ed_any+years + hhsize + dependency +
             log(area_tot)  + log(asset) + hqi +
             log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear), data=Data2010_2012maize)

FCS1 <- lm(hybrd1 ~ subseed1+ subfert1 + fem_head + ed_any+years + hhsize + dependency +
             log(area_tot)  + log(asset) + hqi +
             log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear), data=Data2010_2012maize)

FCS2 <- lm(fert1 ~ subfert1 +subseed1+  fem_head + ed_any+years + hhsize + dependency +
             log(area_tot)  + log(asset) + hqi +
             log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear), data=Data2010_2012maize)

FCSi <- ivreg(FCS ~ hybrd1 +  fert1 + fem_head + ed_any+years + hhsize + dependency +
                log(area_tot)  + log(asset) + hqi +
                log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear)|
                fem_head + ed_any+years + hhsize + dependency +
                log(area_tot)  + log(asset) + hqi +
                log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear) +subseed1 + subfert1, data=Data2010_2012maize)

stargazer(FCSo, FCS1, FCS2, FCSi, type="html", out='IVFCS.htm', model.numbers = FALSE, column.labels = c("OLS","Stage 1","Stage 1", "Stage 2"))
stargazer( FCSo,  FCS1,  FCS2,  FCSi, type="html", out='IV FCS.htm', model.numbers = FALSE, column.labels = c("OLS","Stage 1","Stage 1", "Stage 2"), 
          covariate.labels = c("Hybrid seed use", "Fertiliser use", "Received subsidised seeds", "Received subsidised fertiliser", 
                               "Female headship", "Education head", "Years in community", "Household size", "Dependency ratio", "Ln total land size", "Ln total farm assets", "Housing quality index", 
                               "Ln tropical livestock unit", "Credit- or saving group",
                               "Eastern zone", "Lake zone", "Northern zone", "Southern zone", "Southern Highlands", "Western zone",
                               "Total annual rainfall", "Average annual temperature", "Distance to nearest town", "Distance to nearest market", "Surveyyear 2012")
          , dep.var.labels = c("FCS", "Hybrid seed", "Fertiliser", "FCS"))

summary(FCSi, diagnostics=TRUE)
#Each Year

FCS2010o <- lm(FCS ~ hybrd1 +  fert1 + fem_head + ed_any+years + hhsize + dependency +
                 log(area_tot)  + log(asset) + hqi +
                 log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market, data=Data2010maize)

FCS20101 <- lm(hybrd1 ~ subseed1+ subfert1 + fem_head + ed_any+years + hhsize + dependency +
                 log(area_tot)  + log(asset) + hqi +
                 log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market, data=Data2010maize)

FCS20102 <- lm(fert1 ~ subfert1 +subseed1+  fem_head + ed_any+years + hhsize + dependency +
                 log(area_tot)  + log(asset) + hqi +
                 log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market, data=Data2010maize)

FCS2010i <- ivreg(FCS ~ hybrd1 + fert1 + fem_head + ed_any+years + hhsize + dependency +
                    log(area_tot)  + log(asset) + hqi +
                    log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market|
                    fem_head + ed_any+years + hhsize + dependency +
                    log(area_tot)  + log(asset) + hqi +
                    log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market +subseed1 + subfert1, data=Data2010maize)

FCS2012o <- lm(FCS ~ hybrd1 +  fert1 + fem_head + ed_any+years + hhsize + dependency +
                 log(area_tot)  + log(asset) + hqi +
                 log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market, data=Data2012maize)

FCS20121 <- lm(hybrd1 ~ subseed1+ subfert1 + fem_head + ed_any+years + hhsize + dependency +
                 log(area_tot)  + log(asset) + hqi +
                 log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market, data=Data2012maize)

FCS20122 <- lm(fert1 ~ subfert1 +subseed1+  fem_head + ed_any+years + hhsize + dependency +
                 log(area_tot)  + log(asset) + hqi +
                 log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market, data=Data2012maize)

FCS2012i <- ivreg(FCS ~ hybrd1 +  fert1 + fem_head + ed_any+years + hhsize + dependency +
                    log(area_tot)  + log(asset) + hqi +
                    log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market|
                    fem_head + ed_any+years + hhsize + dependency +
                    log(area_tot)  + log(asset) + hqi +
                    log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market +subseed1 + +subfert1, data=Data2012maize)

stargazer(FCS2010o, FCS20101, FCS20102, FCS2010i, FCS2012o, FCS20121, FCS20122, FCS2012i, type="html", out='IVFCSyear.htm', model.numbers = FALSE, 
          column.labels = c("OLS 2010","Stage 1 2010","Stage 1 2010", "Stage 2 2010", "OLS 2012","Stage 1 2012","Stage 1 2012", "Stage 2 2012"))


summary(FCS2010i, diagnostics=TRUE)
summary(FCS2012i, diagnostics=TRUE)
