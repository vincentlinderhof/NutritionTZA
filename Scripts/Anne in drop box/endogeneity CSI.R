library(AER)
library(systemfit)
library(ivpack)
library(stargazer)
library(woe)

dataPath <- "C:/Users/afver/OneDrive/Documenten/Removable Disk/verbe038/TZA"

Data2010_2012maize <-readRDS(file.path(dataPath, "Data2010_2012maize"))



CSIo <- lm(CSI ~ hybrd1  + fert1 + fem_head + ed_any+years+ hhsize + dependency +
             log(area_tot)  + log(asset) + hqi +
             log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market + factor(surveyyear),  data=Data2010_2012maize)

CSI1 <- lm(hybrd1 ~ subseed1+ subfert1 + fem_head + ed_any+years+ hhsize + dependency +
             log(area_tot)  + log(asset) + hqi +
             log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market + factor(surveyyear),  data=Data2010_2012maize)

CSI2 <- lm(fert1 ~ subfert1 +subseed1 + fem_head + ed_any+years+ hhsize + dependency +
             log(area_tot)  + log(asset) + hqi +
             log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market + factor(surveyyear),  data=Data2010_2012maize)

CSIi <- ivreg(CSI ~ hybrd1  + fert1 + fem_head + ed_any+years+ hhsize + dependency +
                log(area_tot)  + log(asset) + hqi +
                log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market + factor(surveyyear)| 
                fem_head + ed_any+years+ hhsize + dependency +
                log(area_tot)  + log(asset) + hqi +
                log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market + factor(surveyyear) +subseed1 + subfert1, data=Data2010_2012maize)

stargazer(CSIo, CSI1, CSI2, CSIi, type="html", out='IVCSI.htm', model.numbers = FALSE, column.labels = c("OLS","Stage 1","Stage 1", "Stage 2"),
          covariate.labels = c("Hybrid seed use", "Fertiliser use", "Received subsidised seeds", "Received subsidised fertiliser", 
           "Female headship", "Education head", "Years in community", "Household size", "Dependency ratio", "Ln total land size", "Ln total farm assets", "Housing quality index", 
           "Ln tropical livestock unit", "Credit- or saving group",
           "Eastern zone", "Lake zone", "Northern zone", "Southern zone", "Southern Highlands", "Western zone",
           "Total annual rainfall", "Average annual temperature", "Distance to nearest town", "Distance to nearest market", "Surveyyear 2012"))
sink(fil="test.htm", type = c("output", "message"))
summary(CSIi, diagnostics=TRUE)
closeAllConnections()

#Each Year

CSI2010o <- lm(CSI ~ hybrd1   + fert1 + fem_head + ed_any+years+ hhsize + dependency +
                 log(area_tot)  + log(asset) + hqi +
                 log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market + factor(surveyyear),  data=Data2010maize)

CSI20101 <- lm(hybrd1 ~ subseed1+ subfert1   + fem_head + ed_any+years+ hhsize + dependency +
                 log(area_tot)  + log(asset) + hqi +
                 log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market + factor(surveyyear),  data=Data2010maize)

CSI20102 <- lm(fert1 ~ subfert1 +subseed1+ subseed1*hqi + fem_head + ed_any+years+ hhsize + dependency +
                 log(area_tot)  + log(asset) + hqi +
                 log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market + factor(surveyyear),  data=Data2010maize)

CSI2010i <- ivreg(CSI ~ hybrd1 + fert1 + fem_head + ed_any+years+ hhsize + dependency +
                    log(area_tot)  + log(asset) + hqi +
                    log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market + factor(surveyyear)| 
                    fem_head + ed_any+years+ hhsize + dependency +
                    log(area_tot)  + log(asset) + hqi +
                    log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market + factor(surveyyear) +subseed1 + subfert1, data=Data2010maize)

CSI2012o <- lm(CSI ~ hybrd1   + fert1 + fem_head + ed_any+years+ hhsize + dependency +
                 log(area_tot)  + log(asset) + hqi +
                 log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market + factor(surveyyear),  data=Data2012maize)

CSI20121 <- lm(hybrd1 ~ subseed1+ subfert1   + fem_head + ed_any+years+ hhsize + dependency +
                 log(area_tot)  + log(asset) + hqi +
                 log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market + factor(surveyyear),  data=Data2012maize)

CSI20122 <- lm(fert1 ~ subfert1 +subseed1+ subseed1*hqi + fem_head + ed_any+years+ hhsize + dependency +
                 log(area_tot)  + log(asset) + hqi +
                 log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market + factor(surveyyear),  data=Data2012maize)

CSI2012i <- ivreg(CSI ~ hybrd1   + fert1 + fem_head + ed_any+years+ hhsize + dependency +
                    log(area_tot)  + log(asset) + hqi +
                    log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market + factor(surveyyear)| 
                    fem_head + ed_any+years+ hhsize + dependency +
                    log(area_tot)  + log(asset) + hqi +
                    log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market + factor(surveyyear) +subseed1 + subseed1*hqi+subfert1, data=Data2012maize)

stargazer(CSI2010o, CSI20101, CSI20102, CSI2010i, CSI2012o, CSI20121, CSI20122, CSI2012i, type="html", out='IVCSIyear.htm', model.numbers = FALSE, 
          column.labels = c("OLS 2010","Stage 1 2010","Stage 1 2010", "Stage 2 2010", "OLS 2012","Stage 1 2012","Stage 1 2012", "Stage 2 2012"))


summary(CSI2010i, diagnostics=TRUE)
summary(CSI2012i, diagnostics=TRUE)
