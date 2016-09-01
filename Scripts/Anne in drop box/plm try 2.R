dataPath <- "C:/Users/afver/OneDrive/Documenten/Removable Disk/verbe038/TZA"

balanced <- readRDS(file.path(dataPath, "balanced.rds")) %>%
  select(surveyyear, hhid, hhid2008, hhid2010, hhid2012, hybrd1, fert1, fem_head, ed_any, hhsize, dependency, area_tot, asset, hqi, TLU, SACCO, REGNAME, ZONE, avgpPrecip, avgTemp, dist2town, dist2market, DDS, FVS, subseed1, subfert1)

#DDS

between <- plm(DDS ~ hybrd1 + hybrd1* fert1 + fert1 + fem_head + ed_any + hhsize + dependency +
                 log(area_tot)  + log(asset) + hqi +
                 log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market|
                 fem_head + ed_any + hhsize + dependency +
                 log(area_tot)  + log(asset) + hqi +
                 log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market +subseed1 + subseed1* subfert1+subfert1, data=balanced,
                 index=c('hhid2012', 'surveyyear'),
                 model="between")

within <- plm(DDS ~ hybrd1 + hybrd1* fert1 + fert1 + fem_head + ed_any + hhsize + dependency +
               log(area_tot)  + log(asset) + hqi +
               log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market|
               fem_head + ed_any + hhsize + dependency +
               log(area_tot)  + log(asset) + hqi +
               log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market +subseed1 + subseed1* subfert1+subfert1, data=balanced,
             index=c('hhid2012', 'surveyyear'),
             model="within")

random <- plm(DDS ~ hybrd1 + hybrd1* fert1 + fert1 + fem_head + ed_any + hhsize + dependency +
                log(area_tot)  + log(asset) + hqi +
                log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market|
                fem_head + ed_any + hhsize + dependency +
                log(area_tot)  + log(asset) + hqi +
                log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market +subseed1 + subseed1* subfert1+subfert1, data=balanced,
              index=c('hhid2012', 'surveyyear'),
              model="random")


stargazer(between, within, random, out="plmDDS.htm", column.labels = c("between", "within", "random"))

phtest(within, random)

plmtest(within, c('twoways'), type=('bp'))

#final[complete.cases(final),]
#balanced <- balanced[complete.cases(balanced),]

#FVS

between <- plm(FVS ~ hybrd1 + hybrd1* fert1 + fert1 + fem_head + ed_any + hhsize + dependency +
                 log(area_tot)  + log(asset) + hqi +
                 log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market|
                 fem_head + ed_any + hhsize + dependency +
                 log(area_tot)  + log(asset) + hqi +
                 log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market +subseed1 + subseed1* subfert1+subfert1, data=balanced,
               index=c('hhid2012', 'surveyyear'),
               model="between")

within <- plm(FVS ~ hybrd1 + hybrd1* fert1 + fert1 + fem_head + ed_any + hhsize + dependency +
                log(area_tot)  + log(asset) + hqi +
                log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market|
                fem_head + ed_any + hhsize + dependency +
                log(area_tot)  + log(asset) + hqi +
                log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market +subseed1 + subseed1* subfert1+subfert1, data=balanced,
              index=c('hhid2012', 'surveyyear'),
              model="within")

random <- plm(FVS ~ hybrd1 + hybrd1* fert1 + fert1 + fem_head + ed_any + hhsize + dependency +
                log(area_tot)  + log(asset) + hqi +
                log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market|
                fem_head + ed_any + hhsize + dependency +
                log(area_tot)  + log(asset) + hqi +
                log(TLU+1) + SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market +subseed1 + subseed1* subfert1+subfert1, data=balanced,
              index=c('hhid2012', 'surveyyear'),
              model="random")


phtest(within, random)

plmtest(within, c('twoways'), type=('bp'))



stargazer(between, within, random, out="plmFVS.htm", column.labels = c("between", "within", "random"))

#final[complete.cases(final),]
#balanced <- balanced[complete.cases(balanced),]



