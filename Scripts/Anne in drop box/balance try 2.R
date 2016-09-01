dataPath <- "C:/Users/afver/OneDrive/Documenten/Removable Disk/verbe038/TZA"

all_maize <- readRDS(file.path(dataPath, "all_maize"))
fullData <- readRDS(file.path(dataPath, "fullData"))


x1 <- all_maize$hhid[all_maize$surveyyear==2008]
x2 <- all_maize$hhid[all_maize$surveyyear==2010]
x3 <- all_maize$hhid[all_maize$surveyyear==2012]

all_maize <- filter(all_maize, hhid2008 %in% x1)
all_maize <- filter(all_maize, hhid2010 %in% x2)
all_maize <- filter(all_maize, hhid2012 %in% x3)

all_maize <- 
  select(all_maize, surveyyear, hhid, hhid2008, hhid2010, hhid2012, hybrd1, fert1, fem_head, ed_any, hhsize, dependency, area_tot, asset, hqi, TLU, SACCO, REGNAME, ZONE, avgpPrecip, avgTemp, dist2town, dist2market, DDS, FVS, subseed1, subfert1)

all_maize <- unique(all_maize)

summary(all_maize)
saveRDS(all_maize, file.path(dataPath, "balanced.rds"))
ptest(within, random)
plmtest(within, c('time'), type=('bp'))
