## This file uses dataframes created with the following files
## TZA_2008.r
## TZA_2010.r
## TZA_2012.r


library(Deducer)

frequencies(TZA2008HH$status,r.digits=1)
frequencies(TZA2010HH$status,r.digits=1)
frequencies(TZA2012HH$status,r.digits=1)
library(Deducer)
frequencies(TZA2008HH$rural,r.digits=1)
TZA2008HH_z <-subset(TZA2008HH, rural==1)


frequencies(TZA2010HH$rural,r.digits=1)
frequencies(TZA2012HH$rural,r.digits=1)

frequencies(TZA2008HH$ZONE,r.digits=1)
frequencies(TZA2010HH$ZONE,r.digits=1)
frequencies(TZA2012HH$ZONE,r.digits=1)

frequencies(TZA2008HH$cage,r.digits=1)
frequencies(TZA2010HH$cage,r.digits=1)
frequencies(TZA2012HH$cage,r.digits=1)

descriptive.table(vars = d(status, sex, age, years, cage, ed_any, N1555, death, SACCO, 
                           dist2town, dist2market, dist2HQ), data= TZA2008HH, 
                  func.names = c("Mean", "St. Deviation", "Min", "Max", "Valid N"))

descriptive.table(vars = d(status, sex, age, years, cage, ed_any, N1555, death, SACCO), data= TZA2010HH, 
                  func.names = c("Mean", "St. Deviation", "Min", "Max", "Valid N"))

descriptive.table(vars = d(status, sex, age, years, cage, ed_any, N1555, death, SACCO), data= TZA2012HH, 
                  func.names = c("Mean", "St. Deviation", "Min", "Max", "Valid N"))


TZA2008HH_r <-  subset(TZA2008HH, rural == 1)
TZA2010HH_r <-  subset(TZA2010HH, rural == 1)
TZA2012HH_r <-  subset(TZA2012HH, rural == 1)

frequencies(TZA2008HH_r$ZONE,r.digits=1)
frequencies(TZA2010HH_r$ZONE,r.digits=1)
frequencies(TZA2012HH_r$ZONE,r.digits=1)

rm(TZA2008HH_r, TZA2010HH_r, TZA2012HH_r)