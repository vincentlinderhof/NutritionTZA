library(tidyr)
dataPath <- dataPath <- "C:/Users/afver/OneDrive/Documenten/Removable Disk/verbe038/TZA"
lag <- select(all_maize, surveyyear, DISNAME, disfert, dishybrd) %>% unique

lag08 <- filter(lag, surveyyear=="2008") %>%
  rename(year=surveyyear, disfert1=disfert, dishybrd1=dishybrd)

lag10 <- filter(lag, surveyyear=="2010") 

lag2010 <- merge(lag10,lag08,by="DISNAME") %>%
  select(surveyyear, DISNAME, disfert1, dishybrd1)
  
lag10 <- filter(lag, surveyyear=="2010") %>%
  rename(year=surveyyear, disfert1=disfert, dishybrd1=dishybrd)

lag12 <- filter(lag, surveyyear=="2012")

lag2012 <- merge(lag10,lag12,by="DISNAME") %>%
  select(surveyyear, DISNAME, disfert1, dishybrd1)

lag <- rbind(lag2010, lag2012)
rm(lag08, lag10, lag12, lag2012, lag2010)

saveRDS(lag, file.path(dataPath, "lag"))
