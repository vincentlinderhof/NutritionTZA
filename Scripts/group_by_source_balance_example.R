rm(list=ls()) # clear environment

# setwd to wherever the income files are stored
p <- "C:/Users/Tomas/Documents/LEI/NutritionTZA/Scripts"
setwd(p)

# source all of the income variables
# and the panel key
source("income_TZA_2008.R")
source("income_TZA_2010.R")
source("income_TZA_2012.R")
source("panel_key.R")

# We only want households that are
# balanced -> had only
# one observation per year
income_2008 <- left_join(KEY, income_2008, by=c("hhid2008"="hhid")) %>%
  rename(y3_hhid = hhid2012)
income_2008$year <- "2008"

income_2010 <- left_join(KEY, income_2010, by=c("hhid2010"="y2_hhid")) %>%
  rename(y3_hhid = hhid2012)
income_2010$year <- "2010"

income_2012 <- filter(income_2012, y3_hhid %in% KEY$hhid2012)
income_2012$year <- "2012"

# keep only variables which are the same over each
# year
keep <- Reduce(intersect, list(names(income_2008),
                               names(income_2010),
                               names(income_2012)))
income_2008 <- income_2008[keep]
income_2010 <- income_2010[keep]
income_2012 <- income_2012[keep]

# stack on top of each other
income <- rbind(income_2008, income_2010, income_2012)

# make sure every household has three observations
# one for each year

x <- group_by(income, y3_hhid) %>% summarise(nobs=n())
table(x$nobs) # three obs for each household

# attach the time average for income

income_x<- group_by(income, y3_hhid) %>%
  summarise(acrop_value_hh = mean(crop_value_hh, na.rm=TRUE))
income <- left_join(income, income_x)




