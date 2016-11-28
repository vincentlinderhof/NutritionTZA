
if(Sys.info()["user"] == "Tomas"){
  dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA/2010/Data" }
if(Sys.info()["user"] == "linde069"){
  dataPath <- "D:/Analyses/CIMMYT/TZA_Anne/Data"
  setwd("D:/Analyses/CIMMYT/TZA_Anne")}

#Anne
#dataPath <- "C:/Users/afver/OneDrive/Documenten/Removable Disk/verbe038/TZA"
#setwd("C:/Users/afver/OneDrive/Documenten/Removable Disk/a.Thesis/Outputs")

library(haven)
library(stringr)
library(reshape2)
library(dplyr)
library(stargazer)
library(plm)
library(systemfit)
library(pglm)
library(Deducer)

source("Multiple observation detection-VL.r")

#Data2010_2012maize <-readRDS(file.path(dataPath, "Data2010_2012maize")) 
fullData <-readRDS(file.path(dataPath, "fullData")) 
all_maize <-readRDS(file.path(dataPath, "all_maize")) 

Data2010_2012maize$tmp_surveyyear <- as.factor(Data2010_2012maize$surveyyear)

b <- c("2010", "2012") 
table(b, Data2010_2012maize@tmp_surveyyear)
rm(tmp_surveyyear)

adopth <- descriptive.table(vars = d(hybrd1, fert1 , fem_head , ed_any, years, hhsize , dependency , hqi, income/100000, 
                                     area_tot  , asset/100000 , 
                                     TLU , SACCO , avgpPrecip , avgTemp , dist2town , 
                                     dist2market, subseed1, subfert1, DDS, FVS),  
                            data= Data2010_2012maize, func.names =c("Mean","St. Deviation","Valid N")) 

adopth
adopth <- descriptive.table(vars = d(hybrd1, fert1 , fem_head , ed_any, years, hhsize , dependency , hqi, income/100000, 
                                     area_tot  , asset/100000 , 
                                     TLU , SACCO , avgpPrecip , avgTemp , dist2town , dist2market, 
                                     subseed1, subfert1, DDS, FVS), strata=d(hybrd1), 
                            data= Data2010_2012maize, func.names =c("Mean","St. Deviation","Valid N"))
adopth


#VL
#Data2010_2012maize$hhsize <- Data2010_2012maize$infants+Data2010_2012maize$children+Data2010_2012maize$young_adults+Data2010_2012maize$mature_adults+Data2010_2012maize$older_adults
#Data2010_2012maize$dependency <- (Data2010_2012maize$infants+Data2010_2012maize$children+Data2010_2012maize$older_adults)/Data2010_2012maize$hhsize

Data2010_2012maize <- select(Data2010_2012maize, hhid2008, hhid2010, hhid2012, surveyyear, status, hybrd1, fert1 , fem_head , ed_any , years, hhsize , dependency ,
                             income, off_farm_income_hh, area_tot  , asset , hqi ,
                             TLU , SACCO , ZONE, central, eastern, lake, northern, southern, southern_highlands, western, zanzibar, 
                             avgpPrecip , avgTemp , dist2town , dist2market, DDS, FVS, FCS, CSI, subseed1, subfert1, dishybrd1, disfert1, m)
Data2010_2012maize_un <- unique(Data2010_2012maize)

all_maize <- unique(all_maize)
frequencies(all_maize, all_maize$ZONE)
table(all_maize$ZONE)
table(all_maize$ZONE, all_maize$rural)

table(Data2010_2012maize$ZONE)
table(Data2010_2012maize$ZONE, Data2010_2012maize$surveyyear)
table(Data2010_2012maize$ZONE, Data2010_2012maize$rural)

table(Data2010_2012$hybrd1, Data2010_2012$surveyyear)
table(Data2010_2012$fert1,  Data2010_2012$surveyyear)
table(Data2010_2012$hybrd1, Data2010_2012$fert1,  Data2010_2012$surveyyear)
table(Data2010_2012$subseed1, Data2010_2012$surveyyear)
table(Data2010_2012$subfert1,  Data2010_2012$surveyyear)
table(Data2010_2012$hybrd1, Data2010_2012$subseed1,  Data2010_2012$surveyyear) 
table(Data2010_2012$fert1,  Data2010_2012$subfert1,  Data2010_2012$surveyyear) 

table(Data2010_2012maize$fem_head, Data2010_2012maize$surveyyear) 



table(Data2010_2012maize$hhsize, Data2010_2012maize$surveyyear) 
table(Data2010_2012maize$ed_any, Data2010_2012maize$surveyyear) 
table(Data2010_2012maize$SACCO, Data2010_2012maize$surveyyear) 
 
descriptive.table(vars = d(DDS, FVS, FCS, CSI, fem_head , ed_any , years, hhsize , dependency,
                           hqi, income/100000, off_farm_income_hh/100000, area_tot  , asset/100000, TLU , SACCO , central, eastern, lake,
                           northern, southern, southern_highlands, western, zanzibar, 
                           avgpPrecip , avgTemp , dist2town , dist2market, hybrd1, fert1, subseed1, subfert1),
                          data= Data2010_2012maize,
                  strata = surveyyear,
                          func.names =c("Mean","St. Deviation", "Min", "Max", "Skew", "Valid N"))
descriptive.table(vars = d(DDS, FVS, FCS, CSI, fem_head , ed_any , years, hhsize , dependency,
                           hqi, income/100000, off_farm_income_hh/100000, area_tot  , asset/100000, TLU , SACCO , central, eastern, lake,
                           northern, southern, southern_highlands, western, zanzibar, 
                           avgpPrecip , avgTemp , dist2town , dist2market, hybrd1, fert1, subseed1, subfert1),
                  data= Data2010_2012,
                  strata = surveyyear,
                  func.names =c("Mean","St. Deviation", "Min", "Max", "Skew", "Valid N"))

Data2010_2012maize$TLU_original <- Data2010_2012maize$TLU
Data2010_2012maize$TLU <- ifelse(is.na(Data2010_2012maize$TLU),0,Data2010_2012maize$TLU)

Data2010_2012maize$d_hqi <- ifelse(is.na(Data2010_2012maize$hqi),0,1)
table(Data2010_2012maize$d_hqi, Data2010_2012maize$surveyyear)
#REsult: in 2010, all cases show positive number on  hqi, while in 2012 a lot of missings!

Data2010_2012maize$d_no_income <- ifelse((Data2010_2012maize$income)==0,1,0)
table(Data2010_2012maize$d_no_income, Data2010_2012maize$surveyyear)

# calculation of correlation coefficent of DDS and FVS
myvars <- c  ("fem_head", "ed_any", "hhsize", "dependency", "income", "off_farm_income_hh", "area_tot", "asset",
              "TLU", "hqi", "death", "SACCO", "avgpPrecip", "avgTemp", "dist2town", "dist2market", "hybrd1", "fert1", "DDS", "FVS",
              "FCS", "CSI", "surveyyear") 
Data2010_2012maize_sub <- Data2010_2012maize[myvars]
Data2010_maize_sub <- subset(Data2010_2012maize_sub, surveyyear==2010)
Data2012_maize_sub <- subset(Data2010_2012maize_sub, surveyyear==2012)
Cor_matrix <- cor(Data2010_2012maize_sub, use="complete.obs", method="pearson")

Cor_matrix_10 <- cor(Data2010_maize_sub, use="complete.obs", method="pearson")
Cor_matrix_12 <- cor(Data2012_maize_sub, use="complete.obs", method="pearson")

stargazer(Cor_matrix, Cor_matrix_10, Cor_matrix_12, out="Results/final_cor_matrix.htm") 

rm(Data2010_2012maize_sub, myvars)



# sort by mpg and cyl
newdata <- Data2010_2012maize[order(hhid2012, surveyyear),]
Data2010_2012maize$obs_count <- ifelse(Data2010_2012maize$hhid2012==lag(Data2010_2012maize$hhid2012),1,0)


descriptive.table(vars = d(TLU , TLU_original),
                  data= Data2010_2012maize,
                  strata = surveyyear,
                  func.names =c("Mean","St. Deviation", "Min", "Max", "Skew", "Valid N"))

descriptive.table(vars = d(TLU , TLU_original),
                  data= Data2010_2012maize,
                  strata = surveyyear,
                  func.names =c("Mean","St. Deviation", "Min", "Max", "Skew", "Valid N"))

Data2010_2012maize
DDS_TLU_original <- lm(DDS ~ hybrd1 + fert1 + hybrd1*fert1 + fem_head + ed_any+ hhsize + dependency + 
                             log(income+1)+ log(off_farm_income_hh+1) + log(area_tot)  + log(asset) + 
                             log(TLU_original+1)+ hqi + SACCO + factor(ZONE) + avgpPrecip + avgTemp + 
                             dist2town + dist2market+factor(surveyyear), 
                       data=Data2010_2012maize)

DDS_TLU <- lm(DDS ~ hybrd1 + fert1 + hybrd1*fert1 + fem_head + ed_any+ hhsize + dependency + log(income+1)+ log(off_farm_income_hh+1) +
                log(area_tot)  + log(asset) + log(TLU+1)+ hqi +
                SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear), data=Data2010_2012maize)

Data2010_2012maize$one <- 1

DDS_OLS_0 <- lm(DDS ~ one, 
                data=Data2010_2012maize)
DDS_OLS_1 <- lm(DDS ~ factor(ZONE) + factor(surveyyear), 
                data=Data2010_2012maize)
DDS_OLS_2 <- lm(DDS ~ hybrd1 + fert1 + fem_head + ed_any+ hhsize + dependency + log(income+1)+ log(off_farm_income_hh+1) +
                   log(area_tot)  + log(asset) + log(TLU+1)+ hqi +
                   SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear), data=Data2010_2012maize)
DDS_OLS_2i <- lm(DDS ~ hybrd1 + fert1 + hybrd1*fert1 + fem_head + ed_any+ hhsize + dependency + log(income+1)+ log(off_farm_income_hh+1) +
            log(area_tot)  + log(asset) + log(TLU+1)+ hqi +
            SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear), data=Data2010_2012maize)
stargazer(DDS_OLS_0, DDS_OLS_1, DDS_OLS_2, DDS_OLS_2i, out="Results/final_DDS_OLS.htm") 


DDS_test1a <- lm(DDS ~ hybrd1 + fert1 + hybrd1*fert1 + fem_head + ed_any+ hhsize + dependency + log(income+1)
          + log(off_farm_income_hh+1) +
               log(area_tot)  + log(asset) + log(TLU+1)+ hqi +
               SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear),
          data=Data2010_2012maize)
DDS_test2a <- lm(DDS ~ hybrd1 + fert1 + hybrd1*fert1 + fem_head + ed_any+ hhsize + dependency + log(income+1)
          + log(off_farm_income_hh+1) +
            log(area_tot)  + log(asset) + log(TLU+1)+ hqi +
            SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear), 
          data=Data2010_2012)

Data2010_2012maize$TLU <- ifelse(is.na(Data2010_2012maize$TLU),0,Data2010_2012maize$TLU)
Data2010_2012$TLU <- ifelse(is.na(Data2010_2012$TLU),0,Data2010_2012$TLU)


DDS_test1b <- lm(DDS ~ hybrd1 + fert1 + hybrd1*fert1 + fem_head + ed_any+ hhsize + dependency + log(income+1)
                + log(off_farm_income_hh+1) +
                  log(area_tot)  + log(asset) + log(TLU+1)+ hqi +
                  SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear),
                data=Data2010_2012maize)
DDS_test2b <- lm(DDS ~ hybrd1 + fert1 + hybrd1*fert1 + fem_head + ed_any+ hhsize + dependency + log(income+1)
                + log(off_farm_income_hh+1) +
                  log(area_tot)  + log(asset) + log(TLU+1)+ hqi +
                  SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear), 
                data=Data2010_2012)
stargazer(DDS_test1a, DDS_test1b, DDS_test2a, DDS_test2b, type="text", out="Results/FNS_OLS_test.txt") 
stargazer(DDS_test1a, DDS_test1b, DDS_test2a, DDS_test2b, type="html", out="Results/FNS_OLS_test.html") 


FVS <- lm(FVS ~ hybrd1 + fert1 + hybrd1*fert1 +fem_head + ed_any+ hhsize + dependency + log(income+1)+ log(off_farm_income_hh+1) +
            log(area_tot)  + log(asset) + log(TLU+1)+ hqi +
            SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear), data=Data2010_2012maize)

FCS <- lm(FCS ~ hybrd1 + fert1 + hybrd1*fert1 + fem_head + ed_any+ hhsize + dependency + log(income+1)+ log(off_farm_income_hh+1) +
            log(area_tot)  + log(asset) + log(TLU+1)+ hqi +
            SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear), data=Data2010_2012maize)

CSI <- lm(CSI ~ hybrd1 + fert1 + hybrd1*fert1+fem_head + ed_any+ hhsize + dependency + log(income+1)+ log(off_farm_income_hh+1) +
            log(area_tot)  + log(asset) + log(TLU+1)+ hqi +
            SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear), data=Data2010_2012maize)

stargazer(DDS, FVS, FCS, CSI, type="text", out="Results/FNS_OLS.txt") 

stargazer(DDS, FVS, FCS, CSI, out="Results/final.htm") 
stargazer(DDS_TLU, DDS_TLU_original, out="Results/final_TLU.htm") 

rm(DDS, FVS, FCS, CSI)
             
#install.packages("aod")
library(aod)
pr_hybrd_2 <- glm(hybrd1 ~ fem_head + ed_any+ hhsize + dependency + log(income+1) + log(off_farm_income_hh+1) +
                    log(area_tot)  + log(asset) + log(TLU+1)+ hqi +
                    SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear)
                  , family=binomial(link="probit"), data=Data2010_2012maize)
wald.test(b=coef(pr_hybrd_2), Sigma=vcov(pr_hybrd_2), Terms=4:6)

pr_fert_2 <- glm(fert1 ~ fem_head + ed_any+ hhsize + dependency + log(income+1) + log(off_farm_income_hh+1) +
                    log(area_tot)  + log(asset) + log(TLU+1)+ hqi +
                    SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market+factor(surveyyear)
                  , family=binomial(link="probit"), data=Data2010_2012maize)
wald.test(b=coef(pr_fert_2), Sigma=vcov(pr_fert_2), Terms=4:6)

xtabs(~ hybrd1 + fert1, data = Data2010_2012maize)

#install.packages("Zelig")
library(Zelig)
Pr_biv_1 <- zelig(list(mu1=hybrd1 ~ one,
                       mu2=fert1 ~ one,
                       rho = ~1),
                  model="bprobit", data = Data2010_2012maize)

Pr_biv_2 <- zelig(list(mu1=hybrd1 ~ fem_head + ed_any + hhsize + dependency + log(income+1) + log(off_farm_income_hh+1) +
                         log(area_tot)  + log(asset) + log(TLU+1) + hqi +
                         SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market + factor(surveyyear),
                       mu2=fert1 ~ fem_head + ed_any + hhsize + dependency + log(income+1) + log(off_farm_income_hh+1) +
                         log(area_tot)  + log(asset) + log(TLU+1) + hqi +
                         SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market + factor(surveyyear),
                   rho = ~1),
                   model="bprobit",data = Data2010_2012maize)
                  
install.packages("SemiParBIVProbit")
library(SemiParBIVProbit)

pr_biv_2 <- SemiParBIVProbit(list(hybrd1 ~ fem_head + ed_any + hhsize + dependency + log(income+1) + log(off_farm_income_hh+1) +
                               log(area_tot)  + log(asset) + log(TLU+1) + hqi +
                               SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market + factor(surveyyear),
                             fert1 ~ fem_head + ed_any + hhsize + dependency + log(income+1) + log(off_farm_income_hh+1) +
                               log(area_tot)  + log(asset) + log(TLU+1) + hqi +
                               SACCO + factor(ZONE) + avgpPrecip + avgTemp + dist2town + dist2market + factor(surveyyear)),
                        data = Data2010_2012maize) 
conv.check(pr_biv_2) 
summary(pr_biv_2) 
AIC(pr_biv_2) 
BIC(pr_biv_2) 
                  
stargazer(pr_hybrd_2, pr_fert_2, out="Results/final_probits.htm") 
write.csv(pr_biv_2, "final_biprobits.txt")

stargazer(pr_biv_2, out="Results/final_biprobits.htm") 



