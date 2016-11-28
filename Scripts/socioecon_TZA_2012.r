#######################################
########### SOCIO/ECONOMIC ############
#######################################

#detach("dplyr")
library(dplyr)
HH12 <- read_dta(file.path(dataPath, "HH_SEC_B.dta")) %>%
  dplyr::select(hhid=y3_hhid, indidy3, status=hh_b05, sex=hh_b02,
         yob=hh_b03_1, age=hh_b04, years=hh_b26)

HH12$years <- as.numeric(HH12$years)
HH12$years <- ifelse(HH12$years %in% 99, HH12$age, HH12$years)
HH12$status <- as_factor(HH12$status)
HH12$sex <- toupper(as_factor(HH12$sex))
HH12$yob <- as.integer(HH12$yob)

ed <- read_dta(file.path(dataPath, "HH_SEC_C.dta")) %>%
  dplyr::select(hhid=y3_hhid, indidy3, ed_any=hh_c03, start=hh_c04, end=hh_c08)

HH12_ed <- left_join(HH12, ed)
rm(HH12, ed)
HH12_ed$ed_any <- toupper(as_factor(HH12_ed$ed_any))
HH12_ed$ed_any <- ifelse(HH12_ed$ed_any %in% "YES", 1, 0)

SOCIO_P <- subset(HH12_ed, select = -c(indidy3, start))

SOCIO_HH <- subset(SOCIO_P, status %in% "HEAD") 
rm(SOCIO_P)


#AV: make new variable: household size
SIZE <-
  read_dta(file.path(dataPath, "ConsumptionNPS3.dta")) %>%
    dplyr::select(hhid=y3_hhid, hhsize)
SOCIO_HH <- left_join(SOCIO_HH, SIZE); rm(SIZE)

# AV: make new variable: female headship

FHEAD <- mutate(HH12_ed, fem_head = ifelse(sex == 'FEMALE' & status == 'HEAD', 1, 0)) %>%
  group_by(hhid) %>%
  summarise(fem_head = sum(fem_head))
SOCIO_HH <- left_join(SOCIO_HH, FHEAD); rm(FHEAD)

frequencies(SOCIO_HH$status)
frequencies(SOCIO_HH$sex)
descriptive.table(vars = d(age, yob, years, ed_any, hhsize, fem_head),  
                  data= SOCIO_HH, func.names =c("Mean","St. Deviation","Valid N")) 


# AV: make a new variables with number of householdmembers in each age-cathegory
# make a new variable cage (cut age) which splits
# individuals according to their age group with
# breaks at 15, 55 and the max age

COMPOSITION <- HH12_ed %>%
  mutate(infants = ifelse(age < 6, 1, ifelse (NA))) %>%
  mutate(children = ifelse(age >5 & age < 15, 1, ifelse(NA))) %>%
  mutate(young_adults = ifelse(age >14 & age < 19, 1, ifelse(NA))) %>%
  mutate(mature_adults = ifelse(age >18 & age <65, 1, ifelse(NA))) %>%
  mutate(older_adults = ifelse(age >64, 1, ifelse(NA)))

FAM_COM <-
  dplyr::select(COMPOSITION, hhid, infants, children, young_adults,mature_adults, older_adults, sex, ed_any) %>% 
  replace(is.na(.), 0) %>%
  group_by(hhid) %>% 
  summarise(infants      = sum(infants), 
            children     = sum(children), 
            young_adults = sum(young_adults), 
            mature_adults= sum(mature_adults), 
            older_adults = sum(older_adults),
            family_size  = n( ),
            female       = sum(sex %in% "FEMALE"),
            ed_any       = sum(ed_any %in% 1) ) 

FAM_COM$hhsize2     <- FAM_COM$infants+FAM_COM$children+FAM_COM$young_adults+FAM_COM$mature_adults+FAM_COM$older_adults
FAM_COM$dependency <- (FAM_COM$infants+FAM_COM$children+FAM_COM$older_adults)/FAM_COM$hhsize2*100
FAM_COM$female_sh <- FAM_COM$female/FAM_COM$hhsize2*100

descriptive.table(vars = d(infants, children , young_adults , mature_adults, older_adults, family_size, 
                           female, ed_any, hhsize2, dependency, female_sh),  
                  data= FAM_COM, func.names =c("Mean","St. Deviation","Valid N")) 

SOCIO_HH <- left_join(SOCIO_HH, FAM_COM) 
rm(COMPOSITION, FAM_COM)

# -------------------------------------
# education of household members and sum
# of education of all household members
# between the ages of 15 and 55

# -------------------------------------
# death in the family
# -------------------------------------

death <- read_dta(file.path(dataPath, "HH_SEC_S.dta")) %>%
  dplyr::select(hhid=y3_hhid, personid) 
death$personid <- zap_empty(death$personid)
death <- transmute(death, hhid, death=ifelse(!is.na(personid), 1, 0))
death <- group_by(death, hhid) %>%
  summarize(death=sum(death))
library(Deducer)
frequencies(death$death)


# -------------------------------------
# membership to a credit group
# -------------------------------------

credit <- read_dta(file.path(dataPath, "HH_SEC_O2.dta")) %>%
  dplyr::select(hhid=y3_hhid, personid, occ)  
credit$personid <- zap_empty(credit$personid)
credit <- transmute(credit, hhid, SACCO=ifelse(!is.na(personid), 1, 0))
credit <- group_by(credit, hhid) %>%
            summarize(SACCO=sum(SACCO))
#table(credit$SACCO)
frequencies(credit$SACCO)
SOCIO_HH <- left_join(SOCIO_HH, death) 
SOCIO_HH <- left_join(SOCIO_HH, credit) 
#SOCIO_HH <- SOCIO_HH %>%
#  mutate(SACCO = ifelse(is.na(SACCO),0,SACCO)) %>%
#  mutate(death= ifelse(is.na(death), 0,death))
rm(HH12_ed, credit, death)
