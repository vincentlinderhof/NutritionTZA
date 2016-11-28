# -------------------------------------------------------
# --- THIS FILES IS SOURCED IN TZA_2010.r ---------------
# -------------------------------------------------------
#######################################
########### SOCIO/ECONOMIC ############
#######################################

HH10 <- read_dta(file.path(dataPath, "TZNPS2HH1DTA/HH_SEC_B.dta")) %>%
  select(hhid=y2_hhid, indidy2, status=hh_b05, sex=hh_b02,
         yob=hh_b03_1, age=hh_b04, years=hh_b25)

HH10$years <- as.numeric(HH10$years)
HH10$years <- ifelse(HH10$years %in% 99, HH10$age, HH10$years)
HH10$status <- as_factor(HH10$status)
HH10$sex <- toupper(as_factor(HH10$sex))
HH10$yob <- as.integer(HH10$yob)

ed <- read_dta(file.path(dataPath, "TZNPS2HH1DTA/HH_SEC_C.dta")) %>%
  select(hhid=y2_hhid, indidy2, ed_any=hh_c03, start=hh_c04, end=hh_c08)

ed$ed_any <- as_factor(ed$ed_any) # ever went to school
ed$end <- as.integer(as.character(ed$end))
ed$end <- ifelse(ed$end %in% 9999, NA, ed$end)

HH10_ed <- left_join(HH10, ed); rm(HH10)
HH10_ed$ed_any <- toupper(as_factor(HH10_ed$ed_any))
HH10_ed$ed_any <- ifelse(HH10_ed$ed_any %in% "YES", 1, 0)

SOCIO_P <- select(HH10_ed, -indidy2, -start, -end)
SOCIO_HH <- subset(SOCIO_P, status %in% "HEAD")
rm("SOCIO_P", "ed")

#AV: make new variable: household size
SIZE <-
  read_dta(file.path(dataPath, "TZNPS2HH2DTA/TZY2.HH.Consumption.dta")) %>%
  select(hhid=y2_hhid, hhsize_comp=hhsize)
SOCIO_HH <- left_join(SOCIO_HH, SIZE); rm(SIZE)

# AV: make new variable: female headship

FHEAD <- mutate(HH10_ed, fem_head = ifelse(sex == 'FEMALE' & status == 'HEAD', 1, 0)) %>%
  group_by(hhid) %>%
  summarise(fem_head = sum(fem_head))
SOCIO_HH <- left_join(SOCIO_HH, FHEAD); rm(FHEAD)

# AV: make new variables with number of householdmembers in each age-cathegory

COMPOSITION <- HH10_ed %>%
  mutate(infants       = ifelse(age < 6, 1, ifelse (NA))) %>%
  mutate(children      = ifelse(age >5 & age < 15, 1, ifelse(NA))) %>%
  mutate(young_adults  = ifelse(age >14 & age < 19, 1, ifelse(NA))) %>%
  mutate(mature_adults = ifelse(age >18 & age <65, 1, ifelse(NA))) %>%
  mutate(older_adults  = ifelse(age >64, 1, ifelse(NA)))

FAM_COM <-
  select(COMPOSITION, hhid, infants, children, young_adults,mature_adults, older_adults) %>% 
  replace(is.na(.), 0) %>%
  group_by(hhid)%>% 
  summarise(infants = sum(infants), 
            children = sum(children), 
            young_adults = sum(young_adults), 
            mature_adults = sum(mature_adults), 
            older_adults = sum(older_adults))

FAM_COM$hhsize2 <- FAM_COM$infants+FAM_COM$children+FAM_COM$young_adults+FAM_COM$mature_adults+FAM_COM$older_adults
FAM_COM$dependency <- (FAM_COM$infants+FAM_COM$children+FAM_COM$older_adults)/FAM_COM$hhsize2*100

SOCIO_HH <- left_join(SOCIO_HH, FAM_COM) 
rm(COMPOSITION, FAM_COM)

# -------------------------------------
# education of household members and sum
# of education of all household members
# between the ages of 15 and 55
# -------------------------------------

#ed$ed_any <- as_factor(ed$ed_any) # ever went to school
HH10_ed$cage <- cut(HH10_ed$age, breaks = c(0, 5, 15, 45, 65, max(HH10_ed$age, na.rm=TRUE)),
                    labels=c("0-5", "6-15", "16-45", "46-64", "65+"), include.lowest = TRUE, right = TRUE)
HH10_ed <- select(HH10_ed, -indidy2, -status, -yob, -age, -years, -start) 

HH10_ed <- HH10_ed %>%
  mutate(infants       = ifelse(cage %in%  "0-5", 1, 0)) %>%
  mutate(children      = ifelse(cage %in%  "6-14", 1, 0)) %>%
  mutate(young_adults  = ifelse(cage %in%  "15-18", 1, 0)) %>%
  mutate(mature_adults = ifelse(cage %in%  "18-64", 1, 0)) %>%
  mutate(older_adults  = ifelse(cage %in%  "65+", 1, 0))

HH10_x <- group_by(HH10_ed, hhid) %>%
  summarise(n_infants      =sum(infants),
            n_children     =sum(children),
            n_young_adults =sum(young_adults),
            n_mature_adults=sum(mature_adults),
            n_older_adults =sum(older_adults),
            family_size    =n( ),
            n_female       =sum(sex %in% "FEMALE"),
            n_ed_any       =sum(ed_any %in% 1)            ) 

SOCIO_HH <- left_join(SOCIO_HH, HH10_x) 
rm(HH10_x, HH10_ed)

detach(dplyr)
library(dplyr) 
# summarise the data: number of
# household members 15:55

# -------------------------------------
# death in the family
# -------------------------------------

death <- read_dta(file.path(dataPath, "TZNPS2HH2DTA/HH_SEC_S.dta")) %>%
  select(hhid=y2_hhid) %>% mutate(death=1) %>% unique

# -------------------------------------
# membership to a credit group
# -------------------------------------


credit <- read_dta(file.path(dataPath, "TZNPS2HH2DTA/HH_SEC_O2.dta")) %>%
  select(hhid=y2_hhid) %>% unique() %>% mutate(SACCO = 1)

SOCIO_HH <- left_join(SOCIO_HH, death) 
SOCIO_HH <- left_join(SOCIO_HH, credit) 
SOCIO_HH <- SOCIO_HH%>%
  mutate(SACCO = ifelse(is.na(SACCO),0,SACCO)) %>%
  mutate(death= ifelse(is.na(death), 0,death))
rm(ed, credit, death)
