# -------------------------------------------------------
# --- THIS FILES IS SOURCED IN TZA_2008.r ---------------
# -------------------------------------------------------
#######################################
########## TANZANIA 2008-09 ###########
#######################################
########### SOCIO/ECONOMIC ############
#######################################

HH08 <- read_dta(file.path(dataPath, "/TZNPS1HHDTA_E/SEC_B_C_D_E1_F_G1_U.dta")) %>%
  select(hhid, indidy1=sbmemno, status=sbq5, sex=sbq2,
         yob=sbq3yr, age=sbq4, years=sbq24)

HH08$years <- as.numeric(HH08$years)
HH08$years <- ifelse(HH08$years %in% 99, HH08$age, HH08$years)
HH08$status <- as_factor(HH08$status)
HH08$sex <- toupper(as_factor(HH08$sex))
HH08$yob <- as.integer(HH08$yob)


ed <- read_dta(file.path(dataPath, "/TZNPS1HHDTA_E/SEC_B_C_D_E1_F_G1_U.dta")) %>%
  select(hhid, indidy1=sbmemno, ed_any=scq2, start=sbq3yr)

HH08_ed <- left_join(HH08, ed); rm(HH08)
HH08_ed$ed_any <- toupper(as_factor(HH08_ed$ed_any))
HH08_ed$ed_any <- ifelse(HH08_ed$ed_any %in% "YES", 1, 0)

SOCIO_P <- select(HH08_ed, -indidy1, -start)
SOCIO_HH <- subset(SOCIO_P, status %in% "HEAD")
rm("SOCIO_P", "ed")


#AV: make new variable: household size
SIZE <-
  read_dta(file.path(dataPath, "TZNPS1_consdta/TZY1.HH.Consumption.dta")) %>%
  select(hhid, hhsize_comp=hhsize)
SOCIO_HH <- left_join(SOCIO_HH, SIZE); rm(SIZE)

# AV: make new variable: female headship

FHEAD <- mutate(HH08_ed, fem_head = ifelse(sex == 'FEMALE' & status == 'HEAD', 1, 0)) %>%
  group_by(hhid) %>%
  summarise(fem_head = sum(fem_head))
SOCIO_HH <- left_join(SOCIO_HH, FHEAD); rm(FHEAD)

# AV: make a new variables with number of householdmembers in each age-cathegory

COMPOSITION <- HH08_ed %>%
  mutate(infants = ifelse(age < 6, 1, ifelse (NA))) %>%
  mutate(children = ifelse(age >5 & age < 15, 1, ifelse(NA))) %>%
  mutate(young_adults = ifelse(age >14 & age < 19, 1, ifelse(NA))) %>%
  mutate(mature_adults = ifelse(age >18 & age <65, 1, ifelse(NA))) %>%
  mutate(older_adults = ifelse(age >64, 1, ifelse(NA)))

FAM_COM <-
  select(COMPOSITION, hhid, infants, children, young_adults,mature_adults, older_adults) %>% 
  replace(is.na(.), 0) %>%
  group_by(hhid) %>% 
  summarise(infants = sum(infants), 
            children = sum(children), 
            young_adults = sum(young_adults), 
            mature_adults = sum(mature_adults), 
            older_adults = sum(older_adults))

FAM_COM$hhsize2     <- FAM_COM$infants+FAM_COM$children+FAM_COM$young_adults+FAM_COM$mature_adults+FAM_COM$older_adults
FAM_COM$dependency <- (FAM_COM$infants+FAM_COM$children+FAM_COM$older_adults)/FAM_COM$hhsize2*100

SOCIO_HH <- left_join(SOCIO_HH, FAM_COM) 
rm(COMPOSITION, FAM_COM, SOCIO_P)

# -------------------------------------
# education of household members and sum
# of education of all household members
# between the ages of 15 and 55
# -------------------------------------

#ed$ed_any <- as_factor(ed$ed_any) # ever went to school
HH08_ed$cage <- cut(HH08_ed$age, breaks = c(0, 5, 15, 45, 65, max(HH08_ed$age, na.rm=TRUE)),
                    labels=c("0-5", "6-15", "16-45", "46-64", "65+"), include.lowest = TRUE, right = TRUE)
HH08_ed <- select(HH08_ed, -indidy1, -status, -yob, -age, -years, -start) 

HH08_ed <- HH08_ed %>%
  mutate(infants = ifelse(cage %in%  "0-5", 1, 0)) %>%
  mutate(children = ifelse(cage %in%  "6-14", 1, 0)) %>%
  mutate(young_adults = ifelse(cage %in%  "15-18", 1, 0)) %>%
  mutate(mature_adults = ifelse(cage %in%  "18-64", 1, 0)) %>%
  mutate(older_adults = ifelse(cage %in%  "65+", 1, 0))

HH08_x <- group_by(HH08_ed, hhid) %>%
  summarise(n_infants      =sum(infants),
            n_children     =sum(children),
            n_young_adults =sum(young_adults),
            n_mature_adults=sum(mature_adults),
            n_older_adults =sum(older_adults),
            family_size    =n( ),
            n_female       =sum(sex %in% "FEMALE"),
            n_ed_any       =sum(ed_any %in% 1)            ) 

SOCIO_HH <- left_join(SOCIO_HH, HH08_x) 
rm(HH08_x, HH08_ed, ed)

# -------------------------------------
# death in the family
# -------------------------------------

death <- read_dta(file.path(dataPath, "TZNPS1HHDTA_E/SEC_S2.dta")) %>%
  select(hhid) %>% mutate(death=1) %>% unique

# -------------------------------------
# membership to a credit group
# -------------------------------------

credit <- read_dta(file.path(dataPath, "TZNPS1HHDTA_E/SEC_O3.dta")) %>%
  select(hhid) %>% unique() %>% mutate(SACCO = 1)

SOCIO_HH <- left_join(SOCIO_HH, death) 
SOCIO_HH <- left_join(SOCIO_HH, credit) 

rm(credit, death)


