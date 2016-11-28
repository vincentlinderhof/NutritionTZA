#######################################
########## TANZANIA 2008-09 ###########
#######################################
####### Housing quality index #########
#######################################

# AV: make new variables: HQI Housing Quality Index
house<- read_dta(file.path(dataPath, "TZNPS1HHDTA_E/SEC_H1_J_K2_O2_P1_Q1_S1.dta")) %>%
  select(hhid, tenure=sjq1, wall=sjq4,
         roof=sjq5, cook=sjq17_1, light=sjq18, drink1=sjq8, drink2=sjq11, sew=sjq16)

house$tenure <- factor(house$tenure, levels=1:6, labels=c('own', 'empl_sub', 'empl_free', 'rent', 'free', 'nomad'))
house$wall <- factor(house$wall, levels=1:7, labels=c('poles', 'poles_mud_stone', 'mud', 'mud_brick', 'brick', 'concrete', 'other'))
house$roof <- factor(house$roof, levels=1:7, labels=c('grass_leaves', 'mud_grass', 'concrete', 'CGI', 'asbestos', 'tiles', 'other'))
house$cook <- factor(house$cook, levels=1:8, labels=c('firewood', 'paraffin', 'electricity', 'gas', 'charcoal', 'animal_residual', 'biogas', 'other'))
house$light <- factor(house$light, levels=1:9, labels=c('electricity', 'solar', 'gas', 'biogas', 'lampoil', 'candle', 'firewood', 'priv_generator', 'other'))
house$drink1 <- factor(house$drink1, levels=1:11, labels=c('pipe_inside', 'pipe_outside', 'tap_public', 'neighbour', 'vendor', 'truck_tanker', 'well_pump', 'well_nopump', 'surface', 'rain', 'other'))
house$drink2 <- factor(house$drink2, levels=1:11, labels=c('pipe_inside', 'pipe_outside', 'tap_public', 'neighbour', 'vendor', 'truck_tanker', 'well_pump', 'well_nopump', 'surface', 'rain', 'other'))
house$sew <- factor(house$sew, levels=1:5, labels=c('no_toilet', 'flush_toilet', 'pit_latrine', 'VIP', 'other'))

HQI <- house
HQI <- mutate(HQI, tenure1 = ifelse(tenure=='own', 3, 
                                    ifelse(tenure=='empl_sub'|tenure=='empl_free'|tenure=='rent', 2,
                                      ifelse(tenure=='free', 1, 
                                        ifelse(tenure=='nomad', 0, 0)))))
HQI <- mutate(HQI, wall1 = ifelse(wall=='poles', 1,
                                  ifelse(wall=='poles_mud_stone', 2,
                                    ifelse(wall=='mud', 3,
                                      ifelse(wall=='mud_brick', 4,
                                        ifelse(wall=='brick', 5,
                                          ifelse(wall=='concrete', 6, 
                                            ifelse(wall=='other', 3, 0))))))))
HQI <- mutate(HQI, roof1 = ifelse(roof=='grass_leaves', 1,
                                  ifelse(roof=='mud_grass'|roof=='CGI'|roof=='other', 2,
                                    ifelse(roof=='concrete'|roof=='tiles', 4,
                                      ifelse(roof=='asbestos', 3, 0)))))

HQI <- mutate(HQI, cook1 = ifelse(cook=='firewood', 1,
                                  ifelse(cook=='paraffin'|cook=='biogas', 4,
                                    ifelse(cook=='electricity', 6,
                                      ifelse(cook=='gas', 5,
                                        ifelse(cook=='charcoal'|cook=='other', 3,
                                          ifelse(cook=='animal_residual', 2, 0)))))))


HQI <- mutate(HQI, light1 = ifelse(light=='electricity'|light=='priv_generator', 4,
                                   ifelse(light=='solar'|light=='gas'|light=='biogas', 3,
                                     ifelse(light=='lampoil'| light=='other', 2,
                                       ifelse(light=='candle'|light=='firewood', 1,0)))))

HQI <- mutate(HQI, drink11 = ifelse(drink1=='pipe_inside', 7,
                                    ifelse(drink1=='pipe_outside', 6,
                                      ifelse(drink1=='tap_public', 5,
                                        ifelse(drink1=='neighbour'|drink1=='surface'|drink1=='rain', 2,
                                          ifelse(drink1=='vendor'|drink1=='truck_tanker', 1,
                                            ifelse(drink1=='well_pump'|drink1=='other', 4, 
                                              ifelse(drink1=='well_nopump', 3, 0))))))))

HQI <- mutate(HQI, sew1 = ifelse(sew=='no_toilet', 1,
                                 ifelse(sew=='flush_toilet', 4,
                                   ifelse(sew=='pit_latrine'|sew=='other', 2, 
                                     ifelse(sew=='VIP', 3, 0)))))

HQI$hqi <- HQI$tenure1/3+HQI$wall1/6+HQI$roof1/4+HQI$cook1/6+HQI$light1/4+HQI$drink11/7+HQI$sew1/4
saveRDS(HQI, file.path(dataPath, "/../../HQI2008.rds"))

#Select the HQI indicator and remove all individual housing items
HQI <- select(HQI, hhid, hqi)
house <- left_join(house, HQI)
rm(house)
#HH08 <- left_join(HH08, house)
