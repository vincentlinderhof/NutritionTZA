# AV: make new variables: HQI Housing Quality Index

house<- read_dta(file.path(dataPath, "/HH_SEC_I.dta")) %>%
  dplyr::select(hhid=y3_hhid, tenure=hh_i01, wall=hh_i08,
         roof=hh_i09, cook=hh_i16, light=hh_i17, A=hh_i19, B=hh_i22, C=hh_i12)

house$tenure <- as.integer(house$tenure)
house$wall <- as.integer(house$wall)
house$roof <- as.integer(house$roof)
house$cook <- as.integer(house$cook)
house$light <- as.integer(house$light)
house$A <- as.integer(house$A)
house$B <- as.integer(house$B)
house$C <- as.integer(house$C)

house<-mutate(house,drink1=ifelse(A=='1', 1, 
                             ifelse(A=='2', 2, 
                               ifelse(A =='3', 3, 
                                 ifelse(A=='4', 4,
                                   ifelse(A=='5', 5,
                                     ifelse(A=='6', 5,
                                       ifelse(A=='7', 6,
                                         ifelse(A=='8', 7, 
                                           ifelse(A=='9', 7, 
                                             ifelse(A=='10', 8,
                                               ifelse(A=='11', 8,
                                                 ifelse(A=='12', 9,
                                                   ifelse(A=='13', 10, 
                                                     ifelse(A=='14', 11, NA)))))))))))))))

house<-mutate(house,drink2=ifelse(B=='1', 1, 
                             ifelse(B=='2', 2, 
                               ifelse(B =='3', 3, 
                                 ifelse(B=='4', 4,
                                   ifelse(B=='5', 5,
                                     ifelse(B=='6', 5,
                                       ifelse(B=='7', 6,
                                         ifelse(B=='8', 7, 
                                           ifelse(B=='9', 7, 
                                             ifelse(B=='10', 8,
                                               ifelse(B=='11', 8,
                                                 ifelse(B=='12', 9,
                                                   ifelse(B=='13', 10, 
                                                     ifelse(B=='14', 11, NA)))))))))))))))
house <-mutate(house, sew= ifelse(C=='1', 5,
                             ifelse(C=='2', 1,
                               ifelse(C==3, 1,
                                 ifelse(C==4, 2,
                                   ifelse(C==5, 4,
                                     ifelse(C==6, 3,
                                       ifelse(C==7, 3,
                                         ifelse(C==8, 4, NA)))))))))

myvars <- names(house) %in% c("A", "B", "C")
house <- house[!myvars]
rm(myvars)

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
                                ifelse(sew=='VIP', 2, 0)))))


HQI$hqi <- HQI$tenure1/3+HQI$wall1/6+HQI$roof1/4+HQI$cook1/6+HQI$light1/4+HQI$drink11/7+HQI$sew1/4

saveRDS(HQI, "Data/HQI2012.rds")

myvars <- c("hhid", "hqi")
HQI <- HQI[myvars]
rm(myvars, house)
