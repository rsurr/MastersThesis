mensual <- MENSUALES_HD %>% 
  unite("fecha2", PMD_ANIO:PMD_MES, sep="-") %>% 
  mutate(fecha3=as.Date(paste(fecha2, "-01", sep=""))) %>% 
  group_by(ZPMD_IMAE, fecha3) %>% summarize(n=n())

mensual2 <- MENSUALES_HD %>% 
  unite("fecha2", PMD_ANIO:PMD_MES, sep="-") %>% 
  mutate(fecha3=as.Date(paste(fecha2, "-01", sep=""))) %>% 
  group_by(ZPMD_IMAE, fecha3) %>% summarize(n_capacnum=n_distinct(CAPACNUM))

sesional <- SESIONES_HD %>% 
  unite("fecha2", PMD_ANIO:PMD_MES, sep="-") %>% 
  mutate(fecha3=as.Date(paste(fecha2, "-01", sep=""))) %>% 
  group_by(ZPMD_IMAE, fecha3) %>% summarize(n_capacnum=n_distinct(CAPACNUM))

occupancy <- SESIONES_HD %>% 
  unite("fecha2", PMD_ANIO:PMD_MES, sep="-") %>% 
  mutate(fecha3=as.Date(paste(fecha2, "-01", sep=""))) %>% 
  group_by(ZPMD_IMAE, fecha3, CAPACNUM) %>% 
  summarize(CAIMAE=first(PMD_IMAE), n=n()) %>% 
  filter(n>1) %>% 
  ungroup() %>%
  group_by(ZPMD_IMAE, fecha3) %>%
  summarize(CAIMAE=first(CAIMAE), n_capacnum=n_distinct(CAPACNUM))

occupancy <- SESIONES_HD %>% 
  unite("fecha2", PMD_ANIO:PMD_MES, sep="-") %>% 
  mutate(fecha3=as.Date(paste(fecha2, "-01", sep=""))) %>% 
  group_by(ZPMD_IMAE, fecha3, CAPACNUM) %>% 
  summarize(CAIMAE=first(PMD_IMAE), n=n()) %>% 
  filter(n>1) %>% 
  ungroup() %>%
  group_by(ZPMD_IMAE, fecha3) %>%
  summarize(CAIMAE=first(CAIMAE), n_capacnum=n_distinct(CAPACNUM))

summary(occupancy$n_capacnum)

capacity <- INFORMES_IMAES_HD %>% 
  unite("fecha2", CAANIO:CAMES, sep="-") %>% 
  mutate(fecha3=as.Date(paste(fecha2, "-01", sep=""))) %>% 
  select(CAIMAE, CAHMSPBN, CAHMSPBP, fecha3, DTOTALP, DNPUSO, DPPUSO)

occupancy2 <- occupancy %>% left_join(capacity, by=c("fecha3", "CAIMAE")) %>% mutate(slack=DTOTALP-n_capacnum,
                                                                                     slack2=if_else(slack<0, 0, slack))

occupancy2$slack2 %>% hist()

occupancy2 %>% ungroup() %>% select(-c(ZPMD_IMAE, fecha3, CAIMAE, ZPMD_IMAE)) %>% cor(use="complete.obs")

imae <- INGRESOS_HD %>% group_by(CAIMAE) %>% summarise(CAIMAE=first(CAIMAE),
                                                       ZCAIMAE=first(ZCAIMAE))

congestion <- INFORMES_IMAES_HD %>%
  left_join(imae, by="CAIMAE") %>% 
  left_join(IMAE_num, by="ZCAIMAE") %>% 
  rowwise() %>% 
  mutate(
    Nlunes= sum(c(DNLU1O, DNLU2O, DNLU3O, DNLU4O), na.rm = T)/DNLUT,
    Nmartes= sum(c(DNMA1O, DNMA2O, DNMA3O, DNMA4O), na.rm = T)/DNMAT,
    Nmiercoles= sum(c(DNMI1O, DNMI2O, DNMI3O, DNMI4O), na.rm = T)/DNMIT,
    Njueves= sum(c(DNJU1O, DNJU2O, DNJU3O, DNJU4O), na.rm = T)/DNJUT,
    Nviernes= sum(c(DNVI1O, DNVI2O, DNVI3O, DNVI4O), na.rm = T)/DNVIT,
    Nsabado= sum(c(DNSA1O, DNSA2O, DNSA3O, DNSA4O), na.rm = T)/DNSAT,
    
    Nlmv=mean(c(Nlunes, Nmiercoles, Nviernes), na.rm = T),
    Nmjs=mean(c(Nmartes, Njueves, Nsabado), na.rm = T),
    Nmean=mean(c(Nlmv, Nmjs), na.rm = T),
    
    
    Plunes= sum(c(DPLU1O, DPLU2O, DPLU3O, DPLU4O), na.rm = T)/DPLUT,
    Pmartes= sum(c(DPMA1O, DPMA2O, DPMA3O, DPMA4O), na.rm = T)/DPMAT,
    Pmiercoles= sum(c(DPMI1O, DPMI2O, DPMI3O, DPMI4O), na.rm = T)/DPMIT,
    Pjueves= sum(c(DPJU1O, DPJU2O, DPJU3O, DPJU4O), na.rm = T)/DPJUT,
    Pviernes= sum(c(DPVI1O, DPVI2O, DPVI3O, DPVI4O), na.rm = T)/DPVIT,
    Psabado= sum(c(DPSA1O, DPSA2O, DPSA3O, DPSA4O), na.rm = T)/DPSAT,
    
    Plmv=mean(c(Plunes, Pmiercoles, Pviernes), na.rm = T),
    Pmjs=mean(c(Pmartes, Pjueves, Psabado), na.rm = T),
    
    slack=CAHMSPBN-Nmean,
    slack_perc=slack/CAHMSPBN*100) %>% 
  
  unite("fecha2", CAANIO:CAMES, sep="-") %>% 
  mutate(fecha3=as.Date(paste(fecha2, "-01", sep="")),
         mes_solicitud=format(fecha3, "%Y-%m")) %>% 
  mutate(
    tipo_imae=
      case_when(ZCAIMAE %in% c(IMAE_CENEU, IMAE_NEPHROS, IMAE_DIAVERUM, IMAE_SARI) ~ "INDEPENDIENTE",
                ZCAIMAE %in% IMAE_PUBLICO ~ "PUBLICO",
                ZCAIMAE %in% IMAE_PRIVADO ~ "PRIVADO",
                TRUE ~ NA)) %>% 
  select(
    Nlmv, Nmjs, Plmv, Pmjs, Nmean, CAHMSPBN, slack, slack_perc, 
    tipo_imae, fecha3, CAIMAE, ZCAIMAE, mes_solicitud, depto) %>% 
  filter(depto=="01", # Filter for facilities in Montevideo
         ZCAIMAE!="SENNIAD HEMO",
         Nmean!=0) %>% #Not pediatric
  left_join(IMAE_num, by=c("ZPMD_IMAE"="ZCAIMAE")) %>% 
  filter(depto=="01") %>% 
  ungroup() %>% 
  mutate(ZCAIMAE=as.factor(choice)) %>% 
  select(occ_var, occ_var_prop, ZCAIMAE, fecha)
  
a <- left_join(congestion, INGRESOS_HD2, by=c("mes_solicitud", "ZCAIMAE"))

descom <- a %>% filter(SCDESU=="S")
no_descom <- a %>% filter(SCDESU=="N")

summary(descom$slack)
summary(no_descom$slack)
