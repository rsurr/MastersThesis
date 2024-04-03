occupancy <- INFORMES_IMAES_HD %>%
  left_join(imaes, by="CAIMAE") %>% 
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
    
    Plunes= sum(c(DPLU1O, DPLU2O, DPLU3O, DPLU4O), na.rm = T)/DPLUT,
    Pmartes= sum(c(DPMA1O, DPMA2O, DPMA3O, DPMA4O), na.rm = T)/DPMAT,
    Pmiercoles= sum(c(DPMI1O, DPMI2O, DPMI3O, DPMI4O), na.rm = T)/DPMIT,
    Pjueves= sum(c(DPJU1O, DPJU2O, DPJU3O, DPJU4O), na.rm = T)/DPJUT,
    Pviernes= sum(c(DPVI1O, DPVI2O, DPVI3O, DPVI4O), na.rm = T)/DPVIT,
    Psabado= sum(c(DPSA1O, DPSA2O, DPSA3O, DPSA4O), na.rm = T)/DPSAT,
    
    Plmv=mean(c(Plunes, Pmiercoles, Pviernes), na.rm = T),
    Pmjs=mean(c(Pmartes, Pjueves, Psabado), na.rm = T),
    
    slack=CAHMSPBN-mean(c(Nlmv,Nmjs)),
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
  select(Nlmv, Nmjs, Plmv, Pmjs, CAHMSPBN, slack, slack_perc, tipo_imae, fecha3, CAIMAE, ZCAIMAE, mes_solicitud)

write.csv(occupancy, 
          "C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/MastersThesis/OCCUPANCY.csv", 
          row.names=FALSE) 
