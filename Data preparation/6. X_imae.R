library(readr)
library(tidyverse)
library(haven)
library(fastDummies)
library(data.table)
library(zoo)

IMAE_CENEU <- c("CE.DI.SA.", "CENEPA", "CANIMEL", "SEDIC", "INU", "URUGUAYANA")

IMAE_NEPHROS <- c("NEPHROS", "UDIR", "SANEF")

IMAE_DIAVERUM <- c("CETER", "COMECA", "CRANI 33", "CRANI COSTA DE ORO", 
                   "CRANI MINAS", "INTIR", "RENIS", "SEINE", "SENNIAD", "UNEDI",
                   "SENNIAD HEMO")

IMAE_PUBLICO <- c("HOSPITAL MACIEL", "HOSPITAL DE CLINICAS")

IMAE_PRIVADO <- c("ASOCIACION ESPAÑOLA", "SMI - SERVICIO MEDICO INTEGRAL",
                  "S.M.Q. SALTO", "COMEPA", "COMEF", "CASMU", "CASA DE GALICIA",
                  "UNIVERSAL", "A.M. DE SAN JOSE", "CAMEC", "CAMEDUR-CENICA", "CAMOC", 
                  "CANMU", "CENDIME", "COMEF", "COMEPA", "COMERO",
                  "GREMEDA", "HOSPITAL BRITANICO", "HOSPITAL EVANGELICO",
                  "SANATORIO AMERICANO", "HOSPITAL ITALIANO")

IMAE_SARI <- "SARI"

INST_ASSE <- c("HOSPITAL DE CLINICAS", "H.MACIEL", "ASSE GRATUITO", "ASSE-PREPAGO", 
               "H.PEREIRA ROSSELL", "H.SAINT BOIS", "HOSPITAL ESPAÑOL", "C.D. CANELONES", 
               "C.A. LAS PIEDRAS", "C.A. PANDO", "C.A. DE CIUDAD DEL PLATA",
               "C.A. CIUDAD DE LA COSTA", "R.A.P. (S.A.E.)", "H.VILARDEBO", 
               "C.A. RINCON DE LA BOLSA", "C.A.DIARIOS Y REVISTAS", "C.D. RIVERA", 
               "C.D. TREINTA Y TRES", "H.PASTEUR", "C.D. FLORIDA")

INST_IAMCIAMPP <- c("ASOCIACION ESPAÑOLA", "CASMU - IAMPP", "CASA DE GALICIA", "CIRCULO CATOLICO",
                    "EVANGELICO","IMPASA", "MUCAM", "SMI - SERVICIO MEDICO INTEGRAL", "GREMCA", 
                    "SISTEMA NOTARIAL DE SALUD*", "COMEF IAMPP", "UNIVERSAL", "COSEM IAMPP", "CUDAM", 
                    "CAAMEPA IAMPP", "COMUE *", "CRAMI IAMPP", "AMECOM IAMPP", "CAMEC IAMPP", 
                    "AMEDRIN IAMPP", "COMTA IAMPP", "AFITYC***", "CRAME IAMPP", "S.M.Q. SALTO", "MUCAM")

INST_CORPORATIVO <- c("H.POLICIAL", "ANCAP***", "ANV (BHU)***")

INST_SEGUROPRIVADO <- c("COPAMHI MEDICARE", "CIMA ESPAÑA*", "MP MEDICINA PERSONALIZADA",
                        "I.Q. SUDAMERICANO", "HOSPITAL BRITANICO", "SUMMUM FONASA", "BLUE CROSS FONASA")

PAC_INACTIVO <- c("Jubilado", "Menor dependiente (< 15 años)", "Sin ocupación")

PAC_ACTIVO <- c("Artesano", "Empleado comercio", "Empleado rural", "Empleado servicio", 
                "Estudiante", "Funcionario público", "Otros", "Profesional liberal",
                "Propietario agricultura", "Propietario de comercio", "Propietario industrial",
                "Propietario servicio", "Tareas del hogar")

# INGRESOS_HD ----
INGRESOS_HD <- 
  read_sav("C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/Databases/INGRESOS HD.sav") %>% 
  #filter(ZCASDEPAR=="MONTEVIDEO") %>%
  select(-c(
    APNEFROD0, APNEFROD1,
    APCARDD0, APCARDD1, APCARDD2, APCARDD3, APCARDD4,
    APRESPD0, APRESPD1,
    APNEURD0, APNEURD1, APNEURD2,
    APDIGED0, APDIGED1,
    APINMUD0, APINMUD1,
    APEMETD0, APEMETD1,
    APOTROD0, APOTROD1, APOTROD2,
    APHEMAD0, APHEMAD1,
    APNEOPD, ZAPNEOPD,
    SCEFPYM0, SCEFPYM1,
    SCEFBF0, SCEFBF1,
    
  )) %>% 
  rename(
    ante=APERSONA,
    
    ante_nefro=APNEFRO,
    ante_nefro_str0=ZAPNEFROD0,
    ante_nefro_str1=ZAPNEFROD1,
    ante_nefro_irc_fecha=APIRCF,
    ante_nefro_1control_fecha=APPCNFEC,
    ante_nefro_creatinina=APPCNCRE,
    
    ante_cardio=APCARD,
    ante_cardio_str0=ZAPCARDD0,
    ante_cardio_str1=ZAPCARDD1,
    ante_cardio_str2=ZAPCARDD2,
    ante_cardio_str3=ZAPCARDD3,
    ante_cardio_str4=ZAPCARDD4,
    
    ante_respi=APRESP,
    ante_respi_str0=ZAPRESPD0,
    ante_respi_str1=ZAPRESPD1,
    
    ante_neuro=APNEUR,
    ante_neuro_str0=ZAPNEURD0,
    ante_neuro_str1=ZAPNEURD1,
    ante_neuro_str2=ZAPNEURD2,
    
    ante_dige=APDIGE,
    ante_dige_str0=ZAPDIGED0,
    ante_dige_str1=ZAPDIGED1,
    
    ante_inmu=APINMU,
    ante_inmu_str0=ZAPINMUD0,
    ante_inmu_str1=ZAPINMUD1,
    
    ante_meta=APEMET,
    ante_meta_str0=ZAPEMETD0,
    ante_meta_str1=ZAPEMETD1,
    
    ante_otro=APOTRO,
    ante_otro_str0=ZAPOTROD0, 
    ante_otro_str1=ZAPOTROD1,
    ante_otro_str2=ZAPOTROD2,
    
    ante_hema=APHEMA,
    ante_hema_str0=ZAPHEMAD0, 
    ante_hema_str1=ZAPHEMAD1,
    
    ante_neo=APNEOP,
    ante_neo_str0=APNEOPDE,
    
    descom=SCDESU,
    descom_fecha=SCDESUF,
    coord=SCINGC,
    coord_fecha=SCINGF,
    coord_str=SCINGOB,
    
    exa_concien=SCEFCON,
    exa_piel0=ZSCEFPYM0,
    exa_piel1=ZSCEFPYM1,
    exa_nutri=SCEFVN,
    exa_buco0=ZSCEFBF0,
    exa_buco1=ZSCEFBF1,
    exa_linfo=SCEFLG,
    
    exa_cardio=SCEFCV,
    exa_cardio_ritmo=SCEFCVR,
    exa_cardio_frec=SCEFCVF,
    exa_cardio_soplos=SCEFCVS,
    exa_cardio_galope=SCEFCVG,
    exa_cardio_roces=SCEFCVO,
    exa_cardio_pulsosperi=SCEFCVP,
    exa_cardio_varices=SCEFCVV,
    exa_cardio_trombo=SCEFCVT,
    exa_cardio_presion=SCEFCVA,
    exa_cardio_pad=SCEFCVD,
    
    exa_pleuro=SCEFPP,
    exa_abdo=SCEFAB,
    exa_nervio=SCEFSN,
    exa_osteo=SCEFOA,
    exa_peso=SCEFPE,
    exa_altura=SCEFTA,
    exa_capacidad=SCDCF,
    
    med=EMEDIC,
    med_epo=EMERI,
    med_hierro_vo=EMHIEVO,
    med_hierro_iv=EMHIEIV,
    med_ieca=EMIECA,
    med_araii=EMARAII,
    med_esta=EMESTA,
    med_antia_pla=EMAPLA,
    med_calcio=EMCAL,
    med_capto_fos=EMCFOS,
    med_vitd=EMVITD,
    
    estu_azoemia=EAZOV,
    estu_azoemia_fecha=EAZOF,
    estu_creatinemia=ECREAV,
    estu_creatinemia_fecha=ECREAF,
    estu_cicreatina=ECIV,
    estu_cicreatina_fecha=ECIF,
    estu_iono_na=EIONONA,
    estu_iono_k=EIONOK,
    estu_iono_ca=EIONOCA,
    estu_iono_fecha=EIONOF
  ) %>% 
  mutate(
    ZCAIMAE=if_else(ZCAIMAE=="HOSPITAL ITALIANO", "UNIVERSAL", ZCAIMAE),
    CAIMAE=if_else(CAIMAE==63, 95, CAIMAE),
    ZCASINST=if_else(ZCASINST=="HOSPITAL ITALIANO", "UNIVERSAL", ZCASINST),
    chain=
      case_when(ZCAIMAE %in% IMAE_DIAVERUM ~ "DIAVERUM",
                ZCAIMAE %in% IMAE_CENEU ~ "CENEU",
                ZCAIMAE %in% IMAE_NEPHROS ~ "NEPHROS",
                ZCAIMAE %in% IMAE_SARI ~ "SARI",
                ZCAIMAE %in% IMAE_PUBLICO ~ "PUBLICO",
                ZCAIMAE %in% IMAE_PRIVADO ~ "PRIVADO",
                TRUE ~ NA),
    tipo_inst=
      case_when(ZCASINST %in% INST_ASSE ~ "ASSE",
                ZCASINST %in% INST_IAMCIAMPP ~ "IAMC/IAMPP",
                ZCASINST %in% INST_CORPORATIVO ~ "CORPORATIVO",
                ZCASINST %in% INST_SEGUROPRIVADO ~ "SEGURO PRIVADO",
                TRUE ~ NA),
    ZCASINST=if_else(tipo_inst=="ASSE", "ASSE", ZCASINST), #RENOMBRO INSTITUCION A ASSE
    tipo_imae=
      case_when(ZCAIMAE %in% c(IMAE_CENEU, IMAE_NEPHROS, IMAE_DIAVERUM, IMAE_SARI) ~ "INDEPENDIENTE",
                ZCAIMAE %in% IMAE_PUBLICO ~ "PUBLICO",
                ZCAIMAE %in% IMAE_PRIVADO ~ "PRIVADO",
                TRUE ~ NA),
    tiene_imae=
      case_when(ZCASINST=="ASOCIACION ESPAÑOLA" ~ 1,
                ZCASINST=="SMI - SERVICIO MEDICO INTEGRAL" ~ 1,
                ZCASINST=="HOSPITAL BRITANICO" ~ 1,
                ZCASINST=="S.M.Q. SALTO" ~ 1,
                ZCASINST=="COMEPA" ~ 1,
                ZCASINST=="COMEF IAMPP" ~ 1,
                ZCASINST=="CASMU - IAMPP" ~ 1,
                ZCASINST=="CASA DE GALICIA" ~ 1,
                ZCASINST=="COSEM IAMPP" ~ 1,
                ZCASINST=="UNIVERSAL" ~ 1,
                ZCASINST=="HOSPITAL ITALIANO" ~ 1,
                ZCASINST=="ASSE" ~ 1,
                ZCASINST=="MUCAM" ~ 1,
                TRUE ~ 0),
    imae_inst=
      case_when(ZCAIMAE=="ASOCIACION ESPAÑOLA" & ZCASINST=="ASOCIACION ESPAÑOLA" ~ 1,
                ZCAIMAE=="SMI - SERVICIO MEDICO INTEGRAL" & ZCASINST=="SMI - SERVICIO MEDICO INTEGRAL" ~ 1,
                ZCAIMAE=="HOSPITAL BRITANICO" & ZCASINST=="HOSPITAL BRITANICO" ~ 1,
                ZCAIMAE=="S.M.Q. SALTO" & ZCASINST=="S.M.Q. SALTO" ~ 1,
                ZCAIMAE=="COMEPA" & ZCASINST=="COMEPA" ~ 1,
                ZCAIMAE=="COMEF" & ZCASINST=="COMEF IAMPP" ~ 1,
                ZCAIMAE=="CASMU" & ZCASINST=="CASMU - IAMPP" ~ 1,
                ZCAIMAE=="CASA DE GALICIA" & ZCASINST=="CASA DE GALICIA" ~ 1,
                ZCAIMAE=="NEPHROS" & ZCASINST=="COSEM IAMPP" ~ 1,
                
                ZCAIMAE=="UNIVERSAL" & ZCASINST=="UNIVERSAL" ~ 1,
                ZCAIMAE=="HOSPITAL ITALIANO" & ZCASINST=="UNIVERSAL" ~ 1,
                
                ZCAIMAE =="HOSPITAL DE CLINICAS" & ZCASINST=="ASSE" ~ 1,
                ZCAIMAE =="HOSPITAL MACIEL" & ZCASINST=="ASSE" ~ 1,
                ZCAIMAE =="CANMU" & ZCASINST=="MUCAM" ~ 1,
                TRUE ~ 0),
    transp=
      case_when(ZCAIMAE %in% c(IMAE_CENEU, IMAE_NEPHROS, IMAE_DIAVERUM) ~ 1,
                ZCAIMAE %in% IMAE_PUBLICO ~ 1,
                ZCAIMAE == "ASOCIACION ESPAÑOLA" ~ 1,
                ZCAIMAE == "SMI - SERVICIO MEDICO INTEGRAL" ~ 1,
                ZCAIMAE == "CASMU" ~ 1,
                ZCAIMAE == "CASA DE GALICIA" ~ 1,
                ZCAIMAE == "ASOCIACION ESPAÑOLA" ~ 1,
                ZCAIMAE == "HOSPITAL EVANGELICO" ~ 1,
                ZCAIMAE == "CANMU" ~ 0,
                ZCAIMAE == "HOSPITAL BRITANICO" ~ 0,
                ZCAIMAE == "UNIVERSAL" ~ 0,
                ZCAIMAE == "HOSPITAL ITALIANO" ~ 0,
                TRUE ~ NA),
    tipo_pac=
      case_when(ZB1SOCUP0 %in% PAC_ACTIVO ~ "Activo",
                ZB1SOCUP0 %in% PAC_INACTIVO ~ "Inactivo",
                TRUE ~ NA),
    fecha_solicitud=as.Date(CAFECSOL),
    fecha_autorizacion=as.Date(CAFECAUT),
    mes_solicitud=format(fecha_solicitud, "%Y-%m"),
    mes_autorizacion=format(fecha_autorizacion, "%Y-%m"),
    anio_solicitud=as.double(format(fecha_solicitud, "%Y")),
    anio_autorizacion=format(fecha_autorizacion, "%Y")) %>% 
  group_by(CAPACNUM) %>% 
  slice_max(CASEDADA, with_ties = FALSE)

# INFORMES_IMAES_HD ----
INFORMES_IMAES_HD <- read_sav("~/Proyecto Tesis/Databases/INFORMES_IMAES_HD.sav") %>% 
  unite("fecha2", CAANIO:CAMES, sep="-") %>% 
  mutate(fecha3=as.Date(paste(fecha2, "-01", sep="")),
         mes_solicitud=format(fecha3, "%Y-%m"))

# IMAE_num ----
IMAE_num <- read_csv("IMAE_num.csv")

# Congestion ----
congestion <- INFORMES_IMAES_HD %>%
  rowwise() %>% 
  mutate(
    #Promedio pacientes por turno en cada dia
    suma_lunes= sum(c(DNLU1O, DNLU2O, DNLU3O, DNLU4O), na.rm = T),
    Nlunes= suma_lunes/DNLUT,
    
    suma_martes=sum(c(DNMA1O, DNMA2O, DNMA3O, DNMA4O), na.rm = T),
    Nmartes= suma_martes/DNMAT,
    
    suma_miercoles=sum(c(DNMI1O, DNMI2O, DNMI3O, DNMI4O), na.rm = T),
    Nmiercoles= suma_miercoles/DNMIT,
    
    suma_jueves= sum(c(DNJU1O, DNJU2O, DNJU3O, DNJU4O), na.rm = T),
    Njueves= suma_jueves/DNJUT,
    
    suma_viernes=sum(c(DNVI1O, DNVI2O, DNVI3O, DNVI4O), na.rm = T),
    Nviernes=suma_viernes/DNVIT,
    
    suma_sabado=sum(c(DNSA1O, DNSA2O, DNSA3O, DNSA4O), na.rm = T),
    Nsabado= suma_sabado/DNSAT,
    
    #Suma por conjunto de dias
    sumaLMV=sum(c(suma_lunes, suma_miercoles, suma_viernes), na.rm = T),
    sumaMJS=sum(c(suma_martes, suma_jueves, suma_sabado), na.rm = T),
    promedio_suma=mean(c(sumaLMV, sumaMJS), na.rm=T),
    
    #Promedio conjunto de dias
    Nlmv=mean(c(Nlunes, Nmiercoles, Nviernes), na.rm = T),
    Nmjs=mean(c(Nmartes, Njueves, Nsabado), na.rm = T),
    
    Nmean=mean(c(Nlmv, Nmjs), na.rm = T),
    promedio_turnos_semana=
      mean(c(DNLUT, DNMIT, DNVIT), na.rm = T) +
      mean(c(DNMAT, DNJUT, DNSAT), na.rm = T),
    
    slack=CAHMSPBN-Nmean,
    slack_perc=slack/CAHMSPBN*100,
    
    slack2=
      if_else(slack>=10,
              (CAHMSPBN/promedio_turnos_semana)-Nmean, slack),
    slack_perc2=slack2/CAHMSPBN*100) %>% 
  select(CAHMSPBN, slack2, slack_perc2, 
         CAIMAE, mes_solicitud)

# Imae_inst ----
imae_inst <- 
  matrix(c("ASOCIACION ESPAÑOLA", "ASOCIACION ESPAÑOLA", "SMI - SERVICIO MEDICO INTEGRAL",
           "SMI - SERVICIO MEDICO INTEGRAL", "HOSPITAL BRITANICO", "HOSPITAL BRITANICO",
           "S.M.Q. SALTO", "S.M.Q. SALTO", "COMEPA", "COMEPA", "COMEF", "COMEF IAMPP",
           "CASMU", "CASMU - IAMPP", "CASA DE GALICIA", "CASA DE GALICIA", "NEPHROS",
           "COSEM IAMPP", "UNIVERSAL", "UNIVERSAL",
           "HOSPITAL DE CLINICAS", "ASSE", "HOSPITAL MACIEL", "ASSE", "CANMU", "MUCAM"),
         ncol=2, byrow=T) %>% as.data.frame()

colnames(imae_inst) <- c("ZCAIMAE", "ZCASINST_IMAE")

# Turnos ----
turnos <- INFORMES_IMAES_HD %>%
  group_by(CAIMAE, mes_solicitud) %>% 
  summarise(turnosLMV=mean(c(DNLUT, DNMIT, DNVIT), na.rm=T),
            turnosMJS=mean(c(DNMAT, DNJUT, DNSAT), na.rm=T)) %>%
  rowwise() %>% 
  mutate(turnos=mean(c(turnosLMV, turnosMJS), na.rm = T))

# Occupancy ----

#SESIONES_HD <- read_sav("C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/Databases/SESIONES #HD.sav") 

SESIONES_HD <- SESIONES_HD %>% 
  unite("fecha", PMD_ANIO:PMD_MES, sep="-", remove=FALSE) %>% 
  mutate(Date = as.Date(paste(fecha, "-01", sep="")))

SESIONES_HD <- SESIONES_HD %>% 
  mutate(
    ZPMD_IMAE=if_else(ZPMD_IMAE=="HOSPITAL ITALIANO", "UNIVERSAL", ZPMD_IMAE))

occupancy <- SESIONES_HD %>% select(ZPMD_IMAE, PMD_ANIO, PMD_MES, CAPACNUM, Date) %>%
  group_by(ZPMD_IMAE, Date) %>% 
  summarise(n=n(), Date=first(Date), PMD_ANIO=first(PMD_ANIO)) %>% 
  ungroup() %>% 
  group_by(ZPMD_IMAE) %>% 
  arrange(Date) %>%
  mutate(moving_avg_n = rollmean(n, k = 12, fill = "extend", align = "center")) %>% 
  ungroup() %>% 
  mutate(
         dif=n-moving_avg_n,
         dif_prop=dif/moving_avg_n) %>% 
  rename(occ_var=dif,
         occ_var_prop=dif_prop,
         ZCAIMAE=ZPMD_IMAE) %>% 
  mutate(mes_solicitud=format(Date, "%Y-%m")) %>% 
  select(occ_var, occ_var_prop, ZCAIMAE, mes_solicitud)
  
# Medimae ----

MEDICOS <- SESIONES_HD %>%
  left_join(IMAE_num, by=c("ZPMD_IMAE"="ZCAIMAE")) %>% # Column with facility number
  rename(medimae=id) %>% # Name facility number column "medimae"
  filter(depto=="01", # Filter for facilities in Montevideo
         ZPMD_IMAE!="SENNIAD HEMO") %>% #Not pediatric
  group_by(ZB1RMEDICO, medimae, PMD_ANIO) %>% 
  summarise(n=n()) %>% # Get the number of sessions of doctors in each facility (per year)
  mutate(medimae=ifelse(medimae=="", NA, medimae), # Put NA if facility number blank
         ZB1RMEDICO=ifelse(ZB1RMEDICO=="" | # Put NA if doctor name blank
                             ZB1RMEDICO=="  No corresponde" |
                             ZB1RMEDICO=="   No corresponde",
                           NA, ZB1RMEDICO)) %>% 
  filter(!is.na(medimae), !is.na(ZB1RMEDICO)) %>% # Drop facility or doctor NA observations
  group_by(medimae, PMD_ANIO) %>% 
  dummy_cols(select_columns = "medimae") %>% # Dummies per facility (and one observation per facility-doctor-year)
  group_by(ZB1RMEDICO, PMD_ANIO) %>% 
  summarise_at(vars(starts_with("medimae")), funs(.= max(.))) %>% # Collapse to one observation per doctor-year
  select(-"medimae_.")

names(MEDICOS) <- gsub("[._]", "", names(MEDICOS))  # Remove "_" and "."

MEDICOS <- MEDICOS %>%
  mutate(across(starts_with("medimae"), ~ifelse(. == 1, as.integer(sub("medimae", "", cur_column())), .)))

# Delta peso ----

#dP <- SESIONES_HD %>%
#  rename(ZCAIMAE=ZPMD_IMAE) %>% 
#  mutate(DPESOSE=case_when(DPESOSE==999.00 ~ NA,
#                           .default = as.numeric(DPESOSE)),
#         DPESOPO=case_when(DPESOPO==999.00 ~ NA,
#                           .default = as.numeric(DPESOPO)),
#         dife_post=DPESOPO-DPESOSE,
#         dife_pre=DPESOPR-DPESOSE,
#         peso=case_when(abs(dife_post)<=0.5 ~ 1,
#                        abs(dife_post)>0.5 ~ 0,
#                        is.na(dife_post) ~ NA)) %>% 
#  select(peso, CAPACNUM, DPESOPR, DPESOPO, ZCAIMAE, PMD_ANIO, dife_post, 
#         dife_pre)
#
## Reorder "anio" in increasing order
#dP$anio <- factor(dP$PMD_ANIO, levels = sort(unique(dP$PMD_ANIO)))
#
## Reorder "ZCAIMAE" alphabetically
#dP$ZCAIMAE <- factor(dP$ZCAIMAE, levels = sort(unique(dP$ZCAIMAE)))
#
## Set reference levels for anio, ZCAIMAE and CAPACNUM
#base$anio <- relevel(base$anio, ref = "2004")
#base$ZCAIMAE <- relevel(base$ZCAIMAE, ref = "ASOCIACION ESPAÑOLA")
#base$CAPACNUM <- relevel(base$CAPACNUM, ref="344677")
#
#dP <- dP %>% unite("ZCAIMAEanio", ZCAIMAE:PMD_ANIO, sep=":", remove = FALSE) 

#m_dP1 <- feols(peso ~ DPESOPR | ZCAIMAEanio + CAPACNUM, dP)
#m_dP2 <- feols(peso ~ dife_pre | ZCAIMAEanio + CAPACNUM, dP)
#m_dP3 <- feols(dife_post ~ DPESOPR | ZCAIMAEanio + CAPACNUM, dP)
#m_dP4 <- feols(dife_post ~ dife_pre | ZCAIMAEanio + CAPACNUM, dP)
#m_dP5 <- feols(DPESOPO ~ DPESOPR | ZCAIMAEanio + CAPACNUM, dP)
#
#delta_p1 <- fixef(m_dP1)$ZCAIMAEanio %>% as.data.frame() %>% 
#  rownames_to_column("ZCAIMAEanio")
#colnames(delta_p1) <- c("ZCAIMAEanio", "delta_p1")
#
#delta_p2 <- fixef(m_dP2)$ZCAIMAEanio %>% as.data.frame() %>% 
#  rownames_to_column("ZCAIMAEanio")
#colnames(delta_p2) <- c("ZCAIMAEanio", "delta_p2")

#delta_p3 <- fixef(m_dP3)$ZCAIMAEanio %>% as.data.frame() %>% 
#  rownames_to_column("ZCAIMAEanio")
#colnames(delta_p3) <- c("ZCAIMAEanio", "delta_p3")
#
#delta_p4 <- fixef(m_dP4)$ZCAIMAEanio %>% as.data.frame() %>% 
#  rownames_to_column("ZCAIMAEanio")
#colnames(delta_p4) <- c("ZCAIMAEanio", "delta_p4")
#
#delta_p5 <- fixef(m_dP5)$ZCAIMAEanio %>% as.data.frame() %>% 
#  rownames_to_column("ZCAIMAEanio")
#colnames(delta_p5) <- c("ZCAIMAEanio", "delta_p5")

#delta_p <- delta_p1 %>% 
#  left_join(delta_p2, join_by("ZCAIMAEanio")) %>%
#  left_join(delta_p3, join_by("ZCAIMAEanio")) %>%
#  left_join(delta_p4, join_by("ZCAIMAEanio")) %>%
#  left_join(delta_p5, join_by("ZCAIMAEanio")) %>%
#  separate(ZCAIMAEanio, sep = ":", into = c("ZCAIMAE", "anio")) %>% 
#  mutate(anio=as.double(anio))


# Dist ----

GEO <- read_csv("GEO.csv")

dist <- GEO %>% 
  filter(!is.na(dist18))  %>% # FILTRO PROVISORIO SACANDO PACIENTES CON DIST NA (60 OBS)
  filter(!is.na(long_Google), !is.na(lat_Google),
         lat_Google<(-34.7),
         long_Google<(-56)) %>% 
  select(CAPACNUM, starts_with("dist")) %>% 
  pivot_longer(cols = starts_with("dist"), names_to = "names", values_to = "dist") %>% 
  separate(col = names, into = c("borrar", "id"), sep = "t") %>% select(-borrar) %>% 
  mutate(id=as.double(id)) %>% 
  left_join(IMAE_num, by="id") %>% select(-ZCAIMAE) %>% 
  filter(depto=="01") %>% mutate(num_choice=as.double(num_choice))


# Dist_sim ----

GEO_sim <- read_csv("GEO - Simulated.csv")

dist_sim <- GEO_sim  %>% 
  filter(!is.na(dist18))  %>% # FILTRO PROVISORIO SACANDO PACIENTES CON DIST NA (60 OBS)
  filter(!is.na(long_Google), !is.na(lat_Google),
         lat_Google<(-34.7),
         long_Google<(-56)) %>% 
  select(CAPACNUM, starts_with("dist")) %>% 
  pivot_longer(cols = starts_with("dist"), names_to = "names", values_to = "dist_sim") %>% 
  separate(col = names, into = c("borrar", "id"), sep = "t") %>% select(-borrar) %>% 
  mutate(id=as.double(id)) %>% 
  left_join(IMAE_num, by="id") %>%  select(-ZCAIMAE) %>% 
  filter(depto=="01")


# Delays ----

delays <- INGRESOS_HD %>% 
  select(CAPACNUM, CAFECAUT, EBESTADF, ZEBESTAD, CAFECSOL, ZCAIMAE, anio_solicitud) %>%
  mutate(delays = as.numeric(difftime(CAFECAUT, CAFECSOL, units = "days"))) %>% 
  group_by(ZCAIMAE, anio_solicitud) %>% 
  summarise(delays=mean(delays, na.rm = T))

# Habilitados_def ----

MENSUALES_HD <- read_sav("~/Proyecto Tesis/Databases/MENSUALES HD.sav")

imaes <- INGRESOS_HD %>%
  left_join(IMAE_num, join_by("ZCAIMAE")) %>% 
  filter(depto=="01",
         ZCAIMAE!="SENNIAD HEMO") %>% 
  group_by(CAIMAE) %>% 
  summarise(CAIMAE=first(CAIMAE),
            ZCAIMAE=first(ZCAIMAE),
            chain=first(chain),
            transp=first(transp),
            tipo_imae=first(tipo_imae),
            num_choice=first(num_choice),
            id=first(id),
            depto=first(depto)) %>% 
  rename(ID_CAIMAE=num_choice)

pacientes_mensuales <- MENSUALES_HD %>% 
  unite("fecha2", PMD_ANIO:PMD_MES, sep="-", remove=F) %>% 
  mutate(fecha3=as.Date(paste(fecha2, "-01", sep=""))) %>%
  mutate(
    ZPMD_IMAE=if_else(ZPMD_IMAE=="HOSPITAL ITALIANO", "UNIVERSAL", ZPMD_IMAE),
    PMD_IMAE=if_else(PMD_IMAE==63, 95, PMD_IMAE)) %>% 
  filter(PMD_ORIG=="Del centro") %>% 
  rename(CAIMAE=PMD_IMAE) %>% 
  group_by(CAIMAE, fecha3) %>% 
  summarize(n_capacnum=n_distinct(CAPACNUM)) %>% 
  group_by(CAIMAE) %>% 
  arrange(fecha3) %>%
  mutate(moving_avg_n = rollmean(n_capacnum, k = 12, fill = "extend", align = "center")) %>%
  ungroup() %>% 
  group_by(CAIMAE) %>% 
  mutate(mean_imae=mean(n_capacnum, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(
    dif=n_capacnum-moving_avg_n,
    dif_prop=dif/moving_avg_n,
    imae_dif=n_capacnum-mean_imae,
    imae_dif_prop=imae_dif/moving_avg_n) %>% 
  rename(pac_var=dif,
         pac_var_prop=dif_prop,
         imae_pac_var=imae_dif,
         imae_pac_var_prop=imae_dif_prop)

capacity <- INFORMES_IMAES_HD %>% 
  mutate(
    CAIMAE=if_else(CAIMAE==63, 95, CAIMAE)) %>% 
  mutate(fecha3=as.Date(paste(fecha2, "-01", sep=""))) %>% 
  select(CAIMAE, CAHMSPBN, CAHMSPBP, fecha3, DNLUT, DNMAT)

pacmens_capacity <- pacientes_mensuales %>% 
  left_join(capacity, by=c("fecha3", "CAIMAE")) %>% 
  left_join(imaes, by=c("CAIMAE")) %>% 
  #filter(depto=="01",
  #       ZCAIMAE!="SENNIAD HEMO") %>% 
  mutate(habilitados=CAHMSPBN*(DNLUT+DNMAT),
         slack_hab=habilitados-n_capacnum,
         slack_CAH=CAHMSPBN-n_capacnum,
         habilitados_def=if_else(abs(slack_hab)<abs(slack_CAH),
                                 habilitados, CAHMSPBN),
         slack_def=habilitados_def-n_capacnum,
         mes_solicitud=format(fecha3, "%Y-%m"),
         ID_CAIMAE=as.character(CAIMAE))  %>%
  mutate(tipo_imae2=case_when(tipo_imae=="INDEPENDIENTE" ~ "Indep",
                              tipo_imae=="PRIVADO" ~ "Priv Ins",
                              tipo_imae=="PUBLICO" ~ "Pub Ins"),
         tipo_imae3=case_when(tipo_imae2!="Pub Ins" ~ tipo_imae2,
                              ZCAIMAE=="HOSPITAL DE CLINICAS" ~ "Clinicas",
                              ZCAIMAE=="HOSPITAL MACIEL" ~ "Maciel")) %>% 
  ungroup() %>% 
  select(ID_CAIMAE, mes_solicitud, habilitados_def, slack_def, 
         n_capacnum, pac_var, pac_var_prop,
         imae_pac_var, imae_pac_var_prop,
         tipo_imae3, tipo_imae2, fecha3)

# Quality ----

my.max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)

MENSUALES_HD <- read_sav("~/Proyecto Tesis/Databases/MENSUALES HD.sav")

#SESIONES_HD <- read_sav("~/Proyecto Tesis/Databases/SESIONES HD.sav")

PACIENTES <- 
  read_sav("C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/Databases/PACIENTES.sav") %>% 
  select(CAPACNUM, PAC_SEXO, PAC_FEC_NAC)

#dife_peso <- SESIONES_HD %>% 
#  mutate(DPESOSE=case_when(DPESOSE==999.00 ~ NA,
#                           .default = as.numeric(DPESOSE)),
#         DPESOPO=case_when(DPESOPO==999.00 ~ NA,
#                           .default = as.numeric(DPESOPO)),
#         dife=DPESOPO-DPESOSE) %>% 
#  group_by(PMD_ANIO, PMD_MES, CAPACNUM) %>% 
#  summarize(dife=mean(dife, na.rm=T)) %>% 
#  mutate(peso=case_when(abs(dife)<=0.5 ~ 1,
#                        abs(dife)>0.5 ~ 0,
#                        is.na(dife) ~ NA))

MENSUALES_HD2 <- MENSUALES_HD %>% 
  right_join(INGRESOS_HD, by="CAPACNUM") %>% 
  left_join(PACIENTES, by="CAPACNUM") %>%
  #left_join(dist, by=c("CAPACNUM", "PMD_IMAE"="num_choice")) %>%
  #left_join(dife_peso, by=c("PMD_ANIO", "PMD_MES", "CAPACNUM")) %>% 
  select(CAPACNUM, ZPMD_IMAE, DDIAB, DCISQ, DEVP, PMD_ANIO, DTRABA, DFUMA, PMD_CANTBICA, 
         PMD_ANIO, PMD_MES, tipo_inst, PAC_SEXO, CASEDADA, 
         ESAZOPR1, ESAZOPO1, ESAZOPR2, EMAZOEM, ESFOSF, EMHEMOG, ZPMD_ESTADO, 
         COMP, COMP1, COMP2, COMP3, ESKTV,
         descom, coord, 
         PMD_IMAE,
         #peso,
         #dist, depto,
         PAC_FEC_NAC, fecha_solicitud, fecha_autorizacion) %>% 
  rename(ZCAIMAE=ZPMD_IMAE) %>% 
  unite("mes_mensual", c(PMD_ANIO,PMD_MES), sep="-", remove=FALSE) %>% 
  mutate(fecha_mensual = as.Date(paste(mes_mensual, "-01", sep=""))) %>% 
  filter(ZCAIMAE!="SENNIAD HEMO",
         ZCAIMAE!="IMAE A CONFIRMAR") %>% 
  mutate(
    edad = as.numeric(difftime(fecha_mensual, PAC_FEC_NAC, 
                               units = "days") / 365.25),
    ZCAIMAE=if_else(ZCAIMAE=="HOSPITAL ITALIANO", "UNIVERSAL", ZCAIMAE),
    #Date = as.Date(paste(fecha, "-01", sep="")),
    #mes=format(Date, "%Y-%m"),
    DDIAB=case_when(
      DDIAB=="S" ~ 1,
      DDIAB=="N" ~ 0,
      DDIAB=="D" ~ as.numeric(NA),
      .default = as.numeric(NA)),
    DCISQ=case_when(
      DCISQ=="S" ~ 1,
      DCISQ=="N" ~ 0,
      DCISQ=="D" ~ as.numeric(NA),
      .default = as.numeric(NA)),
    DEVP=case_when(
      DEVP=="S" ~ 1,
      DEVP=="N" ~ 0,
      DEVP=="D" ~ as.numeric(NA),
      .default = as.numeric(NA)),
    DTRABA=case_when(
      DTRABA=="S" ~ 1,
      DTRABA=="N" ~ 0,
      DTRABA=="D" ~ as.numeric(NA),
      .default = as.numeric(NA)),
    DFUMA=case_when(
      DFUMA=="S" ~ 1,
      DFUMA=="N" ~ 0,
      DFUMA=="D" ~ as.numeric(NA),
      .default = as.numeric(NA)),
    ASSE=case_when(
      tipo_inst=="ASSE" ~ 1,
      .default = 0),
    IAMC_IAMPP=case_when(
      tipo_inst=="IAMC/IAMPP" ~ 1,
      .default = 0),
    CORPORATIVO=case_when(
      tipo_inst=="CORPORATIVO" ~ 1,
      .default = 0),
    PRIVADO=case_when(
      tipo_inst=="SEGURO PRIVADO" ~ 1,
      .default = 0),
    mujer=case_when(
      PAC_SEXO=="F" ~ 1,
      .default = 0),
    descom=case_when(
      descom=="S" ~ 1,
      descom=="N" ~ 0,
      descom=="D" ~ 0),
    coord=case_when(
      coord=="S" ~ 1,
      coord=="N" ~ 0,
      .default = 0),
    URR=if_else(!is.na(ESAZOPR1), 
                (ESAZOPR1-ESAZOPO1)/ESAZOPR1,
                (ESAZOPR2-ESAZOPO1)/ESAZOPR2),
    URR65=case_when(URR>=0.65 ~ 1,
                    URR<0.65 ~ 0),
    urea17=case_when(EMAZOEM<1.7 ~ 1,
                     EMAZOEM>=1.7 ~ 0),
    fosf55=case_when(ESFOSF<5.5 ~ 1,
                     ESFOSF>=5.5 ~ 0),
    hemo10=case_when(EMHEMOG>=10 ~ 1,
                     EMHEMOG<10 ~ 0),
    surv=case_when(ZPMD_ESTADO!="FALLECIMIENTO" ~ 1,
                   ZPMD_ESTADO=="FALLECIMIENTO" ~ 0),
    comp=case_when(COMP=="S" ~ 0,
                   COMP=="N" ~ 1,
                   COMP=="D" | COMP=="D" | COMP==NA ~ NA),
    sept=case_when(COMP1 %in% c(184010162, 184010183, 184010159, 184010160, 184010161) |
                     COMP2 %in% c(184010162, 184010183, 184010159, 184010160, 184010161) |
                     COMP3 %in% c(184010162, 184010183, 184010159, 184010160, 184010161) ~ 0,
                   COMP=="N" ~ 1,
                   COMP=="D" | COMP=="D" | COMP==NA ~ NA),
    ktv12=case_when(ESKTV>=1.2 ~ 1,
                    ESKTV<1.2 ~ 0)
  ) %>%
  group_by(CAPACNUM, PMD_ANIO) %>% 
  mutate(
    DDIAB=my.max(DDIAB),
    DCISQ=my.max(DCISQ),
    DEVP=my.max(DEVP),
    DTRABA=my.max(DTRABA),
    DFUMA=my.max(DFUMA),
  ) %>%
  ungroup() %>% 
  group_by(CAPACNUM) %>% 
  arrange(PMD_ANIO, PMD_MES) %>% 
  mutate(meses=row_number())

# Reorder "anio" in increasing order
MENSUALES_HD2$anio <- factor(MENSUALES_HD2$PMD_ANIO, levels = sort(unique(MENSUALES_HD2$PMD_ANIO)))

# Reorder "ZCAIMAE" alphabetically
MENSUALES_HD2$ZCAIMAE <- factor(MENSUALES_HD2$ZCAIMAE, levels = sort(unique(MENSUALES_HD2$ZCAIMAE)))

# Set reference levels for anio, ZCAIMAE and CAPACNUM
MENSUALES_HD2$anio <- relevel(MENSUALES_HD2$anio, ref = "2004")
MENSUALES_HD2$ZCAIMAE <- relevel(MENSUALES_HD2$ZCAIMAE, ref = "ASOCIACION ESPAÑOLA")
MENSUALES_HD2$CAPACNUM <- relevel(as.factor(MENSUALES_HD2$CAPACNUM), ref="344677")

m_URR <- lm(URR65 ~ - 1 + 
              CASEDADA + meses + PMD_CANTBICA + descom +
              mujer + 
              DDIAB + DCISQ + DEVP + DTRABA + DFUMA +
              IAMC_IAMPP + PRIVADO + CORPORATIVO +
              ZCAIMAE:anio, 
            data=MENSUALES_HD2)

m_urea <- lm(urea17 ~ - 1 + 
               CASEDADA + meses + PMD_CANTBICA + descom +
               mujer + 
               DDIAB + DCISQ + DEVP + DTRABA + DFUMA +
               IAMC_IAMPP + PRIVADO + CORPORATIVO +
               ZCAIMAE:anio, 
             data=MENSUALES_HD2)

m_fosf <- lm(fosf55 ~ - 1 + 
               CASEDADA + meses + PMD_CANTBICA + descom +
               mujer + 
               DDIAB + DCISQ + DEVP + DTRABA + DFUMA +
               IAMC_IAMPP + PRIVADO + CORPORATIVO +
               ZCAIMAE:anio, 
             data=MENSUALES_HD2)

m_hemo <- lm(hemo10 ~ - 1 + 
               CASEDADA + meses + PMD_CANTBICA + descom +
               mujer + 
               DDIAB + DCISQ + DEVP + DTRABA + DFUMA +
               IAMC_IAMPP + PRIVADO + CORPORATIVO +
               ZCAIMAE:anio, 
             data=MENSUALES_HD2)

m_surv <- lm(surv ~ - 1 + 
               CASEDADA + meses + PMD_CANTBICA + descom +
               mujer + 
               DDIAB + DCISQ + DEVP + DTRABA + DFUMA +
               IAMC_IAMPP + PRIVADO + CORPORATIVO +
               ZCAIMAE:anio, 
             data=MENSUALES_HD2)

m_comp <- lm(comp ~ - 1 + 
               CASEDADA + meses + PMD_CANTBICA + descom +
               mujer + 
               DDIAB + DCISQ + DEVP + DTRABA + DFUMA +
               IAMC_IAMPP + PRIVADO + CORPORATIVO +
               ZCAIMAE:anio, 
             data=MENSUALES_HD2)

m_sept <- lm(sept ~ - 1 + 
               CASEDADA + meses + PMD_CANTBICA + descom +
               mujer + 
               DDIAB + DCISQ + DEVP + DTRABA + DFUMA +
               IAMC_IAMPP + PRIVADO + CORPORATIVO +
               ZCAIMAE:anio, 
             data=MENSUALES_HD2)

m_ktv <- lm(ktv12 ~ - 1 + 
              CASEDADA + meses + PMD_CANTBICA + descom +
              mujer + 
              DDIAB + DCISQ + DEVP + DTRABA + DFUMA +
              IAMC_IAMPP + PRIVADO + CORPORATIVO +
              ZCAIMAE:anio, 
            data=MENSUALES_HD2)

get_all_predictions <- function(model, data) {
  # Create a data frame with all combinations of "IMAE" and "anio"
  data
  combinations <- expand.grid(
    IMAE = levels(data$ZCAIMAE),  
    anio = levels(data$anio)      
  )
  
  # Convert "IMAE" and "anio" to factors
  combinations$IMAE <- as.factor(combinations$IMAE)
  combinations$anio <- as.factor(combinations$anio)
  
  # Create a data frame with mean values for other predictors
  mean_values <- data.frame(
    CASEDADA = mean(data$CASEDADA, na.rm = TRUE),
    meses = mean(data$meses, na.rm = TRUE),
    mujer = mean(data$mujer, na.rm = TRUE),
    DDIAB = mean(data$DDIAB, na.rm = TRUE),
    DCISQ = mean(data$DCISQ, na.rm = TRUE),
    DEVP = mean(data$DEVP, na.rm = TRUE),
    IAMC_IAMPP = mean(data$IAMC_IAMPP, na.rm = TRUE),
    PRIVADO = mean(data$PRIVADO, na.rm = TRUE),
    CORPORATIVO = mean(data$CORPORATIVO, na.rm = TRUE),
    DTRABA = mean(data$DTRABA, na.rm = TRUE),
    DFUMA = mean(data$DFUMA, na.rm = TRUE),
    PMD_CANTBICA = mean(data$PMD_CANTBICA, na.rm = TRUE),
    descom = mean(data$descom, na.rm = TRUE)
  )
  
  # Initialize an empty data frame to store predictions
  all_predictions <- data.frame()
  
  # Loop through all combinations and get predictions
  for (i in 1:nrow(combinations)) {
    mean_values$ZCAIMAE <- combinations$IMAE[i]
    mean_values$anio <- combinations$anio[i]
    predictions <- predict(model, newdata = mean_values)
    
    # Check if the coefficient for ZCAIMAE:anio is NA, and set predictions to NA accordingly
    if (is.na(coef(model)[paste0("ZCAIMAE", combinations$IMAE[i], ":anio", combinations$anio[i])])) {
      predictions <- NA
    }
    
    result <- data.frame(IMAE = combinations$IMAE[i], anio = combinations$anio[i], Prediction = predictions)
    all_predictions <- rbind(all_predictions, result)
  }
  
  return(all_predictions)
}

INGRESOS_HD2 <- read_csv("C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/MastersThesis/INGRESOS_HD2.csv")

tipo_imae <- INGRESOS_HD2 %>% ungroup() %>% select("ZCAIMAE", "tipo_choice") %>% unique()
IMAE_num <- read_csv("C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/MastersThesis/IMAE_num.csv") %>% filter(ZCAIMAE!="HOSPITAL ITALIANO")

pred_urea <- get_all_predictions(m_urea, MENSUALES_HD2) %>% rename(urea=Prediction)
pred_surv <- get_all_predictions(m_surv, MENSUALES_HD2) %>% rename(surv=Prediction)
pred_fosf <- get_all_predictions(m_fosf, MENSUALES_HD2) %>% rename(fosf=Prediction)
pred_hemo <- get_all_predictions(m_hemo, MENSUALES_HD2) %>% rename(hemo=Prediction)
pred_comp <- get_all_predictions(m_comp, MENSUALES_HD2) %>% rename(comp=Prediction)
pred_sept <- get_all_predictions(m_sept, MENSUALES_HD2) %>% rename(sept=Prediction)
#pred_peso <- get_all_predictions(m_peso, base) %>% rename(peso=Prediction)
pred_URR <- get_all_predictions(m_URR, MENSUALES_HD2) %>% rename(URR=Prediction)
pred_ktv <- get_all_predictions(m_ktv, MENSUALES_HD2) %>% rename(ktv=Prediction)
#pred_comp_dialisis <- get_all_predictions(m_comp_dialisis, base) %>% rename(comp_dialisis=Prediction)

quality <- left_join(pred_URR, pred_surv, by=c("IMAE", "anio")) %>% 
  #left_join(pred_fosf, by=c("IMAE", "anio")) %>%
  left_join(pred_hemo, by=c("IMAE", "anio")) %>%
  left_join(pred_comp, by=c("IMAE", "anio")) %>%
  left_join(pred_sept, by=c("IMAE", "anio")) %>%
  #left_join(pred_peso, by=c("IMAE", "anio")) %>%
  left_join(pred_urea, by=c("IMAE", "anio")) %>%
  left_join(pred_ktv, by=c("IMAE", "anio")) %>% 
  #left_join(pred_comp_dialisis, by=c("IMAE", "anio")) %>%
  left_join(tipo_imae, by=c("IMAE"="ZCAIMAE")) %>%
  mutate(tipo_imae2=case_when(tipo_choice=="INDEPENDIENTE" ~ "Indep",
                              tipo_choice=="PRIVADO" ~ "Priv Ins",
                              tipo_choice=="PUBLICO" ~ "Pub Ins")) %>% 
  left_join(IMAE_num, by=c("IMAE"="ZCAIMAE")) %>% 
  rename(ZCAIMAE=IMAE) %>% mutate(anio=as.double(as.character(anio))) %>% 
  select(-c(choice))


write.csv(
  quality,
  "C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/MastersThesis/quality.csv", 
  row.names=FALSE)

non_adj_quality <- MENSUALES_HD2 %>% 
  group_by(ZCAIMAE, anio) %>% 
  summarise(urea=mean(urea17, na.rm = T),
            surv=mean(surv, na.rm = T),
            fosf=mean(fosf55, na.rm = T),
            hemo=mean(hemo10, na.rm = T),
            #peso=mean(peso, na.rm = T),
            comp=mean(comp, na.rm = T),
            URR=mean(URR65, na.rm = T),
            ktv=mean(ktv12, na.rm = T),
            sept=mean(sept, na.rm = T)) %>%
  rename(IMAE=ZCAIMAE) %>% 
  left_join(tipo_imae, by=c("IMAE"="ZCAIMAE")) %>%
  mutate(tipo_imae2=case_when(tipo_choice=="INDEPENDIENTE" ~ "Indep",
                              tipo_choice=="PRIVADO" ~ "Priv Ins",
                              tipo_choice=="PUBLICO" ~ "Pub Ins")) %>% 
  left_join(IMAE_num, by=c("IMAE"="ZCAIMAE"))


adjusted <- quality %>%
  pivot_longer(cols = c("urea", "surv", "hemo", "comp", 
                        "sept", "URR", "ktv"), 
               names_to = "measure", values_to = "value") %>% 
  group_by(measure) %>% 
  summarise(Mean = round(mean(value, na.rm = TRUE), 2), 
            "Std Dev" = paste0("(", round(sd(value, na.rm = TRUE), 2), ")")) %>% 
  as.data.frame()

unadjusted <- non_adj_quality %>%
  pivot_longer(cols = c("urea", "surv", "hemo", 
                        "comp", "sept",  "URR", "ktv"), 
               names_to = "measure", values_to = "value") %>% 
  group_by(measure) %>% 
  summarise(Mean = round(mean(value, na.rm = TRUE), 2), 
            "Std Dev" = paste0("(", round(sd(value, na.rm = TRUE), 2), ")"))

labs <- c("Urea", "Survival", "Hemoglobin", 
          "No complications", "Septic infection", 
          "Urea Reduction Rate", "Kt/V")
names(labs) <- c("urea", "surv", "hemo", "comp", "sept", "URR", "ktv")

names <- cbind(names(labs), labs)%>% as.data.frame()
colnames(names) <- c("measure", "Names") 

mean_sd <- unadjusted %>% full_join(adjusted, by="measure") %>% 
  full_join(names, by="measure") %>%
  select(Names, everything(), -measure)

print(xtable::xtable(mean_sd, caption = "Means and Standard Deviations of Variables"), 
      caption.placement = "top", include.rownames = FALSE)

quality %>%
  filter(depto=="01",
         ZCAIMAE!="SENNIAD HEMO") %>% 
  pivot_longer(c(hemo, comp, URR, ktv)) %>% 
  ggplot(aes(y=value, x=tipo_imae2, color=tipo_imae2)) +
  geom_boxplot() +
  facet_wrap(~name, scale="free_y", labeller = labeller(name=labs)) +
  coord_cartesian(ylim = c(0, 1)) +  # Restrict y-axis from 0 to 1
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),  # Remove x-axis label
    axis.title.y = element_blank(),  # Remove x-axis label
    panel.background = element_rect(fill = "white"),  # Change background to white
    panel.grid.major = element_line(color = "gray"),  # Change major gridlines to gray
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    strip.background = element_blank()  # Remove background from facet titles
  ) +
  scale_color_viridis_d(end=0.8)
#ggsave("adjusted.png", dpi = 500, wid)

non_adj_quality %>%
  filter(depto=="01",
         IMAE!="SENNIAD HEMO") %>% 
  pivot_longer(c(hemo, comp, URR, ktv)) %>% 
  ggplot(aes(y=value, x=tipo_imae2, color=tipo_imae2)) +
  geom_boxplot() +
  facet_wrap(~name, scale="free_y", labeller = labeller(name=labs)) +
  coord_cartesian(ylim = c(0, 1)) +  # Restrict y-axis from 0 to 1
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),  # Remove x-axis label
    axis.title.y = element_blank(),  # Remove x-axis label
    panel.background = element_rect(fill = "white"),  # Change background to white
    panel.grid.major = element_line(color = "gray"),  # Change major gridlines to gray
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    strip.background = element_blank()  # Remove background from facet titles
  ) +
  scale_color_viridis_d(end=0.8)
#ggsave("unadjusted.png", dpi = 500, scale=3)

library(stargazer)
stargazer(m_urea, m_hemo, m_surv, m_comp, m_sept, m_ktv, m_URR,
          type="latex",
          keep = c("CASEDADA", "meses", "PMD_CANTBICA", "descom", "mujer",
                   "DDIAB", "DCISQ", "DEVP", "DTRABA", "DFUMA",
                   "IAMC_IAMPP", "PRIVADO", "CORPORATIVO"), 
          df=FALSE, omit.stat = c("ser","f", "rsq"), no.space = TRUE,
          report = "vc*",
          dep.var.labels = c("Urea", "Hemoglobin", "Survival", 
                             "Complications", "Infections", "Kt/V", "URR"),
          covariate.labels = c("Age", "Months on dialysis", "Monthly sessions", "Decompensated at start", 
                               "Female", "Diabetic",  "Cardiopathy", "Vascular perfipheral disease",
                               "Working", "Smoking",
                               "IAMC/IAMPP", "Private insurance", "Corporate insurance"),
          out="reg.tex",
          notes = "Dependent variables are dummies indicating adecuate levels of the measure.",
          notes.append = T,
          notes.align = "l")


mean_qual <- quality %>%
  filter(depto=="01",
         ZCAIMAE!="SENNIAD HEMO") %>% 
  rename(IMAE=ZCAIMAE) %>% 
  group_by(IMAE) %>% 
  summarise(URR = mean(URR, na.rm = TRUE),
            ktv = mean(ktv, na.rm = TRUE), 
            sept = mean(sept, na.rm = TRUE),
            surv = mean(surv, na.rm = TRUE),
            tipo_imae2 = first(tipo_imae2))


mean_non_adj_qual <- non_adj_quality %>%
  filter(depto=="01",
         IMAE!="SENNIAD HEMO") %>% 
  group_by(IMAE) %>% 
  summarise(URR = mean(URR, na.rm = TRUE),
            ktv = mean(ktv, na.rm = TRUE), 
            sept = mean(sept, na.rm = TRUE),
            surv = mean(surv, na.rm = TRUE),
            tipo_imae2 = first(tipo_imae2))


mean_qual %>% 
  mutate(IMAE=case_when(
    IMAE=="ASOCIACION ESPAÑOLA" ~ "ESPAÑO",
    IMAE=="CANMU" ~ "MEDICA",
    IMAE=="CASMU" ~ "CASMU",
    IMAE=="CE.DI.SA." ~ "CEDISA",
    IMAE=="HOSPITAL BRITANICO" ~ "BRITÁNI",
    IMAE=="HOSPITAL DE CLINICAS" ~ "CLÍNICAS",
    IMAE=="HOSPITAL EVANGELICO" ~ "EVANGÉL",
    IMAE=="HOSPITAL MACIEL" ~ "MACIEL",
    IMAE=="SMI - SERVICIO MEDICO INTEGRAL" ~ "SMI",
    IMAE=="CASA DE GALICIA" ~ "GALICIA",
    IMAE=="URUGUAYANA" ~ "URUGUAY",
    IMAE=="UNIVERSAL" ~ "UNIVERS",
    .default = as.character(IMAE)
  )) %>% 
  ggplot(aes(y = URR, x = reorder(IMAE, -URR), fill = tipo_imae2)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_fill_viridis_d(end = 0.8, name = "Provider type") +  # Adding legend title
  theme(
    legend.position = "bottom",
    axis.title.x = element_blank(),  # Remove x-axis label
    axis.title.y = element_blank(),  # Remove x-axis label
    panel.background = element_rect(fill = "white"),  # Change background to white
    panel.grid.major = element_line(color = "gray"),  # Change major gridlines to gray
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    strip.background = element_blank()  # Remove background from facet titles
  ) +
  scale_x_discrete(labels = function(x) substr(x, nchar(x)-7, nchar(x))) +
  coord_cartesian(ylim = c(0, 1))
ggsave("mean_adjusted_URR.png", dpi = 500, scale=3)


mean_non_adj_qual %>% 
  mutate(IMAE=case_when(
    IMAE=="ASOCIACION ESPAÑOLA" ~ "ESPAÑO",
    IMAE=="CANMU" ~ "MEDICA",
    IMAE=="CASMU" ~ "CASMU",
    IMAE=="CE.DI.SA." ~ "CEDISA",
    IMAE=="HOSPITAL BRITANICO" ~ "BRITÁNI",
    IMAE=="HOSPITAL DE CLINICAS" ~ "CLÍNICAS",
    IMAE=="HOSPITAL EVANGELICO" ~ "EVANGÉL",
    IMAE=="HOSPITAL MACIEL" ~ "MACIEL",
    IMAE=="SMI - SERVICIO MEDICO INTEGRAL" ~ "SMI",
    IMAE=="CASA DE GALICIA" ~ "GALICIA",
    IMAE=="URUGUAYANA" ~ "URUGUAY",
    IMAE=="UNIVERSAL" ~ "UNIVERS",
    .default = as.character(IMAE)
  ))  %>% 
  ggplot(aes(y = URR, x = reorder(IMAE, -URR), fill = tipo_imae2)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_fill_viridis_d(end = 0.8, name = "Provider type") +  # Adding legend title
  theme(
    legend.position = "bottom",
    axis.title.x = element_blank(),  # Remove x-axis label
    axis.title.y = element_blank(),  # Remove x-axis label
    panel.background = element_rect(fill = "white"),  # Change background to white
    panel.grid.major = element_line(color = "gray"),  # Change major gridlines to gray
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    strip.background = element_blank()  # Remove background from facet titles
  ) +
  scale_x_discrete(labels = function(x) substr(x, nchar(x)-7, nchar(x))) +
  coord_cartesian(ylim = c(0, 1))
ggsave("mean_unadjusted_URR.png", dpi = 500, scale=3)


# X_imae ----

CAIMAE <- unique(imaes$CAIMAE)
mes_solicitud <- seq(as.Date("2003-01-01"), as.Date("2016-12-01"), by = "month")

mes_solicitud <- format(mes_solicitud, "%Y-%m") # Convert mes_solicitud to YYYY-MM format
data <- expand.grid(CAIMAE = CAIMAE, mes_solicitud = mes_solicitud) # Create all combinations of CAIMAE and mes_solicitud

quality <- read_csv("quality.csv")

X_imae <-
  data %>% 
  left_join(imaes, by=c("CAIMAE")) %>%
  left_join(imae_inst, by=c("ZCAIMAE")) %>%
  left_join(congestion, by=c("CAIMAE", "mes_solicitud")) %>%
  left_join(occupancy, by=c("ZCAIMAE", "mes_solicitud")) %>%
  left_join(turnos, by=c("CAIMAE", "mes_solicitud")) %>% 
  separate(mes_solicitud, into=c("anio_solicitud", "mes"), remove=F) %>% select(-mes) %>% 
  mutate(CAIMAE=as.factor(CAIMAE), anio_solicitud=as.double(anio_solicitud)) %>% 
  left_join(delays, by=c("ZCAIMAE", "anio_solicitud")) %>% 
  left_join(quality, by=c("ZCAIMAE", "anio_solicitud"="anio", "depto", "id")) %>% 
  select(-c(ID_CAIMAE, tipo_imae2))
  

# Logit_INGRESOS ----


Logit_INGRESOS <- 
  INGRESOS_HD %>%
  left_join(IMAE_num, by="ZCAIMAE") %>%
  filter(depto=="01",
         ZCAIMAE!="SENNIAD HEMO") %>%
  select(CAIMAE, anio_solicitud, CAPACNUM, ZCASINST,
         CASEDADA, CASEXO, ZCASINST, ZCASDEPAR,
         CAFECSOL, ZB1SMEDIC, ZB1SRAZA, ZB1SOCUP0, exa_peso, exa_altura,
         B1SNIVEL, CAPACNUM, ZCASINST, anio_solicitud, tiene_imae, 
         tipo_inst, tipo_pac, estu_creatinemia,
         AADIASI, AACATP, AAFAV, DDIAG1, descom, coord, mes_solicitud, 
         imae_inst, exa_capacidad) %>% 
  dummy_cols(select_columns = "CAIMAE") %>% 
  pivot_longer(cols=starts_with("CAIMAE_"), names_to=c("borrar","ID_CAIMAE"), values_to = "choice", names_sep = "_") %>% 
  select(-c(borrar, CAIMAE)) %>% 
  left_join(pacmens_capacity, by=c("ID_CAIMAE", "mes_solicitud")) %>% 
  left_join(X_imae, by=c("ID_CAIMAE"="CAIMAE", "mes_solicitud", "anio_solicitud")) %>% 
  left_join(MEDICOS, by=c("ZB1SMEDIC"="ZB1RMEDICO", "anio_solicitud"="PMDANIO")) %>%
  #left_join(delta_p, by=c("ZCAIMAE", "anio_solicitud"="anio")) %>% 
  left_join(dist, by=c("CAPACNUM", "num_choice", "id", "depto")) %>% 
  left_join(dist_sim, by=c("CAPACNUM", "num_choice", "id", "depto")) %>% 
  mutate(inst=case_when(ZCASINST==ZCASINST_IMAE~1,
                        is.na(ZCASINST_IMAE)~0,
                              .default = 0),
         medimae = 
           ifelse(rowSums(select(., starts_with("medimae")) == id, 
                          na.rm = TRUE) > 0, 1, 0),
         distk=dist/1000,
         distk_sim=dist_sim/1000) %>% 
  select(-c(matches("^medimae[0-9]+$"))) %>% 
  mutate(real_v_sim=distk_sim-distk)

write_dta(Logit_INGRESOS, 
          "C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/MastersThesis/Logit_INGRESOS.dta",
          version = 14,
          label = attr(data, "label"),
          strl_threshold = 2045,
          adjust_tz = TRUE)

