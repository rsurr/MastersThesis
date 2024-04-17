library(readr)
library(tidyverse)
library(haven)
library(fastDummies)
library(data.table)
library(haven)


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
IMAE_num <- read_csv("IMAE_num.csv") %>% filter(ZCAIMAE!="HOSPITAL ITALIANO")

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
  select(slack2, slack_perc2, 
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

SESIONES_HD <- read_sav("C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/Databases/SESIONES HD.sav") 

SESIONES_HD <- SESIONES_HD %>% 
  unite("fecha", PMD_ANIO:PMD_MES, sep="-", remove=FALSE) %>% 
  mutate(Date = as.Date(paste(fecha, "-01", sep="")))

SESIONES_HD <- SESIONES_HD %>% 
  mutate(
    ZPMD_IMAE=if_else(ZPMD_IMAE=="HOSPITAL ITALIANO", "UNIVERSAL", ZPMD_IMAE))

occupancy <- SESIONES_HD %>% select(ZPMD_IMAE, PMD_ANIO, PMD_MES, CAPACNUM, Date) %>%
  mutate(mes_solicitud=format(Date, "%Y-%m")) %>% 
  group_by(ZPMD_IMAE, mes_solicitud) %>% 
  summarise(n=n(), mes_solicitud=first(mes_solicitud), PMD_ANIO=first(PMD_ANIO)) %>% 
  ungroup() %>% 
  group_by(ZPMD_IMAE, PMD_ANIO) %>% 
  mutate(promedio_anual=mean(n, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(
         dif=n-promedio_anual,
         dif_prop=dif/promedio_anual) %>% 
  rename(occ_var=dif,
         occ_var_prop=dif_prop,
         ZCAIMAE=ZPMD_IMAE) %>% 
  select(occ_var, occ_var_prop, ZCAIMAE, mes_solicitud)
  
# Medimae ----

MEDICOS <- SESIONES_HD %>%
  left_join(IMAE_num, by=c("ZPMD_IMAE"="ZCAIMAE")) %>% # Column with facility number
  rename(medimae=choice) %>% # Name facility number column "medimae"
  filter(depto=="01", # Filter for facilities in Montevideo
         ZPMD_IMAE!="SENNIAD HEMO") %>% #Not pediatric
  group_by(ZB1RMEDICO, medimae, PMD_ANIO) %>% 
  summarise(n=n()) %>% # Get the number of sessions of doctors in each facility (per year)
  mutate(medimae=ifelse(medimae=="", NA, medimae), # Put NA if facility number blank
         ZB1RMEDICO=ifelse(ZB1RMEDICO=="" | # Put NA if doctor name blank
                             ZB1RMEDICO=="  No corresponde" |
                             ZB1RMEDICO=="   No corresponde",
                           NA, ZB1RMEDICO)) %>% 
  filter(!is.na(medimae), !is.na(ZB1RMEDICO)) %>% 
  group_by(ZB1RMEDICO, PMD_ANIO) %>% 
  mutate(n_max=max(n),
         max_imae=if_else(n==n_max, 1, 0)) %>% 
  filter(max_imae==1)

MEDICOS <- SESIONES_HD %>%
  left_join(IMAE_num, by=c("ZPMD_IMAE"="ZCAIMAE")) %>% # Column with facility number
  rename(medimae=choice) %>% # Name facility number column "medimae"
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

# Quality ----


# X_imae ----

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
            num_choice=first(choice),
            depto=first(depto))

unique(imaes$CAIMAE)

CAIMAE <- unique(imaes$CAIMAE)
mes_solicitud <- seq(as.Date("2003-01-01"), as.Date("2016-12-01"), by = "month")

mes_solicitud <- format(mes_solicitud, "%Y-%m") # Convert mes_solicitud to YYYY-MM format
data <- expand.grid(CAIMAE = CAIMAE, mes_solicitud = mes_solicitud) # Create all combinations of CAIMAE and mes_solicitud


X_imae <-
  data %>% 
  left_join(imaes, by=c("CAIMAE")) %>%
  left_join(imae_inst, by=c("ZCAIMAE")) %>%
  left_join(congestion, by=c("CAIMAE", "mes_solicitud")) %>%
  left_join(occupancy, by=c("ZCAIMAE", "mes_solicitud")) %>%
  left_join(turnos, by=c("CAIMAE", "mes_solicitud")) %>% 
  mutate(CAIMAE=as.factor(CAIMAE))

# Delta peso ----

dP <- SESIONES_HD %>%
  rename(ZCAIMAE=ZPMD_IMAE) %>% 
  mutate(DPESOSE=case_when(DPESOSE==999.00 ~ NA,
                           .default = as.numeric(DPESOSE)),
         DPESOPO=case_when(DPESOPO==999.00 ~ NA,
                           .default = as.numeric(DPESOPO)),
         dife_post=DPESOPO-DPESOSE,
         dife_pre=DPESOPR-DPESOSE,
         peso=case_when(abs(dife_post)<=0.5 ~ 1,
                        abs(dife_post)>0.5 ~ 0,
                        is.na(dife_post) ~ NA)) %>% 
  select(peso, CAPACNUM, DPESOPR, DPESOPO, ZCAIMAE, PMD_ANIO, dife_post, 
         dife_pre)

# Reorder "anio" in increasing order
dP$anio <- factor(dP$PMD_ANIO, levels = sort(unique(dP$PMD_ANIO)))

# Reorder "ZCAIMAE" alphabetically
dP$ZCAIMAE <- factor(dP$ZCAIMAE, levels = sort(unique(dP$ZCAIMAE)))

# Set reference levels for anio, ZCAIMAE and CAPACNUM
base$anio <- relevel(base$anio, ref = "2004")
base$ZCAIMAE <- relevel(base$ZCAIMAE, ref = "ASOCIACION ESPAÑOLA")
base$CAPACNUM <- relevel(base$CAPACNUM, ref="344677")

dP <- dP %>% unite("ZCAIMAEanio", ZCAIMAE:PMD_ANIO, sep=":", remove = FALSE) 

m_dP1 <- feols(peso ~ DPESOPR | ZCAIMAEanio + CAPACNUM, dP)
m_dP2 <- feols(peso ~ dife_pre | ZCAIMAEanio + CAPACNUM, dP)
m_dP3 <- feols(dife_post ~ DPESOPR | ZCAIMAEanio + CAPACNUM, dP)
m_dP4 <- feols(dife_post ~ dife_pre | ZCAIMAEanio + CAPACNUM, dP)
m_dP5 <- feols(DPESOPO ~ DPESOPR | ZCAIMAEanio + CAPACNUM, dP)

delta_p1 <- fixef(m_dP1)$ZCAIMAEanio %>% as.data.frame() %>% 
  rownames_to_column("ZCAIMAEanio")
colnames(delta_p1) <- c("ZCAIMAEanio", "delta_p1")

delta_p2 <- fixef(m_dP2)$ZCAIMAEanio %>% as.data.frame() %>% 
  rownames_to_column("ZCAIMAEanio")
colnames(delta_p2) <- c("ZCAIMAEanio", "delta_p2")

delta_p3 <- fixef(m_dP3)$ZCAIMAEanio %>% as.data.frame() %>% 
  rownames_to_column("ZCAIMAEanio")
colnames(delta_p3) <- c("ZCAIMAEanio", "delta_p3")

delta_p4 <- fixef(m_dP4)$ZCAIMAEanio %>% as.data.frame() %>% 
  rownames_to_column("ZCAIMAEanio")
colnames(delta_p4) <- c("ZCAIMAEanio", "delta_p4")

delta_p5 <- fixef(m_dP5)$ZCAIMAEanio %>% as.data.frame() %>% 
  rownames_to_column("ZCAIMAEanio")
colnames(delta_p5) <- c("ZCAIMAEanio", "delta_p5")

delta_p <- delta_p1 %>% 
  left_join(delta_p2, join_by("ZCAIMAEanio")) %>%
  left_join(delta_p3, join_by("ZCAIMAEanio")) %>%
  left_join(delta_p4, join_by("ZCAIMAEanio")) %>%
  left_join(delta_p5, join_by("ZCAIMAEanio")) %>%
  separate(ZCAIMAEanio, sep = ":", into = c("ZCAIMAE", "anio")) %>% 
  mutate(anio=as.double(anio))


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
  left_join(X_imae, by=c("ID_CAIMAE"="CAIMAE", "mes_solicitud")) %>% 
  left_join(MEDICOS, by=c("ZB1SMEDIC"="ZB1RMEDICO", "anio_solicitud"="PMDANIO")) %>%
  left_join(quality, by=c("ZCAIMAE", "anio_solicitud"="anio")) %>% 
  left_join(delta_p, by=c("ZCAIMAE", "anio_solicitud"="anio")) %>% 
  mutate(inst=case_when(ZCASINST==ZCASINST_IMAE~1,
                        is.na(ZCASINST_IMAE)~0,
                              .default = 0),
         medimae = ifelse(rowSums(select(., starts_with("medimae")) == num_choice, na.rm = TRUE) > 0, 1, 0)) %>% 
  select(-c(matches("^medimae[0-9]+$")))


write_dta(Logit_INGRESOS, 
          "C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/MastersThesis/Logit_INGRESOS.dta",
          version = 14,
          label = attr(data, "label"),
          strl_threshold = 2045,
          adjust_tz = TRUE)

