library(readr)
library(tidyverse)
library(haven)
library(fastDummies)
library(data.table)

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
                    "AMEDRIN IAMPP", "COMTA IAMPP", "AFITYC***", "CRAME IAMPP", "S.M.Q. SALTO")

INST_CORPORATIVO <- c("H.POLICIAL", "ANCAP***", "ANV (BHU)***")

INST_SEGUROPRIVADO <- c("COPAMHI MEDICARE", "CIMA ESPAÑA*", "MP MEDICINA PERSONALIZADA",
                        "I.Q. SUDAMERICANO", "HOSPITAL BRITANICO", "SUMMUM FONASA", "BLUE CROSS FONASA")

PAC_INACTIVO <- c("Jubilado", "Menor dependiente (< 15 años)", "Sin ocupación")

PAC_ACTIVO <- c("Artesano", "Empleado comercio", "Empleado rural", "Empleado servicio", 
                "Estudiante", "Funcionario público", "Otros", "Profesional liberal",
                "Propietario agricultura", "Propietario de comercio", "Propietario industrial",
                "Propietario servicio", "Tareas del hogar")

INGRESOS_HD <- 
  read_sav("C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/Databases/INGRESOS HD.sav") %>% 
  #filter(ZCASDEPAR=="MONTEVIDEO") %>% 
  mutate(
    ZCAIMAE=if_else(ZCAIMAE=="HOSPITAL ITALIANO", "UNIVERSAL", ZCAIMAE),
    ZCASINST=if_else(ZCAINST=="HOSPITAL ITALIANO", "UNIVERSAL", ZCASINST),
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
                ZCASINST %in% INST_ASSE ~ 1,
                TRUE ~ 0),
    inst_imae=
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
                
                ZCAIMAE =="HOSPITAL DE CLINICAS" & ZCASINST %in% INST_ASSE ~ 1,
                ZCAIMAE =="HOSPITAL MACIEL" & ZCASINST %in% INST_ASSE ~ 1,
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

SESIONES_HD <- read_sav("C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/Databases/SESIONES HD.sav") %>% 
  unite("fecha", PMD_ANIO:PMD_MES, sep="-", remove=FALSE) %>% 
  mutate(Date = as.Date(paste(fecha, "-01", sep="")))

IMAE_num <- read_csv("IMAE_num.csv")

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
  summarise_at(vars(starts_with("medimae")), funs(.= max(.))) # Collapse to one observation per doctor-year

names(MEDICOS) <- gsub("[._]", "", names(MEDICOS))  # Remove "_" and "."

a <- 
  matrix(c("ASOCIACION ESPAÑOLA", "ASOCIACION ESPAÑOLA", "SMI - SERVICIO MEDICO INTEGRAL",
           "SMI - SERVICIO MEDICO INTEGRAL", "HOSPITAL BRITANICO", "HOSPITAL BRITANICO",
           "S.M.Q. SALTO", "S.M.Q. SALTO", "COMEPA", "COMEPA", "COMEF", "COMEF IAMPP",
           "CASMU", "CASMU - IAMPP", "CASA DE GALICIA", "CASA DE GALICIA", "NEPHROS",
           "COSEM IAMPP", "UNIVERSAL", "UNIVERSAL", "HOSPITAL ITALIANO", "UNIVERSAL",
           "HOSPITAL DE CLINICAS", "INST_ASSE", "HOSPITAL MACIEL", "INST_ASSE"),
         ncol=2, byrow=T) %>% as.data.frame()

colnames(a) <- c("ZCAIMAE", "ZCAINST")

imae_inst <- a %>% left_join(IMAE_num, by="ZCAIMAE")

colnames(imae_inst) <- c("ZCAIMAE", "ZCASINST", "inst", "depto")

INST <- 
  left_join(INGRESOS_HD, imae_inst, by="ZCASINST", 
            relationship="many-to-many") %>% # Join facility-institution dataset with pacient dataset
  filter(depto=="01", # Filter for facilities in Montevideo
         ZCAIMAE.x!="SENNIAD HEMO") %>% #Not pediatric
  dummy_cols(select_columns = c("inst", "tipo_inst")) %>% 
  group_by(CAPACNUM) %>% 
  summarise_at(vars(starts_with("inst")), funs(.= max(.))) %>% 
  mutate(inst2=0, 
         #inst3=0, inst4=0, inst5=0, inst6=0, 
         #inst7=0, 
         #inst8=0, 
         inst9=0,
         #inst10=0, inst11=0, 
         inst12=0, 
         inst14=0,inst16=0, inst17=0, inst19=0, inst20=0, inst24=0, 
         #inst25=0, inst26=0, inst28=0, inst29=0, inst31=0, inst32=0,
         inst34=0, inst35=0, 
         #inst36=0, inst37=0, inst38=0, inst39=0,inst41=0
         ) 

names(INST) <- gsub("[._]", "", names(INST))  # Remove "_" and "."

tipo <- INGRESOS_HD %>%
  group_by(ZCAIMAE) %>% 
  summarize(tipo_imae=first(tipo_imae)) %>%
  left_join(IMAE_num, by="ZCAIMAE") %>% 
  rename(tipo=tipo_imae, tipo_imae=choice) %>%
  select(tipo_imae, tipo) %>% 
  pivot_wider(names_from = "tipo_imae", values_from = "tipo", names_prefix = "tipo_imae") %>%
  select(tipo_imae1, tipo_imae2, 
         tipo_imae9, 
         tipo_imae12, tipo_imae13, tipo_imae14, 
         tipo_imae16, tipo_imae17, tipo_imae18, tipo_imae19, tipo_imae20, tipo_imae21, tipo_imae22, 
         tipo_imae24, 
         tipo_imae33, tipo_imae34, tipo_imae35, 
         tipo_imae40)

chain <- INGRESOS_HD %>%
  group_by(ZCAIMAE) %>% 
  summarize(chain=first(chain)) %>%
  left_join(IMAE_num, by="ZCAIMAE") %>% 
  rename(cadena=chain, chain=choice) %>%
  select(cadena, chain) %>% 
  pivot_wider(names_from = "chain", values_from = "cadena", names_prefix = "chain") %>%
  select(chain1, chain2, chain9, chain12, 
         chain13, chain14, chain16,
         chain17, chain18, chain19, chain20, 
         chain21, chain22, chain24, chain33, chain34,
         chain35, chain40)

INGRESOS_HD2 <- left_join(INGRESOS_HD, MEDICOS, 
                          by=c("ZB1SMEDIC"="ZB1RMEDICO",
                               "anio_solicitud"="PMDANIO")) %>%
  group_by(CAPACNUM) %>% 
  slice_max(CASEDADA, with_ties = FALSE) %>% 
  left_join(INST, by = join_by(CAPACNUM)) %>%
  cbind(tipo) %>% 
  cbind(chain) %>% 
  select(c(ZCAIMAE, CAPACNUM, 
           num_range("medimae", range = 1:41), 
           num_range("inst", range = 1:41),
           num_range("tipo_imae", range = 1:41),
           num_range("chain", range = 1:41),
           CASEDADA, CASEXO, ZCASINST, ZCASDEPAR,
           CAFECSOL, ZB1SMEDIC, ZB1SRAZA, ZB1SOCUP0, SCEFPE, SCEFTA,
           B1SNIVEL, CAPACNUM, ZCASINST, anio_solicitud, tiene_imae, 
           tipo_inst, tipo_pac, ECREAV, tipo_imae,
           AADIASI, AACATP, AAFAV, DDIAG1, SCDESU, mes_solicitud, inst_imae)) %>% 
  rename(tipo_choice=tipo_imae) %>% 
  mutate_at(vars(starts_with(c("inst", "medimae"))), ~replace(., is.na(.), 0))

write.csv(INGRESOS_HD2, 
          "C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/MastersThesis/INGRESOS_HD2.csv", 
          row.names=FALSE)  

