library(readr)
library(tidyverse)
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
                  "CANMU", "CASMU", "CENDIME", "COMEF", "COMEPA", "COMERO",
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

INGRESOS_HD <- read_sav("C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/Databases/INGRESOS HD.sav") %>% 
  #filter(ZCASDEPAR=="MONTEVIDEO") %>% 
  mutate(
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
                ZCASINST=="CASMU IAMPP" ~ 1,
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
                ZCAIMAE=="CASMU" & ZCASINST=="CASMU IAMPP" ~ 1,
                ZCAIMAE=="CASA DE GALICIA" & ZCASINST=="CASA DE GALICIA" ~ 1,
                ZCAIMAE=="NEPHROS" & ZCASINST=="COSEM IAMPP" ~ 1,
                
                ZCAIMAE=="UNIVERSAL" & ZCASINST=="UNIVERSAL" ~ 1,
                ZCAIMAE=="HOSPITAL ITALIANO" & ZCASINST=="UNIVERSAL" ~ 1,
                
                ZCAIMAE =="HOSPITAL DE CLINICAS" & ZCASINST %in% INST_ASSE ~ 1,
                ZCAIMAE =="HOSPITAL MACIEL" & ZCASINST %in% INST_ASSE ~ 1,
                TRUE ~ 0),
    tipo_pac=
      case_when(ZB1SOCUP0 %in% PAC_ACTIVO ~ "Activo",
                ZB1SOCUP0 %in% PAC_INACTIVO ~ "Inactivo",
                TRUE ~ NA),
    fecha_solicitud=as.Date(CAFECSOL),
    fecha_autorizacion=as.Date(CAFECAUT),
    mes_solicitud=format(fecha_solicitud, "%Y-%m"),
    mes_autorizacion=format(fecha_autorizacion, "%Y-%m")) %>% 
  group_by(CAPACNUM) %>% 
  slice_max(CASEDADA, with_ties = FALSE)

SESIONES_HD <- read_sav("C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/Databases/SESIONES HD.sav") 

SESIONES_HD <-  SESIONES_HD %>% 
  unite("fecha", PMD_ANIO:PMD_MES, sep="-", remove=FALSE) %>% 
  mutate(Date = as.Date(paste(fecha, "-01", sep="")),
         mes = format_ISO8601(Date, precision = "ym"))

MEDICOS <- SESIONES_HD %>% 
  group_by(ZB1RMEDICO, ZPMD_IMAE) %>% 
  summarise(n=n()) %>%
  mutate(ZPMD_IMAE=ifelse(ZPMD_IMAE=="", NA, ZPMD_IMAE),
         ZB1RMEDICO=ifelse(ZB1RMEDICO=="" |
                             ZB1RMEDICO=="  No corresponde" |
                             ZB1RMEDICO=="   No corresponde",
                           NA, ZB1RMEDICO)) %>% 
  filter(!is.na(ZPMD_IMAE), !is.na(ZB1RMEDICO)) %>%
  group_by(ZB1RMEDICO) %>% 
  mutate(prop=round(n/sum(n), 4),
         imae_rank=row_number(desc(prop))) %>% 
  select(-c(n, prop)) %>% 
  pivot_wider(names_from = imae_rank, values_from = ZPMD_IMAE,
              names_prefix = "imae") %>% 
  select(ZB1RMEDICO, imae1, imae2, imae3, imae4, imae5, imae6,
         imae7, imae8, imae9, imae10, imae11, imae12, imae13, 
         imae14, imae15, imae16, imae17, imae18, imae19)


PACIENTES_MEDICOS <- left_join(INGRESOS_HD, MEDICOS, by=c("ZB1SMEDIC"="ZB1RMEDICO")) %>%
  #filter(ZCASDEPAR=="MONTEVIDEO") %>% 
  mutate(med_imae=case_when(ZCAIMAE==imae1 |
                              ZCAIMAE==imae2 | 
                              ZCAIMAE==imae3 | 
                              ZCAIMAE==imae4 | 
                              ZCAIMAE==imae5 | 
                              ZCAIMAE==imae6 | 
                              ZCAIMAE==imae7 | 
                              ZCAIMAE==imae8 | 
                              ZCAIMAE==imae9 | 
                              ZCAIMAE==imae10 | 
                              ZCAIMAE==imae11 | 
                              ZCAIMAE==imae12 | 
                              ZCAIMAE==imae13 |
                              ZCAIMAE==imae14 |
                              ZCAIMAE==imae15 |
                              ZCAIMAE==imae16 |
                              ZCAIMAE==imae17 |
                              ZCAIMAE==imae18 |
                              ZCAIMAE==imae19
                            ~ 1, 
                            TRUE ~ 0)) %>% 
  group_by(CAPACNUM) %>% 
  slice_max(CASEDADA, with_ties = FALSE)

INGRESOS_HD <- PACIENTES_MEDICOS %>% select(CAPACNUM, med_imae) %>% 
  right_join(INGRESOS_HD, by="CAPACNUM") %>% 
  mutate(med_imae=ifelse(is.na(med_imae), 0, med_imae))

write.csv(INGRESOS_HD, 
          "C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/MastersThesis/INGRESOS_HD2.csv", 
          row.names=FALSE)
