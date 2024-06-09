# Paquetes ----
library(readr)
library(tidyverse)
library(haven)
library(fastDummies)
library(data.table)
library(zoo)

# Working directory ----
setwd("C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/MastersThesis/Clean")

# INGRESOS ----
source("Script INGRESOS.R")

# INFORMES ----
source("Script INFORMES.R")

# IMAE_num ----
source("Script IMAE_num.R")

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

# MEDICOS ----
MEDICOS <- read.csv("MEDICOS.csv")

# DELTA_P ----
DELTA_P <- read.csv("DELTA_P.csv")

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
  left_join(IMAE_num, by="id") %>% select(-ZCAIMAE) %>% 
  filter(depto=="01") %>% mutate(num_choice=as.double(num_choice))

# Dist_sim ----
GEO_sim <- read_csv("GEO_sim.csv")

dist_sim <- GEO_sim  %>% 
  filter(!is.na(dist18))  %>% # FILTRO PROVISORIO SACANDO PACIENTES CON DIST NA (60 OBS)
  filter(!is.na(long_Google), !is.na(lat_Google),
         lat_Google<(-34.7),
         long_Google<(-56)) %>% 
  select(CAPACNUM, starts_with("dist")) %>% 
  pivot_longer(cols = starts_with("dist"), names_to = "names", values_to = "dist_sim") %>% 
  separate(col = names, into = c("borrar", "id"), sep = "t") %>% select(-borrar) %>% 
  left_join(IMAE_num, by="id") %>%  select(-ZCAIMAE) %>% 
  filter(depto=="01") %>% mutate(num_choice=as.double(num_choice))

# Delays ----
delays <- INGRESOS %>% 
  select(CAPACNUM, CAFECAUT, EBESTADF, ZEBESTAD, CAFECSOL, ZCAIMAE, anio_solicitud) %>%
  mutate(delays = as.numeric(difftime(CAFECAUT, CAFECSOL, units = "days"))) %>% 
  group_by(ZCAIMAE, anio_solicitud) %>% 
  summarise(delays=mean(delays, na.rm = T))

# Quality ----
quality <- read.csv("quality.csv") %>% 
  select(-c(depto, id, tipo_imae))

# Instrumentos
Zmed <- Logit_INGRESOS %>% group_by(ZCAIMAE, anio_solicitud) %>% 
  summarise(Zmed=sum(medimae)) %>% arrange(ZCAIMAE, anio_solicitud)

Zins <- Logit_INGRESOS %>% group_by(ZCAIMAE, anio_solicitud) %>% 
  summarise(Zins=sum(inst)) %>% arrange(ZCAIMAE, anio_solicitud)

# Pacientes mensuales ----
MENSUALES_HD <- read_sav("~/Proyecto Tesis/Databases/MENSUALES HD.sav")

pacientes_mensuales <- MENSUALES_HD %>% 
  unite("fecha2", PMD_ANIO:PMD_MES, sep="-", remove=F) %>% 
  mutate(fecha3=as.Date(paste(fecha2, "-01", sep=""))) %>%
  mutate(
    ZPMD_IMAE=if_else(ZPMD_IMAE=="HOSPITAL ITALIANO", "UNIVERSAL", ZPMD_IMAE),
    PMD_IMAE=if_else(PMD_IMAE==63, 95, PMD_IMAE)) %>% 
  #filter(PMD_ORIG=="Del centro") %>% 
  rename(CAIMAE=PMD_IMAE) %>% 
  group_by(CAIMAE, fecha3) %>% 
  summarize(
    n_centro=n_distinct(CAPACNUM[PMD_ORIG=="Del centro"]),
    n_trans=n_distinct(CAPACNUM[PMD_ORIG=="Transitorio"]),
    ses_centro=sum(PMD_CANTBICA[PMD_ORIG=="Del centro"]),
    ses_trans=sum(PMD_CANTBICA[PMD_ORIG=="Transitorio"]),
  ) %>% 
  mutate(mes_solicitud=format(fecha3, "%Y-%m"),
         CAIMAE=as.factor(CAIMAE))

# INSTRUMENTOS ----
source("Script INSTRUMENTOS.R")

# Delta ----
delta200_000 <- read_dta("~/Proyecto Tesis/MastersThesis/delta200_000.dta") %>% 
  select(CAPACNUM, anio_solicitud, ZCAIMAE, delta, starts_with("b_"), total, n, s_obs, s,
         epsilon, max_epsilon, p_iv)

# Congestion
CONGESTION <- INGRESOS %>% 
  group_by(ZCAIMAE, anio_solicitud) %>% 
  summarise(turnos_op=mean(turnos_op),
            hab_op=mean(hab_op))

# X_imae ----
imaes <- INGRESOS %>%
  left_join(IMAE_num, join_by("ZCAIMAE")) %>% 
  filter(depto=="01",
         ZCAIMAE!="SENNIAD HEMO") %>% 
  group_by(CAIMAE) %>% 
  summarise(CAIMAE=first(CAIMAE),
            ZCAIMAE=first(ZCAIMAE),
            chain=first(chain),
            transp=first(transp),
            tipo_imae=first(tipo_imae),
            privado=first(privado), 
            indep=first(indep), 
            publico=first(publico),
            num_choice=first(num_choice),
            id=first(id),
            depto=first(depto)) %>% 
  rename(ID_CAIMAE=num_choice)

CAIMAE <- unique(imaes$CAIMAE)
mes_solicitud <- seq(as.Date("2003-01-01"), as.Date("2016-12-01"), by = "month")

mes_solicitud <- format(mes_solicitud, "%Y-%m") # Convert mes_solicitud to YYYY-MM format
data <- expand.grid(CAIMAE = CAIMAE, mes_solicitud = mes_solicitud) # Create all combinations of CAIMAE and mes_solicitud

X_imae <-
  data %>% 
  left_join(imaes, by=c("CAIMAE")) %>%
  left_join(imae_inst, by=c("ZCAIMAE")) %>%
  separate(mes_solicitud, into=c("anio_solicitud", "mes"), remove=F) %>% select(-mes) %>% 
  mutate(CAIMAE=as.factor(CAIMAE), anio_solicitud=as.double(anio_solicitud)) %>% 
  left_join(delays, by=c("ZCAIMAE", "anio_solicitud")) %>% 
  left_join(quality, by=c("ZCAIMAE", "anio_solicitud"="anio")) %>% 
  left_join(Zmed, by=c("ZCAIMAE", "anio_solicitud")) %>% 
  left_join(Zins, by=c("ZCAIMAE", "anio_solicitud")) %>% 
  left_join(pacientes_mensuales, by=c("CAIMAE", "mes_solicitud")) %>% 
  left_join(CONGESTION, by=c("ZCAIMAE", "anio_solicitud")) %>% 
  left_join(INSTRUMENTOS, by=c("ZCAIMAE"="ZPMD_IMAE", "anio_solicitud"="PMD_ANIO")) %>% 
  select(-c(ID_CAIMAE, tipo_imae2)) %>% 
  mutate(
    fecha3=as.Date(paste(mes_solicitud, "-01", sep="")),
    arancel=case_when(
      fecha3<as.Date("2004-05-01") ~ 7573,
      as.Date("2004-05-01")<=fecha3 & fecha3<as.Date("2005-03-01") ~ 6384,
      as.Date("2005-03-01")<=fecha3 & fecha3<as.Date("2006-01-01") ~ 6304,
      as.Date("2006-01-01")<=fecha3 & fecha3<as.Date("2006-07-01") ~ 5907,
      as.Date("2006-07-01")<=fecha3 & fecha3<as.Date("2007-01-01") ~ 5852,
      as.Date("2007-01-01")<=fecha3 & fecha3<as.Date("2007-07-01") ~ 5979,
      as.Date("2007-07-01")<=fecha3 & fecha3<as.Date("2008-01-01") ~ 5647,
      as.Date("2008-01-01")<=fecha3 & fecha3<as.Date("2008-07-01") ~ 5612,
      as.Date("2008-07-01")<=fecha3 & fecha3<as.Date("2009-01-01") ~ 5483,
      as.Date("2009-01-01")<=fecha3 & fecha3<as.Date("2009-07-01") ~ 6054,
      as.Date("2009-07-01")<=fecha3 & fecha3<as.Date("2010-01-01") ~ 6103,
      as.Date("2010-01-01")<=fecha3 & fecha3<as.Date("2010-07-01") ~ 6027,
      as.Date("2010-07-01")<=fecha3 & fecha3<as.Date("2011-01-01") ~ 6216,
      as.Date("2011-01-01")<=fecha3 & fecha3<as.Date("2011-07-01") ~ 6230,
      as.Date("2011-07-01")<=fecha3 & fecha3<as.Date("2012-01-01") ~ 6347,
      as.Date("2012-01-01")<=fecha3 & fecha3<as.Date("2012-07-01") ~ 6227,
      as.Date("2012-07-01")<=fecha3 & fecha3<as.Date("2013-07-01") ~ 6442,
      as.Date("2013-07-01")<=fecha3 & fecha3<as.Date("2014-07-01") ~ 6531,
      as.Date("2014-07-01")<=fecha3 & fecha3<as.Date("2015-07-01") ~ 6585,
      as.Date("2015-07-01")<=fecha3 & fecha3<as.Date("2016-07-01") ~ 6522,
      as.Date("2016-07-01")<=fecha3 & fecha3<as.Date("2018-07-01") ~ 6553,
      as.Date("2018-07-01")<=fecha3 & fecha3<as.Date("2019-07-01") ~ 6462,
                      ))

# Logit_INGRESOS ----
Logit_INGRESOS <- 
  INGRESOS %>%
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
  left_join(X_imae, by=c("ID_CAIMAE"="CAIMAE", "mes_solicitud", "anio_solicitud")) %>% 
  left_join(MEDICOS, by=c("ZB1SMEDIC"="ZB1RMEDICO", "anio_solicitud"="PMDANIO")) %>%
  #left_join(delta_p, by=c("ZCAIMAE", "anio_solicitud"="anio")) %>% 
  left_join(dist, by=c("CAPACNUM", "num_choice", "id", "depto")) %>% 
  left_join(dist_sim, by=c("CAPACNUM", "num_choice", "id", "depto")) %>% 
  left_join(delta200_000, by=c("CAPACNUM", "anio_solicitud", "ZCAIMAE")) %>% 
  mutate(inst=case_when(ZCASINST==ZCASINST_IMAE~1,
                        is.na(ZCASINST_IMAE)~0,
                        .default = 0),
         medimae = 
           ifelse(rowSums(select(., starts_with("medimae")) == id, 
                          na.rm = TRUE) > 0, 1, 0),
         distk=dist/1000,
         distk_sim=dist_sim/1000) %>% 
  select(-c(matches("^medimae[0-9]+$"))) %>% 
  mutate(real_v_sim=distk_sim-distk,
         cap_op=turnos_op*hab_op,
         cong_op=n_centro/cap_op, 
         cong_op2=(cap_op-n_centro)) %>% 
  filter(!(ZCAIMAE=="SARI" & anio_solicitud>2013))

# Export ----
write_dta(Logit_INGRESOS, 
          "C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/MastersThesis/Logit_INGRESOS.dta",
          version = 14,
          label = attr(data, "label"),
          strl_threshold = 2045,
          adjust_tz = TRUE)



