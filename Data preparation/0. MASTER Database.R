library(readr)
library(mlogit)
library(tidyverse)
library(peakRAM)
library(haven)
library(kableExtra)

setwd("C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/MastersThesis/")

source("C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/MastersThesis/Data preparation/1. Database INGRESOS_HD.R")
source("C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/MastersThesis/Data preparation/2. Database GEO.R")

INGRESOS_HD2 <- read_csv("INGRESOS_HD2.csv")
GEO <- read_csv("GEO.csv") %>% 
  select(CAPACNUM, lat_Google, long_Google,
         dist1, dist2, dist9, dist12, dist13, dist14, dist16,
         dist17, dist18, dist19, dist20, dist21, dist22, dist24, dist33, dist34,
         dist35, dist40)
IMAE_num <- read_csv("IMAE_num.csv") %>% filter(ZCAIMAE!="HOSPITAL ITALIANO")
occupancy <- read_csv("OCCUPANCY.CSV")
quality_reg <- read_csv("quality.csv")
MENSUALES_HD <- read_sav("~/Proyecto Tesis/Databases/MENSUALES HD.sav") %>% 
  select(CAPACNUM, PMD_IMAE, ZPMD_IMAE, PMD_ANIO, PMD_MES) %>% 
  mutate(
    ZPMD_IMAE=if_else(ZPMD_IMAE=="HOSPITAL ITALIANO", "UNIVERSAL", ZPMD_IMAE))
occupancy <- read_csv("OCCUPANCY.CSV")

# BASE INGRESOS
DATA_INGRESOS <- INGRESOS_HD2 %>% 
  left_join(GEO, by="CAPACNUM") %>% 
  left_join(IMAE_num, by=c("ZCAIMAE"="ZCAIMAE")) %>%
  #left_join(quality_reg, by=c("anio_solicitud"="anio")) %>%
  filter(depto=="01",
         ZCAIMAE!="SENNIAD HEMO") %>% 
  filter(!is.na(dist18))  %>% # FILTRO PROVISORIO SACANDO PACIENTES CON DIST NA (60 OBS)
  filter(!is.na(long_Google), !is.na(lat_Google),
         lat_Google<(-34.7),
         long_Google<(-56))

aborrar <- DATA_INGRESOS %>% select(starts_with("dist"))

mlogit_INGRESOS <- dfidx(DATA_INGRESOS, 
                 choice="choice",
                 idnames = c("CAPACNUM", "ZCAIMAE"),
                 shape = "long",
                 varying=grep('^medimae|^inst|^dist|^tipo_imae|^chain|^transp|^turnos', names(DATA_INGRESOS)), 
                 sep="")

library(peakRAM)
peakRAM({
  mlogitdta <- mlogit_INGRESOS %>% as.data.frame() %>% unnest_wider(idx)
  
  mlogitdta2 <- mlogitdta %>% 
    left_join(occupancy, by=c("ZCAIMAE", "mes_solicitud"="fecha")) %>%
    mutate(anio_solicitud=as.factor(anio_solicitud),
           ZCAIMAE=as.numeric(as.character(ZCAIMAE))) %>% 
    left_join(quality, by=c("ZCAIMAE", "anio_solicitud"="anio")) %>% 
    select(-c("depto.x", "depto.y", "tipo_choice.x", "tipo_choice.y"))
    
  
  write_dta(mlogitdta2, 
            "C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/MastersThesis/mlogitdta.dta",
            version = 14,
            label = attr(data, "label"),
            strl_threshold = 2045,
            adjust_tz = TRUE)})

no_prestador <- mlogit_INGRESOS %>% filter(tiene_imae==0, inst==1)

unique(DATAi$ZCAIMAE)

DATAm <- 
  left_join(MENSUALES_HD, INGRESOS_HD2, by=c("CAPACNUM")) %>%
  left_join(GEO, by="CAPACNUM") %>% 
  left_join(IMAE_num, by=c("ZPMD_IMAE"="ZCAIMAE")) %>%
  left_join(quality_reg, by=c("PMD_ANIO"="anio")) %>%
  filter(depto=="01",
         ZPMD_IMAE!="SENNIAD HEMO") %>% 
  filter(!is.na(dist18))  %>% # FILTRO PROVISORIO SACANDO PACIENTES CON DIST NA (60 OBS)
  filter(!is.na(long_Google), !is.na(lat_Google),
         lat_Google<(-34.7),
         long_Google<(-56))


aborrar <- DATA_INGRESOS %>% select(starts_with("turnos"))

DATA <- DATA %>% 
  mutate(ID=paste(CAPACNUM, PMD_ANIO, PMD_MES, PMD_IMAE, sep = "_"))


mlogit <- mlogit %>% mutate(inst=ifelse(is.na(inst), 0, inst),
                            medimae=ifelse(is.na(medimae), 0, medimae))

table(mlogit$inst, useNA = "always")
table(mlogit$medimae, useNA = "always")


mlogitdta <- mlogit %>% as.data.frame() %>% unnest_wider(idx)




write_dta(mlogitdta, 
          "C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/MastersThesis/mlogitdta.dta",
          version = 14,
          label = attr(data, "label"),
          strl_threshold = 2045,
          adjust_tz = TRUE)

library(peakRAM)
peakRAM({
  model1 <- mlogit(choice ~ inst + medimae + dist , data = mlogit)
})

summary(model1)

peakRAM({
  model1 <- mlogit(choice ~ dist + medimae + inst, 
                   data = mlogit1)
})

margins <- mlogit1dta %>% 
  group_by(IMAE) %>% 
  summarise(medimae=mean(medimae, na.rm = T),
            inst=mean(inst, na.rm = T),
            dist=mean(dist, na.rm = T)) %>% as.data.frame()

rownames(margins) <- margins$IMAE

margins <- margins %>% select(-IMAE)

margins_inst <- effects(model1, covariate = "inst", type = "aa", data = margins) %>% 
  as.data.frame()
margins_medimae <- effects(model1, covariate = "medimae", type = "aa", data = margins) %>% 
  as.data.frame()
margins_dist <- effects(model1, covariate = "dist", type = "aa", data = margins) %>% 
  as.data.frame()

margins_dist*1000

peakRAM({
  model0 <- mlogit(choice ~ dist + medimae + inst | 
                     CASEDADA + CASEXO + ZCASINST + ZB1SRAZA + ZB1SOCUP0 + B1SNIVEL, 
                   data = mlogit0)
})

model1 <- mlogit(choice ~ dist + medimae + inst | 
                  CASEDADA + CASEXO + ZCASINST +
                  ZCASDEPAR + ZB1SRAZA + ZB1SOCUP0 + B1SNIVEL, 
                data = mlogit1)

model0 <- mlogit(choice ~ dist + medimae + inst | 
                   CASEDADA + CASEXO + ZCASINST +
                   ZCASDEPAR + ZB1SRAZA + ZB1SOCUP0 + B1SNIVEL, 
                 data = mlogit0)

model <- mlogit(choice ~ dist, 
               data = mlogit)

summary(model)


#--------------------- 

result_tiene <- DATA_INGRESOS %>%
  filter(tiene_imae==1) %>% 
  group_by(ZCAIMAE) %>%
  summarise(Frequency = n()) %>%
  mutate(Percentage = round(Frequency / sum(Frequency) * 100, 2),
         ZCAIMAE=str_to_title(ZCAIMAE)) %>%
  arrange(desc(Frequency)) %>%
  add_row(ZCAIMAE = "Total", Frequency = sum(.$Frequency), Percentage = 100)

result_notiene <- DATA_INGRESOS %>%
  filter(tiene_imae==0) %>% 
  group_by(ZCAIMAE) %>%
  summarise(Frequency = n()) %>%
  mutate(Percentage = round(Frequency / sum(Frequency) * 100, 2),
         ZCAIMAE=str_to_title(ZCAIMAE)) %>%
  arrange(desc(Frequency)) %>%
  add_row(ZCAIMAE = "Total", Frequency = sum(.$Frequency), Percentage = 100) 

inner_join(result_tiene, result_notiene, by="ZCAIMAE") %>%
  kable("latex", booktabs = TRUE) %>% 
  writeLines("shares.tex")



