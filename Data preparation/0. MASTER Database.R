library(readr)
library(mlogit)
library(tidyverse)
library(peakRAM)
library(haven)

source("1. Database INGRESOS_HD.R")
source("2. Database GEO.R")

INGRESOS_HD2 <- read_csv("INGRESOS_HD2.csv")
GEO <- read_csv("GEO.csv") %>% 
  select(CAPACNUM, 
         dist1, dist2, dist7, dist9, dist12, dist13, dist14, dist15, dist16,
         dist17, dist18, dist19, dist20, dist21, dist22, dist24, dist33, dist34,
         dist35, dist40)
IMAE_num <- read_csv("IMAE_num.csv")
quality_reg <- read_csv("quality.csv")

DATA <- left_join(INGRESOS_HD2, GEO, by=c("CAPACNUM")) %>% 
  left_join(IMAE_num, by="ZCAIMAE") %>%
  left_join(quality_reg, by=c("anio_solicitud"="anio")) %>%
  filter(depto=="01") %>% 
  filter(!is.na(dist18)) # FILTRO PROVISORIO SACANDO PACIENTES CON DIST NA (60 OBS)




DATA1 <- DATA %>% 
  filter(tiene_imae==1) 
# %>% mutate_at(vars(starts_with(c("quality"))), ~replace(., is.na(.), 0))

DATA0 <- DATA %>% 
  filter(tiene_imae==0)

DATAP <- DATA %>% 
  filter(tipo_imae=="PUBLICO")

DATAPr <- DATA %>% 
  filter(tipo_imae=="PRIVADO")

DATAI <- DATA %>% 
  filter(tipo_imae=="INDEPENDIENTE")

mlogit <- dfidx(DATA, 
                 choice="choice",
                 idnames = c("CAPACNUM", "IMAE"),
                 shape = "wide",
                 varying=grep('^medimae|^inst|^dist|^quality', names(DATA)), 
                 sep="")   

mlogitP <- dfidx(DATAP, 
                 choice="choice",
                 idnames = c("CAPACNUM", "IMAE"),
                 shape = "wide",
                 varying=grep('^medimae|^inst|^dist|^quality', names(DATAP)), 
                 sep="") 

mlogitPr <- dfidx(DATAPr, 
                 choice="choice",
                 idnames = c("CAPACNUM", "IMAE"),
                 shape = "wide",
                 varying=grep('^medimae|^inst|^dist|^quality', names(DATAPr)), 
                 sep="")

mlogitI <- dfidx(DATAI, 
                  choice="choice",
                  idnames = c("CAPACNUM", "IMAE"),
                  shape = "wide",
                  varying=grep('^medimae|^inst|^dist|^quality', names(DATAI)), 
                  sep="")

mlogit1dta <- mlogit1 %>% as.data.frame() %>% unnest_wider(idx)
mlogit0dta <- mlogit0 %>% as.data.frame() %>% unnest_wider(idx)
mlogitPdta <- mlogitP %>% as.data.frame() %>% unnest_wider(idx)
mlogitPrdta <- mlogitPr %>% as.data.frame() %>% unnest_wider(idx)
mlogitIdta <- mlogitI %>% as.data.frame() %>% unnest_wider(idx)
mlogitdta <- mlogit %>% as.data.frame() %>% unnest_wider(idx) %>%
  mutate(drop= ifelse((dist > 500000 & choice == 1), 1, 0)) %>% 
  group_by(CAPACNUM) %>%
  mutate(drop=max(drop)) %>% 
  filter(drop==0)

chosen <- mlogitdta %>% filter(choice==1)

tail(table(chosen$dist), n=60)

write_dta(mlogit1dta, 
          "C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/MastersThesis/mlogit1dta.dta",
          version = 14,
          label = attr(data, "label"),
          strl_threshold = 2045,
          adjust_tz = TRUE)

write_dta(mlogit0dta, 
          "C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/MastersThesis/mlogit0dta.dta",
          version = 14,
          label = attr(data, "label"),
          strl_threshold = 2045,
          adjust_tz = TRUE)

write_dta(mlogitPdta, 
          "C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/MastersThesis/mlogitPdta.dta",
          version = 14,
          label = attr(data, "label"),
          strl_threshold = 2045,
          adjust_tz = TRUE)

write_dta(mlogitPrdta, 
          "C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/MastersThesis/mlogitPrdta.dta",
          version = 14,
          label = attr(data, "label"),
          strl_threshold = 2045,
          adjust_tz = TRUE)

write_dta(mlogitIdta, 
          "C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/MastersThesis/mlogitIdta.dta",
          version = 14,
          label = attr(data, "label"),
          strl_threshold = 2045,
          adjust_tz = TRUE)

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


#--------------------- Muestra

DATA1m <- left_join(INGRESOS_HD2, GEO, by=c("CAPACNUM")) %>% 
  left_join(IMAE_num, by="ZCAIMAE") %>% 
  filter(tiene_imae==1) %>% head(n=500)

mlogit1m <- dfidx(DATA1m, idx="CAPACNUM", choice="choice", 
                  varying=grep('^dist|^medimae|^inst', names(DATA)), sep="")


peakRAM({
  model1m <- mlogit(choice ~ dist + medimae + inst | 
                     CASEDADA + CASEXO + ZCASINST +
                     ZCASDEPAR + ZB1SRAZA + ZB1SOCUP0 + B1SNIVEL, 
                   data = mlogit1m)
})
