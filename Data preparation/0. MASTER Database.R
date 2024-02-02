library(readr)
library(mlogit)
library(tidyverse)
library(peakRAM)

source("1. Database INGRESOS_HD.R")
source("2. Database GEO.R")

INGRESOS_HD2 <- read_csv("INGRESOS_HD2.csv")
GEO <- read_csv("GEO.csv") %>% 
  select(CAPACNUM, 
         dist1, dist2, dist7, dist9, dist12, dist13, dist14, dist15, dist16,
         dist17, dist18, dist19, dist20, dist21, dist22, dist24, dist33, dist34,
         dist35, dist40)
IMAE_num <- read_csv("IMAE_num.csv")
quality <- read_csv("quality.csv")

DATA <- left_join(INGRESOS_HD2, GEO, by=c("CAPACNUM")) %>% 
  left_join(IMAE_num, by="ZCAIMAE") %>%
  left_join(quality, by=c("anio_solicitud"="anio")) %>%
  filter(depto=="01",
         anio_solicitud>2003)

test <- DATA %>% select(starts_with("quality"))

DATA1 <- DATA %>% 
  filter(tiene_imae==1) 
# %>% mutate_at(vars(starts_with(c("quality"))), ~replace(., is.na(.), 0))

DATA0 <- DATA %>% 
  filter(tiene_imae==0)
  
mlogit1 <- dfidx(DATA1, 
                 choice="choice",
                 idnames = c("CAPACNUM", "IMAE"),
                 shape = "long",
                 varying=grep('^medimae|^inst|^dist', names(DATA)), 
                 sep="") 

mlogit1dta <- mlogit1 %>% as.data.frame() %>% unnest_wider(idx)


write_dta(mlogit1dta, 
          "C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/MastersThesis/mlogit1dta.dta",
          version = 14,
          label = attr(data, "label"),
          strl_threshold = 2045,
          adjust_tz = TRUE)

mlogit0 <- dfidx(DATA0, idx="CAPACNUM", choice="choice", 
                 varying=grep('^dist|^medimae|^inst', names(DATA)), sep="")

library(peakRAM)
peakRAM({
  model1 <- mlogit(choice ~ inst + medimae + dist, data = mlogit1)
})

summary(model1)

peakRAM({
  model1 <- mlogit(choice ~ dist + medimae + inst, 
                   data = mlogit1)
})

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