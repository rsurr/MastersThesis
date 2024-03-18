library(readr)
library(mlogit)
library(tidyverse)
library(peakRAM)
library(haven)
library(kableExtra)

source("1. Database INGRESOS_HD.R")
source("2. Database GEO.R")

INGRESOS_HD2 <- read_csv("INGRESOS_HD2.csv")
GEO <- read_csv("GEO.csv") %>% 
  select(CAPACNUM, lat_Google, long_Google,
         dist1, dist2, dist7, dist9, dist12, dist13, dist14, dist15, dist16,
         dist17, dist18, dist19, dist20, dist21, dist22, dist24, dist33, dist34,
         dist35, dist40)
IMAE_num <- read_csv("IMAE_num.csv")
quality_reg <- read_csv("quality.csv")
MENSUALES_HD <- read_sav("~/Proyecto Tesis/Databases/MENSUALES HD.sav")

DATA <- 
  left_join(MENSUALES_HD, INGRESOS_HD2, by=c("CAPACNUM", "PMD_ANIO"="anio_solicitud")) %>% 
  left_join(GEO, by="CAPACNUM") %>% 
  left_join(IMAE_num, by=c("ZPMD_IMAE"="ZCAIMAE")) %>%
  left_join(quality_reg, by=c("PMD_ANIO"="anio")) %>%
  filter(depto=="01",
         ZPMD_IMAE!="SENNIAD HEMO") %>% 
  filter(!is.na(dist18)) # FILTRO PROVISORIO SACANDO PACIENTES CON DIST NA (60 OBS)

DATA <- DATA %>% 
  mutate(ID=paste(CAPACNUM, PMD_ANIO, PMD_MES, PMD_IMAE, sep = "_"))

n_distinct(DATA$ID)

mlogit <- dfidx(DATA, 
                 choice="choice",
                idnames = c("ID", "PMD_IMAE"),
                 shape = "wide",
                 varying=grep('^medimae|^inst|^dist|^quality|^tipo_imae', names(DATA)), 
                 sep="")   

mlogitdta <- mlogit %>% as.data.frame() %>% unnest_wider(idx)

mlogitdta %>% filter(choice==1, tiene_imae==1) %>% 
  summarize(inst=mean(inst))

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

result_tiene <- DATA %>%
  filter(tiene_imae==1) %>% 
  group_by(ZCAIMAE) %>%
  summarise(Frequency = n()) %>%
  mutate(Percentage = round(Frequency / sum(Frequency) * 100, 2),
         ZCAIMAE=str_to_title(ZCAIMAE)) %>%
  arrange(desc(Frequency)) %>%
  add_row(ZCAIMAE = "Total", Frequency = sum(.$Frequency), Percentage = 100)

result_notiene <- DATA %>%
  filter(tiene_imae==0) %>% 
  group_by(ZCAIMAE) %>%
  summarise(Frequency = n()) %>%
  mutate(Percentage = round(Frequency / sum(Frequency) * 100, 2),
         ZCAIMAE=str_to_title(ZCAIMAE)) %>%
  arrange(desc(Frequency)) %>%
  add_row(ZCAIMAE = "Total", Frequency = sum(.$Frequency), Percentage = 100) 

inner_join(result_tiene, result_notiene, by="ZCAIMAE") %>%
  kable("latex", booktabs = TRUE) %>% 
  writeLines("table.tex")

DATA$
