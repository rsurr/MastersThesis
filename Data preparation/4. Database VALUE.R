library(tidyverse)
library(haven)
library(readr)

PACIENTES <- read_sav("C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/Databases/PACIENTES.sav")
MENSUALES_HD <- read_sav("C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/Databases/MENSUALES HD.sav") %>% 
  mutate_if(is.character, na_if,"") %>% 
  group_by(CAPACNUM) %>% 
  fill(DDIAB, DCISQ, DEVP) %>% 
  mutate(DDIAB=case_when(DDIAB=="D" ~ NA,
                         .default = as.character(DDIAB)),
         DCISQ=case_when(DCISQ=="D" ~ NA,
                         .default = as.character(DCISQ)),
         DEVP=case_when(DEVP=="D" ~ NA,
                        .default = as.character(DEVP)))

INGRESOS_HD2 <- read_csv("INGRESOS_HD2.csv")

base <- left_join(MENSUALES_HD, PACIENTES, by="CAPACNUM") %>% 
  left_join(INGRESOS_HD2, by="CAPACNUM") %>%
  left_join(IMAE_num, by="ZCAIMAE") %>% 
  filter(depto=="01") %>% 
  mutate(urea17=case_when(EMAZOEM<1.7 ~ 1,
                          EMAZOEM>=1.7 ~ 0),
         BMI=SCEFPE/(SCEFTA/100)**2)

# Reorder "anio" in increasing order
base$anio <- factor(base$PMD_ANIO, levels = sort(unique(base$PMD_ANIO)))

# Reorder "ZCAIMAE" alphabetically
base$ZCAIMAE <- factor(base$ZCAIMAE, levels = sort(unique(base$ZCAIMAE)))

# Set reference levels for anio and ZCAIMAE
base$anio <- relevel(base$anio, ref = "2004")
base$ZCAIMAE <- relevel(base$ZCAIMAE, ref = "ASOCIACION ESPAÑOLA")

# Fit the linear regression model
model <- lm(urea17 ~ CASEDADA + PAC_SEXO + ZB1SRAZA + DDIAB + DCISQ + DEVP + B1SNIVEL + ZCAIMAE:anio + CAPACNUM, data=base)

# Use tidy() to extract coefficients
quality <- tidy(model) %>%
  select(term, estimate) %>% 
  filter(str_detect(term, "ZCAIMAE") & str_detect(term, ":anio")) %>%
  separate(term, into = c("ZCAIMAE", "anio"), sep = ":") %>%
  mutate(anio = gsub("anio", "", anio),
         ZCAIMAE = gsub("ZCAIMAE", "", ZCAIMAE)) %>% 
  pivot_wider(names_from = "anio", values_from = "estimate")

coefs <- tidy(model) %>%
  select(term, estimate) %>% 
  filter(str_detect(term, "ZCAIMAE") & str_detect(term, ":anio")) %>%
  separate(term, into = c("ZCAIMAE", "anio"), sep = ":") %>%
  mutate(anio = gsub("anio", "", anio),
         ZCAIMAE = gsub("ZCAIMAE", "", ZCAIMAE)) %>%
  left_join(IMAE_num, by="ZCAIMAE") %>% 
  rename(quality=choice) %>%
  select(anio, estimate, quality) %>% 
  pivot_wider(names_from = "quality", values_from = "estimate", names_prefix = "quality") %>% 
  select(anio,
         quality1, quality2, quality7, quality9, quality12, 
         quality13, quality14, quality15, quality16,
         quality17, quality18, quality19, quality20, 
         quality21, quality22, quality24, quality33, quality34,
         quality35, quality40)

write.csv(
  quality,
  "C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/MastersThesis/quality.csv", 
          row.names=FALSE)
