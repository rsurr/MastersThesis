library(readr)
library(tidyverse)
library(haven)
library(fastDummies)
library(data.table)

SESIONES_HD <- read_sav("C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/Databases/SESIONES HD.sav") 

SESIONES_HD <- SESIONES_HD %>% 
  unite("fecha", PMD_ANIO:PMD_MES, sep="-", remove=FALSE) %>% 
  mutate(Date = as.Date(paste(fecha, "-01", sep="")))

SESIONES_HD <- SESIONES_HD %>% 
  mutate(
    ZPMD_IMAE=if_else(ZPMD_IMAE=="HOSPITAL ITALIANO", "UNIVERSAL", ZPMD_IMAE))

occupancy <- SESIONES_HD %>% select(ZPMD_IMAE, PMD_ANIO, PMD_MES, fecha, CAPACNUM, fecha) %>% 
  group_by(ZPMD_IMAE, PMD_ANIO, PMD_MES) %>% 
  summarise(n=n(), fecha=first(fecha)) %>% 
  ungroup() %>% 
  group_by(ZPMD_IMAE, PMD_ANIO) %>% 
  mutate(promedio_anual=mean(n, na.rm = T),
         dif=promedio_anual-n,
         dif_prop=dif/promedio_anual) %>% 
  rename(occ_var=dif,
         occ_var_prop=dif_prop) %>% 
  left_join(IMAE_num, by=c("ZPMD_IMAE"="ZCAIMAE")) %>% 
  filter(depto=="01") %>% 
  ungroup() %>% 
  mutate(ZCAIMAE=as.factor(choice)) %>% 
  select(occ_var, occ_var_prop, ZCAIMAE, fecha)

write.csv(occupancy, 
          "C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/MastersThesis/OCCUPANCY.csv", 
          row.names=FALSE)
