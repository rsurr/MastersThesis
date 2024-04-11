library(readr)
library(tidyverse)
library(haven)
library(fastDummies)
library(data.table)

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
