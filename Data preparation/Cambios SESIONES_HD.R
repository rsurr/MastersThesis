library(tidyverse)

SESIONES_HD <- 
  read_sav("C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/Databases/SESIONES HD.sav") 


IMAE_ingreso <- INGRESOS_HD2 %>% rename(IMAE_ingreso=ZCAIMAE)

cambios <- SESIONES_HD %>% 
  group_by(CAPACNUM, ZPMD_IMAE) %>%
  summarize(n=n()) %>% 
  group_by(CAPACNUM) %>% 
  mutate(prop=n/sum(n),
         max_prop=max(prop),
         imae_max=if_else(prop==max_prop, 1, 0)) %>% 
  filter(imae_max==1) %>% 
  right_join(IMAE_ingreso, by="CAPACNUM") %>% 
  mutate(ingreso_max=if_else(ZPMD_IMAE==IMAE_ingreso, 1, 0))
  
summary(cambios$ingreso_max)

cambios_noMAX <- cambios %>% filter(ingreso_max==0)
cambios_MAX <- cambios %>% filter(ingreso_max==1)

summary(cambios_noMAX$imae_inst)
summary(cambios_MAX$imae_inst)


cambios %>% filter(m>1)

summary(cambios_I$m)

cambios_I <- INGRESOS_HD %>% 
  group_by(CAPACNUM, ZCAIMAE) %>%
  summarize(n=n()) %>% 
  group_by(CAPACNUM) %>% 
  mutate(m=n())


  

SESIONES_HD$ZPMD_IMAE