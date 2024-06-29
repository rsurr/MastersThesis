library(tidyverse)
library(haven)

## MENSUALES
MENSUALES_HD <- read_sav("Proyecto Tesis/Databases/MENSUALES HD.sav")

## SESIONES
SESIONES_HD <- read_sav("Proyecto Tesis/Databases/SESIONES HD.sav")

# cod_septi <- c(184010162, 184010183, 184010159, 184010160, 184010161)

#shares <- SESIONES_HD %>% 
#  left_join(MENSUALES_HD, by=c("PMD_IMAE", "PMD_ANIO", "PMD_MES")) 

cod_septi <- c(184010162, 184010183, 184010159, 184010160, 184010161)

shares  <- MENSUALES_HD %>% 
  mutate(complicacion=if_else(COMP=="S", 1, 0),
         muerte=if_else(ZPMD_ESTADO=="FALLECIMIENTO", 1, 0),
         septi=if_else(COMP1 %in% cod_septi |
                         COMP2 %in% cod_septi |
                         COMP3 %in% cod_septi, 1, 0)) %>% 
  group_by(PMD_IMAE, PMD_ANIO, PMD_MES) %>% 
  summarise(n_IMAE=length(unique(CAPACNUM)),
            hemo=mean(EMHEMOG, na.rm=T),
            urea=mean(EMAZOEM, na.rm=T),
            comp=mean(complicacion, na.rm=T),
            muerte=mean(muerte, na.rm=T),
            septi=mean(septi, na.rm=T)) %>% 
  group_by(PMD_ANIO, PMD_MES) %>% 
  mutate(n_TOTAL=sum(n_IMAE))

HD <- read_excel("Seminario/HD.xlsx", col_types = c("numeric", "numeric", "numeric"))


#shares  <- SESIONES_HD %>% 
#  group_by(PMD_IMAE, PMD_ANIO, PMD_MES) %>% 
#    mutate(reut=if_else(DUSOF=="Re uso", 1, 0)) %>% 
#    summarise(n_IMAE=length(unique(CAPACNUM)),
#              prices=mean(reut)) 

shares <- shares %>% left_join(HD, by="PMD_ANIO") %>% 
  group_by(PMD_ANIO, PMD_MES) %>% 
  mutate(n_TOTAL=(100/porcentajeHD)*sum(n_IMAE), 
         shares=n_IMAE/n_TOTAL, market_ids=paste0(PMD_ANIO, PMD_MES)) %>% 
  select(-c(n_TOTAL, n_IMAE, porcentajeHD, n))


write.csv(shares, "C:/Users/julie/OneDrive/Documentos/Seminario/BLP/shares.csv", row.names=FALSE)


library(haven)
MENSUALES_DPCA <- read_sav("Proyecto Tesis/Databases/MENSUALES DPCA.sav")
View(MENSUALES_DPCA)

unique(MENSUALES_HD$ZPMD_IMAE)

unique(MENSUALES_DPCA$ZPMD_IMAE)


DIR <- PACIENTES %>% select(CAPACNUM,
                            PAC_TV_ID, PAC_TV_DESC, PAC_DIRECCION, PAC_NPTA, PAC_DEP_ID,
                            PAC_DEP_DESC, PAC_LOC_ID, PAC_LOC_DESC, 
                            PAC_LOCALIDAD, PAC_CP) %>%
  filter(CAPACNUM %in% MENSUALES_HD$CAPACNUM)

length(unique(DIR$CAPACNUM))

write.csv(DIR, "C:/Users/julie/OneDrive/Documentos/Seminario/BLP/dir.csv", row.names=FALSE)

length(unique(MENSUALES_HD$CAPACNUM))

write.xls(DIR, "C:/Users/julie/OneDrive/Documentos/Seminario/BLP/dir.xls", row.names=FALSE)


