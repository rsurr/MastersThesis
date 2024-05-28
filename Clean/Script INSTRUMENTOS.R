my.max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)

#MENSUALES_HD <- read_sav("~/Proyecto Tesis/Databases/MENSUALES HD.sav")

MENSUALES_HD2 <- MENSUALES_HD %>% 
  left_join(PACIENTES, by="CAPACNUM") %>%
  unite("mes_mensual", c(PMD_ANIO,PMD_MES), sep="-", remove=FALSE) %>% 
  mutate(fecha_mensual = as.Date(paste(mes_mensual, "-01", sep=""))) %>% 
  filter(ZPMD_IMAE!="SENNIAD HEMO",
         ZPMD_IMAE!="IMAE A CONFIRMAR") %>% 
  mutate(
    edad = as.numeric(difftime(fecha_mensual, PAC_FEC_NAC, 
                               units = "days") / 365.25),
    ZPMD_IMAE=if_else(ZPMD_IMAE=="HOSPITAL ITALIANO", "UNIVERSAL", ZPMD_IMAE),
    ZPMD_IMAE=if_else(ZPMD_IMAE=="IMPASA", "SMI - SERVICIO MEDICO INTEGRAL", ZPMD_IMAE),
    ZPMD_IMAE=if_else(ZPMD_IMAE=="CEDINA", "CE.DI.SA.", ZPMD_IMAE),
    #Date = as.Date(paste(fecha, "-01", sep="")),
    #mes=format(Date, "%Y-%m"),
    DDIAB=case_when(
      DDIAB=="S" ~ 1,
      DDIAB=="N" ~ 0,
      DDIAB=="D" ~ as.numeric(NA),
      .default = as.numeric(NA)),
    DCISQ=case_when(
      DCISQ=="S" ~ 1,
      DCISQ=="N" ~ 0,
      DCISQ=="D" ~ as.numeric(NA),
      .default = as.numeric(NA)),
    DEVP=case_when(
      DEVP=="S" ~ 1,
      DEVP=="N" ~ 0,
      DEVP=="D" ~ as.numeric(NA),
      .default = as.numeric(NA)),
    DTRABA=case_when(
      DTRABA=="S" ~ 1,
      DTRABA=="N" ~ 0,
      DTRABA=="D" ~ as.numeric(NA),
      .default = as.numeric(NA)),
    DFUMA=case_when(
      DFUMA=="S" ~ 1,
      DFUMA=="N" ~ 0,
      DFUMA=="D" ~ as.numeric(NA),
      .default = as.numeric(NA)),
    #ASSE=case_when(
    #  tipo_inst=="ASSE" ~ 1,
    #  .default = 0),
    #IAMC_IAMPP=case_when(
    #  tipo_inst=="IAMC/IAMPP" ~ 1,
    #  .default = 0),
    #CORPORATIVO=case_when(
    #  tipo_inst=="CORPORATIVO" ~ 1,
    #  .default = 0),
    #PRIVADO=case_when(
    #  tipo_inst=="SEGURO PRIVADO" ~ 1,
    #  .default = 0),
    mujer=case_when(
      PAC_SEXO=="F" ~ 1,
      .default = 0),
    #descom=case_when(
    #  descom=="S" ~ 1,
    #  descom=="N" ~ 0,
    #  descom=="D" ~ 0),
    #coord=case_when(
    #  coord=="S" ~ 1,
    #  coord=="N" ~ 0,
    #  .default = 0),
    URR=if_else(!is.na(ESAZOPR1), 
                (ESAZOPR1-ESAZOPO1)/ESAZOPR1,
                (ESAZOPR2-ESAZOPO1)/ESAZOPR2),
    URR65=case_when(URR>=0.65 ~ 1,
                    URR<0.65 ~ 0),
    urea17=case_when(EMAZOEM<1.7 ~ 1,
                     EMAZOEM>=1.7 ~ 0),
    fosf55=case_when(ESFOSF<5.5 ~ 1,
                     ESFOSF>=5.5 ~ 0),
    hemo10=case_when(EMHEMOG>=10 ~ 1,
                     EMHEMOG<10 ~ 0),
    surv=case_when(ZPMD_ESTADO!="FALLECIMIENTO" ~ 1,
                   ZPMD_ESTADO=="FALLECIMIENTO" ~ 0),
    comp=case_when(COMP=="S" ~ 0,
                   COMP=="N" ~ 1,
                   COMP=="D" | COMP=="D" | COMP==NA ~ NA),
    sept=case_when(COMP1 %in% c(184010162, 184010183, 184010159, 184010160, 184010161) |
                     COMP2 %in% c(184010162, 184010183, 184010159, 184010160, 184010161) |
                     COMP3 %in% c(184010162, 184010183, 184010159, 184010160, 184010161) ~ 0,
                   COMP=="N" ~ 1,
                   COMP=="D" | COMP=="D" | COMP==NA ~ NA),
    ktv12=case_when(ESKTV>=1.2 ~ 1,
                    ESKTV<1.2 ~ 0)
  ) %>%
  group_by(CAPACNUM, PMD_ANIO) %>% 
  mutate(
    DDIAB=my.max(DDIAB),
    DCISQ=my.max(DCISQ),
    DEVP=my.max(DEVP),
    DTRABA=my.max(DTRABA),
    DFUMA=my.max(DFUMA),
  ) %>%
  ungroup() %>% 
  group_by(CAPACNUM) %>% 
  arrange(PMD_ANIO, PMD_MES) %>% 
  mutate(meses=row_number())

INSTRUMENTOS <- MENSUALES_HD2 %>% 
  group_by(PMD_ANIO, ZPMD_IMAE) %>% 
  filter(PMD_ORIG=="Del centro") %>% 
  summarise(
    n_pac=n_distinct(CAPACNUM),
    
    n_diab=n_distinct(CAPACNUM[DDIAB==1]),
    n_dcisq=n_distinct(CAPACNUM[DCISQ==1]),
    n_devp=n_distinct(CAPACNUM[DEVP==1]),
    n_dtraba=n_distinct(CAPACNUM[DTRABA==1]),
    n_dfuma=n_distinct(CAPACNUM[DFUMA==1]),
    
    n_edad50=n_distinct(CAPACNUM[edad>50]),
    n_edad60=n_distinct(CAPACNUM[edad>60]),
    n_edad70=n_distinct(CAPACNUM[edad>70]),
    n_edad80=n_distinct(CAPACNUM[edad>80]),
    
    p_diab=n_diab/n_pac,
    p_dcisq=n_dcisq/n_pac,
    p_devp=n_devp/n_pac,
    p_dtraba=n_dtraba/n_pac,
    p_dfuma=n_dfuma/n_pac,
    
    p_edad50=n_edad50/n_pac,
    p_edad60=n_edad60/n_pac,
    p_edad70=n_edad70/n_pac,
    p_edad80=n_edad80/n_pac
            )
  
