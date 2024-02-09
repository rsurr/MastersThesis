library(tidyverse)
library(haven)
library(readr)
library(fastDummies)

PACIENTES <- read_sav("C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/Databases/PACIENTES.sav")
MENSUALES_HD <- read_sav("C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/Databases/MENSUALES HD.sav")
INGRESOS_HD2 <- read_csv("INGRESOS_HD2.csv")

dife_peso <- SESIONES_HD %>% 
  mutate(DPESOSE=case_when(DPESOSE==999.00 ~ NA,
                           .default = as.numeric(DPESOSE)),
         DPESOPO=case_when(DPESOPO==999.00 ~ NA,
                           .default = as.numeric(DPESOPO)),
         dife=DPESOPO-DPESOSE) %>% 
  group_by(PMD_ANIO, PMD_MES, CAPACNUM) %>% 
  summarize(dife=mean(dife)) %>% 
  mutate(peso=case_when(abs(dife)<=0.5 ~ 1,
                        abs(dife)>0.5 ~ 0,
                        is.na(dife) ~ NA))

cod_sept <- c(184010162, 184010183, 184010159, 184010160, 184010161)

base <- left_join(MENSUALES_HD, PACIENTES, by="CAPACNUM") %>% 
  left_join(INGRESOS_HD2, by="CAPACNUM") %>%
  left_join(IMAE_num, by="ZCAIMAE") %>% 
  left_join(dife_peso, by=c("PMD_ANIO", "PMD_MES", "CAPACNUM")) %>% 
  filter(depto=="01",
         ZCAIMAE!="SENNIAD HEMO") %>%
  mutate_if(is.character, na_if,"") %>% 
  group_by(CAPACNUM, PMD_ANIO) %>% 
  fill(DDIAB, DCISQ, DEVP) %>% 
  ungroup() %>% 
  mutate(DDIAB=case_when(DDIAB=="D" ~ NA,
                         .default = as.character(DDIAB)),
         DCISQ=case_when(DCISQ=="D" ~ NA,
                         .default = as.character(DCISQ)),
         DEVP=case_when(DEVP=="D" ~ NA,
                        .default = as.character(DEVP)),
         ZB1SRAZA=case_when(ZB1SRAZA=="S/D" ~ NA,
                            ZB1SRAZA=="MULATA" ~ "NEGRA",
                            .default = as.character(ZB1SRAZA)),
         B1SNIVEL=case_when(B1SNIVEL=="SD" ~ NA,
                            B1SNIVEL=="UTU" ~ "Secundaria",
                            .default = as.character(B1SNIVEL)),
         PAC_SEXO=case_when(PAC_SEXO=="U" ~ NA,
                            .default = as.character(PAC_SEXO)),
         ocupado=case_when(ZB1SOCUP0=="Estudiante" |
                             ZB1SOCUP0=="Jubilado" |
                             ZB1SOCUP0=="Menor dependiente (< 15 años)" |
                             ZB1SOCUP0=="Sin ocupación" |
                             ZB1SOCUP0=="Tareas del hogar" ~ 0,
                           ZB1SOCUP0=="sd" ~ NA,
                            .default = 1)) %>% 
  mutate(urea17=case_when(EMAZOEM<1.7 ~ 1,
                          EMAZOEM>=1.7 ~ 0),
         BMI=SCEFPE/(SCEFTA/100)**2,
         fosf55=case_when(ESFOSF<5.5 ~ 1,
                          ESFOSF>=5.5 ~ 0),
         #pas140=case_when(EMAZOEM<1.7 ~ 1,
         #                 EMAZOEM>=1.7 ~ 0),
         hemo10=case_when(EMHEMOG>=10 ~ 1,
                          EMHEMOG<10 ~ 0),
         surv=case_when(ZPMD_ESTADO!="FALLECIMIENTO" ~ 1,
                         ZPMD_ESTADO=="FALLECIMIENTO" ~ 0),
         comp=case_when(COMP=="S" ~ 1,
                        COMP=="N" ~ 0,
                        COMP=="D" | COMP=="D" | COMP==NA ~ NA),
         sept=case_when(COMP1 %in% cod_sept |
                         COMP2 %in% cod_sept |
                         COMP3 %in% cod_sept ~ 0,
                         COMP=="N" ~ 1,
                         COMP=="D" | COMP=="D" | COMP==NA ~ NA),
         URR=if_else(!is.na(ESAZOPR1), 
                     (ESAZOPR1-ESAZOPO1)/ESAZOPR1,
                     (ESAZOPR2-ESAZOPO1)/ESAZOPR2),
         URR65=case_when(URR>=0.65 ~ 1,
                         URR<0.65 ~ 0),
         ktv12=case_when(ESKTV>=1.2 ~ 1,
                         ESKTV<1.2 ~ 0)) %>% 
  dummy_cols(select_columns = c("PAC_SEXO", "ZB1SRAZA", 
                                "DDIAB", "DCISQ", "DEVP", "B1SNIVEL",
                                "tipo_inst"),
             ignore_na = T) %>%
  rename(tipo_inst_IAMCIAMPP=`tipo_inst_IAMC/IAMPP`,
         tipo_inst_SEGUROPRIVADO=`tipo_inst_SEGURO PRIVADO`) %>%
  ungroup() %>% 
  group_by(CAPACNUM) %>% 
  arrange(PMD_ANIO, PMD_MES) %>% 
  mutate(meses=row_number())

# Reorder "anio" in increasing order
base$anio <- factor(base$PMD_ANIO, levels = sort(unique(base$PMD_ANIO)))

# Reorder "ZCAIMAE" alphabetically
base$ZCAIMAE <- factor(base$ZCAIMAE, levels = sort(unique(base$ZCAIMAE)))

# Set reference levels for anio and ZCAIMAE
base$anio <- relevel(base$anio, ref = "2004")
base$ZCAIMAE <- relevel(base$ZCAIMAE, ref = "ASOCIACION ESPAÑOLA")

# Fit the linear regression model
m_urea <- lm(urea17 ~ - 1 + 
               CASEDADA + meses + ocupado +
               PAC_SEXO_F + 
               ZB1SRAZA_NEGRA + ZB1SRAZA_OTRA +
               DDIAB_S + DCISQ_S + DEVP_S + ECREAV + 
               B1SNIVEL_Primaria + B1SNIVEL_Secundaria + B1SNIVEL_Universidad +
               tipo_inst_IAMCIAMPP + tipo_inst_SEGUROPRIVADO + tipo_inst_CORPORATIVO +
               ZCAIMAE:anio, 
             data=base)

m_hemo <- lm(hemo10 ~ - 1 + 
               CASEDADA + meses + ocupado +
               PAC_SEXO_F + 
               ZB1SRAZA_NEGRA + ZB1SRAZA_OTRA +
               DDIAB_S + DCISQ_S + DEVP_S + ECREAV + 
               B1SNIVEL_Primaria + B1SNIVEL_Secundaria + B1SNIVEL_Universidad +
               tipo_inst_IAMCIAMPP + tipo_inst_SEGUROPRIVADO + tipo_inst_CORPORATIVO +
               ZCAIMAE:anio, 
             data=base)

m_fosf <- lm(fosf55 ~ - 1 + 
               CASEDADA + meses + ocupado +
               PAC_SEXO_F + 
               ZB1SRAZA_NEGRA + ZB1SRAZA_OTRA +
               DDIAB_S + DCISQ_S + DEVP_S + ECREAV + 
               B1SNIVEL_Primaria + B1SNIVEL_Secundaria + B1SNIVEL_Universidad +
               tipo_inst_IAMCIAMPP + tipo_inst_SEGUROPRIVADO + tipo_inst_CORPORATIVO +
               ZCAIMAE:anio, 
             data=base)

m_surv <- lm(surv ~ - 1 + 
                CASEDADA + meses + ocupado +
                PAC_SEXO_F + 
                ZB1SRAZA_NEGRA + ZB1SRAZA_OTRA +
                DDIAB_S + DCISQ_S + DEVP_S + ECREAV + 
                B1SNIVEL_Primaria + B1SNIVEL_Secundaria + B1SNIVEL_Universidad +
                tipo_inst_IAMCIAMPP + tipo_inst_SEGUROPRIVADO + tipo_inst_CORPORATIVO +
                ZCAIMAE:anio, 
              data=base)

m_comp <- lm(comp ~ - 1 + 
               CASEDADA + meses + ocupado +
               PAC_SEXO_F + 
               ZB1SRAZA_NEGRA + ZB1SRAZA_OTRA +
               DDIAB_S + DCISQ_S + DEVP_S + ECREAV + 
               B1SNIVEL_Primaria + B1SNIVEL_Secundaria + B1SNIVEL_Universidad +
               tipo_inst_IAMCIAMPP + tipo_inst_SEGUROPRIVADO + tipo_inst_CORPORATIVO +
               ZCAIMAE:anio, 
             data=base)

m_sept <- lm(sept ~ - 1 + 
               CASEDADA + meses + ocupado +
               PAC_SEXO_F + 
               ZB1SRAZA_NEGRA + ZB1SRAZA_OTRA +
               DDIAB_S + DCISQ_S + DEVP_S + ECREAV + 
               B1SNIVEL_Primaria + B1SNIVEL_Secundaria + B1SNIVEL_Universidad +
               tipo_inst_IAMCIAMPP + tipo_inst_SEGUROPRIVADO + tipo_inst_CORPORATIVO +
               ZCAIMAE:anio, 
             data=base)

m_peso <- lm(peso ~ - 1 + 
               CASEDADA + meses + ocupado +
               PAC_SEXO_F + 
               ZB1SRAZA_NEGRA + ZB1SRAZA_OTRA +
               DDIAB_S + DCISQ_S + DEVP_S + ECREAV + 
               B1SNIVEL_Primaria + B1SNIVEL_Secundaria + B1SNIVEL_Universidad +
               tipo_inst_IAMCIAMPP + tipo_inst_SEGUROPRIVADO + tipo_inst_CORPORATIVO +
               ZCAIMAE:anio, 
             data=base)

m_URR <- lm(URR65 ~ - 1 + 
               CASEDADA + meses + ocupado +
               PAC_SEXO_F + 
               ZB1SRAZA_NEGRA + ZB1SRAZA_OTRA +
               DDIAB_S + DCISQ_S + DEVP_S + ECREAV + 
               B1SNIVEL_Primaria + B1SNIVEL_Secundaria + B1SNIVEL_Universidad +
               tipo_inst_IAMCIAMPP + tipo_inst_SEGUROPRIVADO + tipo_inst_CORPORATIVO +
               ZCAIMAE:anio, 
             data=base)

m_ktv <- lm(ktv12 ~ - 1 + 
              CASEDADA + meses + ocupado +
              PAC_SEXO_F + 
              ZB1SRAZA_NEGRA + ZB1SRAZA_OTRA +
              DDIAB_S + DCISQ_S + DEVP_S + ECREAV + 
              B1SNIVEL_Primaria + B1SNIVEL_Secundaria + B1SNIVEL_Universidad +
              tipo_inst_IAMCIAMPP + tipo_inst_SEGUROPRIVADO + tipo_inst_CORPORATIVO +
              ZCAIMAE:anio, 
            data=base)

library(broom)
coef_ktv <- tidy(m_ktv) %>%
  select(term, estimate) %>% 
  filter(str_detect(term, "ZCAIMAE") & str_detect(term, ":anio")) %>%
  separate(term, into = c("ZCAIMAE", "anio"), sep = ":") %>%
  mutate(anio = gsub("anio", "", anio),
         IMAE = gsub("ZCAIMAE", "", ZCAIMAE)) %>% 
  rename(ktv=estimate) %>% 
  select(anio, ktv, IMAE) %>% 
  pivot_wider(names_from = "anio", values_from = "ktv")

left_join(tipo_imae, by=c("ZCAIMAE")) %>%
  mutate(tipo_imae2=case_when(tipo_imae=="INDEPENDIENTE" ~ "Indep",
                              tipo_imae=="PRIVADO" ~ "Netwk",
                              tipo_imae=="PUBLICO" ~ "Public"))


tipo_imae <- INGRESOS_HD2 %>% ungroup() %>% select("ZCAIMAE", "tipo_imae") %>% unique()

pred_urea <- get_all_predictions(m_urea, base) %>% rename(urea=Prediction)
pred_surv <- get_all_predictions(m_surv, base) %>% rename(surv=Prediction)
pred_fosf <- get_all_predictions(m_fosf, base) %>% rename(fosf=Prediction)
pred_hemo <- get_all_predictions(m_hemo, base) %>% rename(hemo=Prediction)
pred_comp <- get_all_predictions(m_comp, base) %>% rename(comp=Prediction)
pred_sept <- get_all_predictions(m_sept, base) %>% rename(sept=Prediction)
pred_peso <- get_all_predictions(m_peso, base) %>% rename(peso=Prediction)
pred_URR <- get_all_predictions(m_URR, base) %>% rename(URR=Prediction)
pred_ktv <- get_all_predictions(m_ktv, base) %>% rename(ktv=Prediction)


# %>% pivot_wider(names_from = "anio", values_from = "Prediction")

quality <- left_join(pred_urea, pred_surv, by=c("IMAE", "anio")) %>% 
  left_join(pred_fosf, by=c("IMAE", "anio")) %>%
  left_join(pred_hemo, by=c("IMAE", "anio")) %>%
  left_join(pred_comp, by=c("IMAE", "anio")) %>%
  left_join(pred_sept, by=c("IMAE", "anio")) %>%
  left_join(pred_peso, by=c("IMAE", "anio")) %>%
  left_join(pred_URR, by=c("IMAE", "anio")) %>%
  left_join(pred_ktv, by=c("IMAE", "anio")) %>%
  left_join(tipo_imae, by=c("IMAE"="ZCAIMAE")) %>%
  mutate(tipo_imae2=case_when(tipo_imae=="INDEPENDIENTE" ~ "Indep",
                              tipo_imae=="PRIVADO" ~ "Netwk",
                              tipo_imae=="PUBLICO" ~ "Public"))

pred_URRW <- pred_URR %>% 
  pivot_wider(names_from = "anio", values_from = "URR")

URR65 <- base %>% group_by(ZCAIMAE, anio) %>% summarise(URR65=mean(URR65, na.rm=T)) %>% 
  pivot_wider(names_from = "anio", values_from = "URR65")

URR <- base %>% group_by(ZCAIMAE, anio) %>% summarise(URR=mean(URR, na.rm=T)) %>% 
  pivot_wider(names_from = "anio", values_from = "URR")

quality %>% 
  group_by(tipo_imae2) %>% 
  summarise(Urea=mean(urea, na.rm = T)*100,
            Survival=mean(surv, na.rm = T)*100,
            Phosphorus=mean(fosf, na.rm = T)*100,
            Hemoglobin=mean(hemo, na.rm = T)*100,
            Complication=mean(comp, na.rm = T)*100,
            "Septic infections"=mean(sept, na.rm = T)*100) %>% as.data.frame()

mean <- quality %>% 
  summarise(Urea = round(mean(urea, na.rm = TRUE) * 100, 2),
            Survival = round(mean(surv, na.rm = TRUE) * 100, 2),
            Phosphorus = round(mean(fosf, na.rm = TRUE) * 100, 2),
            Hemoglobin = round(mean(hemo, na.rm = TRUE) * 100, 2),
            Complication = round(mean(comp, na.rm = TRUE) * 100, 2),
            "Septic infections" = round(mean(sept, na.rm = TRUE) * 100, 2)) %>% 
  as.data.frame()

sd <- quality %>% 
  summarise(Urea = round(sd(urea, na.rm = TRUE) * 100, 2),
            Survival = round(sd(surv, na.rm = TRUE) * 100, 2),
            Phosphorus = round(sd(fosf, na.rm = TRUE) * 100, 2),
            Hemoglobin = round(sd(hemo, na.rm = TRUE) * 100, 2),
            Complication = round(sd(comp, na.rm = TRUE) * 100, 2),
            "Septic infections" = round(sd(sept, na.rm = TRUE) * 100, 2)) %>% 
  as.data.frame()

quality_summary <- rbind(mean, sd)

# Print LaTeX table
print(xtable::xtable(quality_summary, caption = "Means and Standard Deviations of Variables"), 
      caption.placement = "top", include.rownames = FALSE)


quality_reg <- quality %>%
  left_join(IMAE_num, by=c("IMAE"="ZCAIMAE")) %>% 
  rename(quality=choice) %>%
  select(anio, URR, quality) %>% 
  pivot_wider(names_from = "quality", values_from = "URR", names_prefix = "quality") %>%
  select(anio,
         quality1, quality2, quality7, quality9, quality12, 
         quality13, quality14, quality15, quality16,
         quality17, quality18, quality19, quality20, 
         quality21, quality22, quality24, quality33, quality34,
         quality35, quality40) %>% 
  mutate(anio=as.factor(anio))

write.csv(
  quality_reg,
  "C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/MastersThesis/quality.csv", 
  row.names=FALSE)

stargazer::stargazer(m_urea, m_hemo, m_fosf, m_surv, m_comp, m_sept, m_peso, type="latex",
                     keep = c("CASEDADA", "meses", "ocupado",
                                "PAC_SEXO_F",
                                "ZB1SRAZA_NEGRA", "ZB1SRAZA_OTRA",
                                "DDIAB_S", "DCISQ_S", "DEVP_S", "ECREAV",
                                "B1SNIVEL_Primaria", "B1SNIVEL_Secundaria", "B1SNIVEL_Universidad",
                              "tipo_inst_IAMCIAMPP", "tipo_inst_SEGUROPRIVADO", "tipo_inst_CORPORATIVO"), 
                     df=FALSE, omit.stat = c("ser","f", "rsq"), no.space = TRUE,
                     out="reg.tex")

stargazer::stargazer(m_urea, m_hemo, m_fosf, m_surv, m_comp, m_sept, m_peso, type="latex",
                     keep = c("CASEDADA", "meses", "ocupado"), df=FALSE, no.space = TRUE)
