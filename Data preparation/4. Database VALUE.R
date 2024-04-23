library(tidyverse)
library(haven)
library(readr)
library(fastDummies)
library(broom)


PACIENTES <- 
  read_sav("C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/Databases/PACIENTES.sav")
MENSUALES_HD <- read_sav("~/Proyecto Tesis/Databases/MENSUALES HD.sav") %>% 
  select(CAPACNUM, PMD_IMAE, ZPMD_IMAE, PMD_ANIO, PMD_MES, 
         DDIAB, DCISQ, DEVP, EMAZOEM, ESFOSF, EMHEMOG, 
         COMP, COMP1, COMP2, COMP3, ESAZOPR1, ESAZOPR2, ESAZOPO1, ESKTV,
         ZPMD_ESTADO, DLISTATR
         ) %>% 
  mutate(
    ZPMD_IMAE=if_else(ZPMD_IMAE=="HOSPITAL ITALIANO", "UNIVERSAL", ZPMD_IMAE))
INGRESOS_HD2 <- read_csv("INGRESOS_HD2.csv")
IMAE_num <- read_csv("IMAE_num.csv") %>% filter(ZCAIMAE!="HOSPITAL ITALIANO")
SESIONES_HD <- 
  read_sav("C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/Databases/SESIONES HD.sav") 

dife_peso <- SESIONES_HD %>% 
  mutate(DPESOSE=case_when(DPESOSE==999.00 ~ NA,
                           .default = as.numeric(DPESOSE)),
         DPESOPO=case_when(DPESOPO==999.00 ~ NA,
                           .default = as.numeric(DPESOPO)),
         dife=DPESOPO-DPESOSE) %>% 
  group_by(PMD_ANIO, PMD_MES, CAPACNUM) %>% 
  summarize(dife=mean(dife, na.rm=T)) %>% 
  mutate(peso=case_when(abs(dife)<=0.5 ~ 1,
                        abs(dife)>0.5 ~ 0,
                        is.na(dife) ~ NA))

cod_sept <- c(184010162, 184010183, 184010159, 184010160, 184010161)

base <- left_join(MENSUALES_HD, PACIENTES, by="CAPACNUM") %>% 
  left_join(INGRESOS_HD, by="CAPACNUM") %>% 
  rename(imae_ingresos=ZCAIMAE,
         ZCAIMAE=ZPMD_IMAE) %>%
  #mutate(
  #  ZCAIMAE=if_else(ZCAIMAE=="HOSPITAL ITALIANO", "UNIVERSAL", ZCAIMAE)) %>% 
  left_join(IMAE_num, by="ZCAIMAE") %>% 
  left_join(dife_peso, by=c("PMD_ANIO", "PMD_MES", "CAPACNUM")) %>% 
  filter(depto=="01",
         ZCAIMAE!="SENNIAD HEMO") %>%
  mutate_if(is.character, na_if,"") %>% 
  mutate(
    DDIAB=if_else(DDIAB=="S", 1, 0),
    DCISQ=if_else(DCISQ=="S", 1, 0),
    DEVP=if_else(DEVP=="S", 1, 0),
  ) %>% 
  group_by(CAPACNUM, PMD_ANIO) %>% 
  mutate(
    DDIAB=max(DDIAB, na.rm = T),
    DCISQ=max(DCISQ, na.rm = T),
    DEVP=max(DEVP, na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(#DDIAB=case_when(DDIAB=="D" ~ NA,
         #                .default = as.character(DDIAB)),
         #DCISQ=case_when(DCISQ=="D" ~ NA,
         #                .default = as.character(DCISQ)),
         #DEVP=case_when(DEVP=="D" ~ NA,
         #               .default = as.character(DEVP)),
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
                            .default = 1), 
         CAPACNUM=as.factor(CAPACNUM)) %>% 
  mutate(urea17=case_when(EMAZOEM<1.7 ~ 1,
                          EMAZOEM>=1.7 ~ 0),
         BMI=exa_peso/(exa_altura/100)**2,
         fosf55=case_when(ESFOSF<5.5 ~ 1,
                          ESFOSF>=5.5 ~ 0),
         #pas140=case_when(EMAZOEM<1.7 ~ 1,
         #                 EMAZOEM>=1.7 ~ 0),
         hemo10=case_when(EMHEMOG>=10 ~ 1,
                          EMHEMOG<10 ~ 0),
         surv=case_when(ZPMD_ESTADO!="FALLECIMIENTO" ~ 1,
                         ZPMD_ESTADO=="FALLECIMIENTO" ~ 0),
         comp=case_when(COMP=="S" ~ 0,
                        COMP=="N" ~ 1,
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
                                "B1SNIVEL",
                                "tipo_inst"),
             ignore_na = T) %>%
  rename(tipo_inst_IAMCIAMPP=`tipo_inst_IAMC/IAMPP`,
         tipo_inst_SEGUROPRIVADO=`tipo_inst_SEGURO PRIVADO`) %>%
  ungroup() %>% 
  group_by(CAPACNUM) %>% 
  arrange(PMD_ANIO, PMD_MES) %>% 
  mutate(meses=row_number())

#sesiones_base <- SESIONES_HD %>% mutate(CAPACNUM=as.factor(CAPACNUM)) %>%
#  left_join(base, by=c("CAPACNUM", "PMD_ANIO", "PMD_MES", "ZPMD_IMAE"="ZCAIMAE"))
#
#
#sesiones_base <- sesiones_base %>% mutate(comp_dialisis=case_when(DCOMP=="S" ~ 0,
#                                                                  DCOMP=="N" ~ 1, 
#                                                                  .default = NA)) %>% 
#  rename(ZCAIMAE=ZPMD_IMAE) %>% 
#  filter(depto=="01",
#         ZCAIMAE!="SENNIAD HEMO") 

# Reorder "anio" in increasing order
base$anio <- factor(base$PMD_ANIO, levels = sort(unique(base$PMD_ANIO)))

# Reorder "ZCAIMAE" alphabetically
base$ZCAIMAE <- factor(base$ZCAIMAE, levels = sort(unique(base$ZCAIMAE)))

# Set reference levels for anio, ZCAIMAE and CAPACNUM
base$anio <- relevel(base$anio, ref = "2004")
base$ZCAIMAE <- relevel(base$ZCAIMAE, ref = "ASOCIACION ESPAÑOLA")
base$CAPACNUM <- relevel(base$CAPACNUM, ref="344677")

# Fit the linear regression model
m_urea <- lm(urea17 ~ - 1 + 
               CASEDADA + meses + ocupado +
               PAC_SEXO_F + 
               DDIAB + DCISQ + DEVP + estu_creatinemia + 
               B1SNIVEL_Primaria + B1SNIVEL_Secundaria + B1SNIVEL_Universidad +
               tipo_inst_IAMCIAMPP + tipo_inst_SEGUROPRIVADO + tipo_inst_CORPORATIVO +
               ZCAIMAE:anio, 
             data=base)

m_hemo <- lm(hemo10 ~ - 1 + 
               CASEDADA + meses + ocupado +
               PAC_SEXO_F + 
               DDIAB + DCISQ + DEVP + estu_creatinemia + 
               B1SNIVEL_Primaria + B1SNIVEL_Secundaria + B1SNIVEL_Universidad +
               tipo_inst_IAMCIAMPP + tipo_inst_SEGUROPRIVADO + tipo_inst_CORPORATIVO +
               ZCAIMAE:anio, 
             data=base)

m_fosf <- lm(fosf55 ~ - 1 + 
               CASEDADA + meses + ocupado +
               PAC_SEXO_F + 
               DDIAB + DCISQ + DEVP + estu_creatinemia + 
               B1SNIVEL_Primaria + B1SNIVEL_Secundaria + B1SNIVEL_Universidad +
               tipo_inst_IAMCIAMPP + tipo_inst_SEGUROPRIVADO + tipo_inst_CORPORATIVO +
               ZCAIMAE:anio, 
             data=base)

m_surv <- lm(surv ~ - 1 + 
                CASEDADA + meses + ocupado +
                PAC_SEXO_F + 
                DDIAB + DCISQ + DEVP + estu_creatinemia + 
                B1SNIVEL_Primaria + B1SNIVEL_Secundaria + B1SNIVEL_Universidad +
                tipo_inst_IAMCIAMPP + tipo_inst_SEGUROPRIVADO + tipo_inst_CORPORATIVO +
                ZCAIMAE:anio, 
              data=base)

m_comp <- lm(comp ~ - 1 + 
               CASEDADA + meses + ocupado +
               PAC_SEXO_F + 
               DDIAB + DCISQ + DEVP + estu_creatinemia + 
               B1SNIVEL_Primaria + B1SNIVEL_Secundaria + B1SNIVEL_Universidad +
               tipo_inst_IAMCIAMPP + tipo_inst_SEGUROPRIVADO + tipo_inst_CORPORATIVO +
               ZCAIMAE:anio, 
             data=base)

m_sept <- lm(sept ~ - 1 + 
               CASEDADA + meses + ocupado +
               PAC_SEXO_F + 
               DDIAB + DCISQ + DEVP + estu_creatinemia + 
               B1SNIVEL_Primaria + B1SNIVEL_Secundaria + B1SNIVEL_Universidad +
               tipo_inst_IAMCIAMPP + tipo_inst_SEGUROPRIVADO + tipo_inst_CORPORATIVO +
               ZCAIMAE:anio, 
             data=base)

m_peso <- lm(peso ~ - 1 + 
               CASEDADA + meses + ocupado +
               PAC_SEXO_F + 
               DDIAB + DCISQ + DEVP + estu_creatinemia + 
               B1SNIVEL_Primaria + B1SNIVEL_Secundaria + B1SNIVEL_Universidad +
               tipo_inst_IAMCIAMPP + tipo_inst_SEGUROPRIVADO + tipo_inst_CORPORATIVO +
               ZCAIMAE:anio, 
             data=base)

m_URR <- lm(URR65 ~ - 1 + 
               CASEDADA + meses + ocupado +
               PAC_SEXO_F + 
               DDIAB + DCISQ + DEVP + estu_creatinemia + 
               B1SNIVEL_Primaria + B1SNIVEL_Secundaria + B1SNIVEL_Universidad +
               tipo_inst_IAMCIAMPP + tipo_inst_SEGUROPRIVADO + tipo_inst_CORPORATIVO +
               ZCAIMAE:anio, 
             data=base)

m_ktv <- lm(ktv12 ~ - 1 + 
              CASEDADA + meses + ocupado +
              PAC_SEXO_F + 
              DDIAB + DCISQ + DEVP + estu_creatinemia + 
              B1SNIVEL_Primaria + B1SNIVEL_Secundaria + B1SNIVEL_Universidad +
              tipo_inst_IAMCIAMPP + tipo_inst_SEGUROPRIVADO + tipo_inst_CORPORATIVO +
              ZCAIMAE:anio, 
            data=base)

#m_comp_dialisis <- lm(comp_dialisis ~ - 1 + 
#                        CASEDADA + meses + ocupado +
#                        PAC_SEXO_F + 
#                        DDIAB + DCISQ + DEVP + estu_creatinemia + 
#                        B1SNIVEL_Primaria + B1SNIVEL_Secundaria + B1SNIVEL_Universidad +
#                        tipo_inst_IAMCIAMPP + tipo_inst_SEGUROPRIVADO + tipo_inst_CORPORATIVO +
#                        ZCAIMAE:anio, 
#                      data=sesiones_base)

#coef_ktv <- tidy(m_ktv) %>%
#  select(term, estimate) %>% 
#  filter(str_detect(term, "ZCAIMAE") & str_detect(term, ":anio")) %>%
#  separate(term, into = c("ZCAIMAE", "anio"), sep = ":") %>%
#  mutate(anio = gsub("anio", "", anio),
#         IMAE = gsub("ZCAIMAE", "", ZCAIMAE)) %>% 
#  rename(ktv=estimate) %>% 
#  select(anio, ktv, IMAE) %>% 
#  pivot_wider(names_from = "anio", values_from = "ktv")

#left_join(tipo_imae, by=c("ZCAIMAE")) %>%
#  mutate(tipo_imae2=case_when(tipo_imae=="INDEPENDIENTE" ~ "Indep",
#                              tipo_imae=="PRIVADO" ~ "Priv Ins",
#                              tipo_imae=="PUBLICO" ~ "Pub Ins"))

tipo_imae <- INGRESOS_HD2 %>% ungroup() %>% select("ZCAIMAE", "tipo_choice") %>% unique()

source("C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/MastersThesis/Data preparation/Functions.R")
pred_urea <- get_all_predictions(m_urea, base) %>% rename(urea=Prediction)
pred_surv <- get_all_predictions(m_surv, base) %>% rename(surv=Prediction)
pred_fosf <- get_all_predictions(m_fosf, base) %>% rename(fosf=Prediction)
pred_hemo <- get_all_predictions(m_hemo, base) %>% rename(hemo=Prediction)
pred_comp <- get_all_predictions(m_comp, base) %>% rename(comp=Prediction)
pred_sept <- get_all_predictions(m_sept, base) %>% rename(sept=Prediction)
pred_peso <- get_all_predictions(m_peso, base) %>% rename(peso=Prediction)
pred_URR <- get_all_predictions(m_URR, base) %>% rename(URR=Prediction)
pred_ktv <- get_all_predictions(m_ktv, base) %>% rename(ktv=Prediction)
#pred_comp_dialisis <- get_all_predictions(m_comp_dialisis, base) %>% rename(comp_dialisis=Prediction)

quality <- left_join(pred_URR, pred_surv, by=c("IMAE", "anio")) %>% 
  left_join(pred_fosf, by=c("IMAE", "anio")) %>%
  left_join(pred_hemo, by=c("IMAE", "anio")) %>%
  left_join(pred_comp, by=c("IMAE", "anio")) %>%
  left_join(pred_sept, by=c("IMAE", "anio")) %>%
  left_join(pred_peso, by=c("IMAE", "anio")) %>%
  left_join(pred_urea, by=c("IMAE", "anio")) %>%
  left_join(pred_ktv, by=c("IMAE", "anio")) %>% 
  #left_join(pred_comp_dialisis, by=c("IMAE", "anio")) %>%
  left_join(tipo_imae, by=c("IMAE"="ZCAIMAE")) %>%
  mutate(tipo_imae2=case_when(tipo_choice=="INDEPENDIENTE" ~ "Indep",
                              tipo_choice=="PRIVADO" ~ "Priv Ins",
                              tipo_choice=="PUBLICO" ~ "Pub Ins")) %>% 
  left_join(IMAE_num, by=c("IMAE"="ZCAIMAE")) 
 # %>% rename(ZCAIMAE=IMAE) %>% mutate(anio=as.double(as.character(anio)))  
  

write.csv(
  quality,
  "C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/MastersThesis/quality.csv", 
  row.names=FALSE)

#quality_input_outcome <- quality %>% 
#  group_by(IMAE, anio) %>% 
#  mutate(input=mean(c(URR, ktv, peso), na.rm = T),
#         output_int=mean(c(urea, hemo, fosf), na.rm = T),
#         output_fin=mean(c(surv, comp, sept), na.rm = T))
#
#pred_URRW <- pred_URR %>% 
#  pivot_wider(names_from = "anio", values_from = "URR")
#
#URR65 <- base %>% group_by(ZCAIMAE, anio) %>% summarise(URR65=mean(URR65, na.rm=T)) %>% 
#  pivot_wider(names_from = "anio", values_from = "URR65")
#
#URR <- base %>% group_by(ZCAIMAE, anio) %>% summarise(URR=mean(URR, na.rm=T)) %>% 
#  pivot_wider(names_from = "anio", values_from = "URR")
#
#quality %>% 
#  group_by(tipo_imae2) %>% 
#  summarise(
#    Urea=mean(urea, na.rm = T)*100,
#    Survival=mean(surv, na.rm = T)*100,
#    Phosphorus=mean(fosf, na.rm = T)*100,
#    Hemoglobin=mean(hemo, na.rm = T)*100,
#    Complication=mean(comp, na.rm = T)*100,
#    "Septic infections"=mean(sept, na.rm = T)*100
#  ) %>% as.data.frame()

non_adj_quality <- base %>% 
  group_by(ZCAIMAE, anio) %>% 
  summarise(urea=mean(urea17, na.rm = T),
            surv=mean(surv, na.rm = T),
            fosf=mean(fosf55, na.rm = T),
            hemo=mean(hemo10, na.rm = T),
            peso=mean(peso, na.rm = T),
            comp=mean(comp, na.rm = T),
            URR=mean(URR65, na.rm = T),
            ktv=mean(ktv12, na.rm = T),
            sept=mean(sept, na.rm = T)) %>%
  rename(IMAE=ZCAIMAE) %>% 
  left_join(tipo_imae, by=c("IMAE"="ZCAIMAE")) %>%
  mutate(tipo_imae2=case_when(tipo_choice=="INDEPENDIENTE" ~ "Indep",
                                tipo_choice=="PRIVADO" ~ "Priv Ins",
                                tipo_choice=="PUBLICO" ~ "Pub Ins"))


#non_adj_quality_sesiones_base <- sesiones_base %>% 
#  group_by(ZCAIMAE, anio) %>% 
#  summarise(comp_dialisis=mean(comp_dialisis, na.rm = T)) %>%
#  rename(IMAE=ZCAIMAE) %>% 
#  left_join(tipo_imae, by=c("IMAE"="ZCAIMAE")) %>%
#  mutate(tipo_imae2=case_when(tipo_choice=="INDEPENDIENTE" ~ "Indep",
#                              tipo_choice=="PRIVADO" ~ "Priv Ins",
#                              tipo_choice=="PUBLICO" ~ "Pub Ins"))
#
#non_adj_quality <- non_adj_quality_base %>% left_join(non_adj_quality_sesiones_base, by=c("IMAE", "anio", "tipo_imae2"))

adjusted <- quality %>%
  pivot_longer(cols = c("urea", "surv", "fosf", "hemo", "comp", 
                        "sept", "peso", "URR", "ktv"), 
               names_to = "measure", values_to = "value") %>% 
  group_by(measure) %>% 
  summarise(Mean = round(mean(value, na.rm = TRUE), 2), 
            "Std Dev" = paste0("(", round(sd(value, na.rm = TRUE), 2), ")")) %>% 
  as.data.frame()

unadjusted <- non_adj_quality %>%
  pivot_longer(cols = c("urea", "surv", "fosf", "hemo", 
                        "comp", "sept", "peso", "URR", "ktv"), 
               names_to = "measure", values_to = "value") %>% 
  group_by(measure) %>% 
  summarise(Mean = round(mean(value, na.rm = TRUE), 2), 
            "Std Dev" = paste0("(", round(sd(value, na.rm = TRUE), 2), ")"))

mean_sd <- unadjusted %>% full_join(adjusted, by="measure") %>% 
  full_join(names, by="measure") %>%
  select(Names, everything(), -measure)


#
#quality_summary <- rbind(mean, sd)

# Print LaTeX table
print(xtable::xtable(mean_sd, caption = "Means and Standard Deviations of Variables"), 
      caption.placement = "top", include.rownames = FALSE)
#
#quality_input <- quality %>%
#  ungroup() %>% 
#  left_join(IMAE_num, by=c("IMAE"="ZCAIMAE")) %>% 
#  rename(quality=choice) %>%
#  select(anio, URR, quality) %>% 
#  pivot_wider(names_from = "quality", values_from = "URR", names_prefix = "quality_input") %>%
#  select(anio,
#         quality_input1, quality_input2, quality_input9, quality_input12, 
#         quality_input13, quality_input14, quality_input16,
#         quality_input17, quality_input18, quality_input19, quality_input20, 
#         quality_input21, quality_input22, quality_input24, quality_input33, quality_input34,
#         quality_input35, quality_input40) %>% 
#  mutate(anio=as.factor(anio))

#quality_output_fin <- quality_input_outcome %>%
#  ungroup() %>% 
#  left_join(IMAE_num, by=c("IMAE"="ZCAIMAE")) %>% 
#  rename(quality=choice) %>%
#  select(anio, output_int, quality) %>% 
#  pivot_wider(names_from = "quality", values_from = "output_int", names_prefix = "quality_output_int") %>%
#  select(anio,
#         quality_output_int1, quality_output_int2, quality_output_int9, quality_output_int12, 
#         quality_output_int13, quality_output_int14, quality_output_int15, quality_output_int16,
#         quality_output_int17, quality_output_int18, quality_output_int19, quality_output_int20, 
#         quality_output_int21, quality_output_int22, quality_output_int24, quality_output_int33, quality_output_int34,
#         quality_output_int35, quality_output_int40)


#source("C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/MastersThesis/Data preparation/Functions.R")
#out_urea <- create_quality_output(quality, "urea")
#out_surv <- create_quality_output(quality, "surv")
#out_fosf <- create_quality_output(quality, "fosf")
#out_hemo <- create_quality_output(quality, "hemo")
#out_comp <- create_quality_output(quality, "comp")
#out_sept <- create_quality_output(quality, "sept")
#out_peso <- create_quality_output(quality, "peso")
#out_URR <- create_quality_output(quality, "URR")
#out_ktv <- create_quality_output(quality, "ktv")
##out_comp_dialisis <- create_quality_output(quality, "comp_dialisis")



#quality_output <- quality %>%
#  ungroup() %>% 
#  left_join(IMAE_num, by=c("IMAE"="ZCAIMAE")) %>% 
#  rename(quality=choice) %>%
#  select(anio, surv, quality) %>% 
#  pivot_wider(names_from = "quality", values_from = "surv", names_prefix = "quality_output") %>%
#  select(anio,
#         quality_output1, quality_output2, quality_output9, quality_output12, 
#         quality_output13, quality_output14, quality_output16,
#         quality_output17, quality_output18, quality_output19, quality_output20, 
#         quality_output21, quality_output22, quality_output24, quality_output33, quality_output34,
#         quality_output35, quality_output40)

#quality_reg <- left_join(out_urea, out_surv, by="anio") %>% 
#  left_join(out_fosf, by="anio") %>% 
#  left_join(out_hemo, by="anio") %>% 
#  left_join(out_comp, by="anio") %>% 
#  left_join(out_sept, by="anio") %>% 
#  left_join(out_peso, by="anio") %>% 
#  left_join(out_URR, by="anio") %>% 
#  left_join(out_ktv, by="anio") %>% 
#  #left_join(out_comp_dialisis, by="anio") %>% 
#  mutate(anio=as.numeric(as.character(anio)))

#write.csv(
#  quality_reg,
#  "C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/MastersThesis/quality.csv", 
#  row.names=FALSE)
#
# Tabla ----

library(stargazer)
stargazer(m_urea, m_hemo, m_fosf, m_surv, m_comp, 
                     m_sept, m_peso, m_ktv, m_URR,
                     type="latex",
                     keep = c("CASEDADA", "meses", "ocupado",
                                "PAC_SEXO_F",
                                "ZB1SRAZA_NEGRA", "ZB1SRAZA_OTRA",
                                "DDIAB", "DCISQ", "DEVP", "estu_creatinemia",
                                "B1SNIVEL_Primaria", "B1SNIVEL_Secundaria", "B1SNIVEL_Universidad",
                              "tipo_inst_IAMCIAMPP", "tipo_inst_SEGUROPRIVADO", "tipo_inst_CORPORATIVO"), 
                     df=FALSE, omit.stat = c("ser","f", "rsq"), no.space = TRUE,
          report = "vc*",
          dep.var.labels = c("Urea", "Hemoglobin", "Phosphorus", "Survival", 
                             "Complications", "Infections", "Weight", "Kt/V", "URR"),
          covariate.labels = c("Age", "Months on dialysis", "Working", "Female", "Diabetic",  
                               "Cardiopathy", "Vascular perfipheral", "Creatinine", "Primary", 
                               "Secondary", "University", "IAMC/IAMPP", "Seguro privado", "Corporativo"),
                     out="reg.tex",
          notes = "Dependent variables are dummies indicating adecuate levels of the measure.",
          notes.append = T,
          notes.align = "l")

stargazer::stargazer(m_urea, m_hemo, m_fosf, m_surv, m_comp, m_sept, m_peso, type="latex",
                     keep = c("CASEDADA", "meses", "ocupado"), df=FALSE, no.space = TRUE)

base %>% group_by(CAPACNUM) %>%
  summarise(Age=mean(CASEDADA, na.rm=T),
            Diabetic=mean(DDIAB, na.rm=T)) %>%
  summarise(Age=mean(Age, na.rm=T),
            Diabetic=paste0(round(mean(Diabetic, na.rm=T), digits = 2)*100, "%"))

#  Boxplots ----

labs <- c("Urea", "Survival", "Phosphorus", "Hemoglobin", 
          "No complications", "Septic infection", "Weight", 
          "Urea Reduction Rate", "Kt/V")
names(labs) <- c("urea", "surv", "fosf", "hemo", "comp", "sept", "peso", "URR", "ktv")

labs_io <- c("Input", "Outcome: intermediate", "Outcome: final")
names(labs_io) <- c("input", "output_int", "output_fin")

names <- cbind(names(labs), labs)%>% as.data.frame()
colnames(names) <- c("measure", "Names") 



quality %>%
  pivot_longer(c(urea, surv, fosf, hemo, comp, sept, peso, URR, ktv)) %>% 
  ggplot(aes(y=value, x=tipo_imae2, color=tipo_imae2)) +
  geom_boxplot() +
  facet_wrap(~name, scale="free_y", labeller = labeller(name=labs)) +
  coord_cartesian(ylim = c(0, 1)) +  # Restrict y-axis from 0 to 1
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),  # Remove x-axis label
    axis.title.y = element_blank(),  # Remove x-axis label
    panel.background = element_rect(fill = "white"),  # Change background to white
    panel.grid.major = element_line(color = "gray"),  # Change major gridlines to gray
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    strip.background = element_blank()  # Remove background from facet titles
  ) +
  scale_color_viridis_d(end=0.8)
ggsave("adjusted.png", dpi = 500, wid)

non_adj_quality %>%
  pivot_longer(c(urea, surv, fosf, hemo, comp, sept, peso, URR, ktv)) %>% 
  ggplot(aes(y=value, x=tipo_imae2, color=tipo_imae2)) +
  geom_boxplot() +
  facet_wrap(~name, scale="free_y", labeller = labeller(name=labs)) +
  coord_cartesian(ylim = c(0, 1)) +  # Restrict y-axis from 0 to 1
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),  # Remove x-axis label
    axis.title.y = element_blank(),  # Remove x-axis label
    panel.background = element_rect(fill = "white"),  # Change background to white
    panel.grid.major = element_line(color = "gray"),  # Change major gridlines to gray
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    strip.background = element_blank()  # Remove background from facet titles
  ) +
  scale_color_viridis_d(end=0.8)
ggsave("unadjusted.png", dpi = 500, scale=3)


# Ranking ----


mean_qual <- quality %>%
  group_by(IMAE) %>% 
  summarise(URR = mean(URR, na.rm = TRUE),
            ktv = mean(ktv, na.rm = TRUE), 
            sept = mean(sept, na.rm = TRUE),
            surv = mean(surv, na.rm = TRUE),
            tipo_imae2 = first(tipo_imae2))


mean_non_adj_qual <- non_adj_quality %>%
  group_by(IMAE) %>% 
  summarise(URR = mean(URR, na.rm = TRUE),
            ktv = mean(ktv, na.rm = TRUE), 
            sept = mean(sept, na.rm = TRUE),
            surv = mean(surv, na.rm = TRUE),
            tipo_imae2 = first(tipo_imae2))

mean_qual %>% 
  ggplot(aes(y = URR, x = reorder(IMAE, -URR), fill = tipo_imae2)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_fill_viridis_d(end = 0.8, name = "Provider type") +  # Adding legend title
  theme(
    legend.position = "bottom",
    axis.title.x = element_blank(),  # Remove x-axis label
    axis.title.y = element_blank(),  # Remove x-axis label
    panel.background = element_rect(fill = "white"),  # Change background to white
    panel.grid.major = element_line(color = "gray"),  # Change major gridlines to gray
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    strip.background = element_blank()  # Remove background from facet titles
  ) +
  scale_x_discrete(labels = function(x) substr(x, nchar(x)-7, nchar(x))) +
  coord_cartesian(ylim = c(0, 1))
ggsave("mean_adjusted_URR.png", dpi = 500, scale=3)


mean_non_adj_qual %>% 
  ggplot(aes(y = URR, x = reorder(IMAE, -URR), fill = tipo_imae2)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_fill_viridis_d(end = 0.8, name = "Provider type") +  # Adding legend title
  theme(
    legend.position = "bottom",
    axis.title.x = element_blank(),  # Remove x-axis label
    axis.title.y = element_blank(),  # Remove x-axis label
    panel.background = element_rect(fill = "white"),  # Change background to white
    panel.grid.major = element_line(color = "gray"),  # Change major gridlines to gray
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    strip.background = element_blank()  # Remove background from facet titles
  ) +
  scale_x_discrete(labels = function(x) substr(x, nchar(x)-7, nchar(x))) +
  coord_cartesian(ylim = c(0, 1))
ggsave("mean_unadjusted_URR.png", dpi = 500, scale=3)

mean_qual %>% 
  ggplot(aes(y = surv, x = reorder(IMAE, -surv), fill = tipo_imae2)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_fill_viridis_d(end = 0.8, name = "Provider type") +  # Adding legend title
  theme(
    legend.position = "bottom",
    axis.title.x = element_blank(),  # Remove x-axis label
    axis.title.y = element_blank(),  # Remove x-axis label
    panel.background = element_rect(fill = "white"),  # Change background to white
    panel.grid.major = element_line(color = "gray"),  # Change major gridlines to gray
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    strip.background = element_blank()  # Remove background from facet titles
  ) +
  scale_x_discrete(labels = function(x) substr(x, nchar(x)-7, nchar(x))) +
  coord_cartesian(ylim = c(0, 1))
ggsave("mean_adjusted_surv.png", dpi = 500, scale=3)


mean_non_adj_qual %>% 
  ggplot(aes(y = surv, x = reorder(IMAE, -surv), fill = tipo_imae2)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_fill_viridis_d(end = 0.8, name = "Provider type") +  # Adding legend title
  theme(
    legend.position = "bottom",
    axis.title.x = element_blank(),  # Remove x-axis label
    axis.title.y = element_blank(),  # Remove x-axis label
    panel.background = element_rect(fill = "white"),  # Change background to white
    panel.grid.major = element_line(color = "gray"),  # Change major gridlines to gray
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    strip.background = element_blank()  # Remove background from facet titles
  ) +
  scale_x_discrete(labels = function(x) substr(x, nchar(x)-7, nchar(x))) +
  coord_cartesian(ylim = c(0, 1))
ggsave("mean_unadjusted_surv.png", dpi = 500, scale=3)


# Correlation ----


cor_quality_adj <- quality %>% 
  select(-c(IMAE, anio, tipo_choice, tipo_imae2, depto, ZCAIMAE)) %>% 
  cor(use="complete.obs")

cor_quality_non <- non_adj_quality %>%  ungroup() %>% 
  select(-c(IMAE, anio, tipo_imae2, tipo_choice.x, tipo_choice.y)) %>% 
  cor(use="complete.obs")


install.packages("corrplot")
library(corrplot)

# Subset numeric variables excluding 'anio'
numeric_data <- quality[, sapply(quality, is.numeric) & names(quality) != "anio"]

# Compute correlation matrix
correlation_matrix <- cor(numeric_data, use = "pairwise.complete.obs")

library(ggcorrplot)


# Plot correlation matrix
ggcorrplot(correlation_matrix, type = "upper", outline.col = "white",
           colors = c("#E46726", "white","#6D9EC1" ),
           lab = TRUE)

# Subset numeric variables excluding 'anio'
numeric_data <- non_adj_quality[, sapply(non_adj_quality, is.numeric) & names(quality) != "anio"]

# Compute correlation matrix
correlation_matrix <- cor(numeric_data, use = "pairwise.complete.obs")

library(ggcorrplot)


# Plot correlation matrix
ggcorrplot(correlation_matrix, type = "upper", outline.col = "white",
           colors = c("#E46726", "white","#6D9EC1" ),
           lab = TRUE)




