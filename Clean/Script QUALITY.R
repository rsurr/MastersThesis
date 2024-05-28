my.max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)

MENSUALES_HD <- read_sav("~/Proyecto Tesis/Databases/MENSUALES HD.sav")

#SESIONES_HD <- read_sav("~/Proyecto Tesis/Databases/SESIONES HD.sav")

PACIENTES <- 
  read_sav("C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/Databases/PACIENTES.sav") %>% 
  select(CAPACNUM, PAC_SEXO, PAC_FEC_NAC)

#dife_peso <- SESIONES_HD %>% 
#  mutate(DPESOSE=case_when(DPESOSE==999.00 ~ NA,
#                           .default = as.numeric(DPESOSE)),
#         DPESOPO=case_when(DPESOPO==999.00 ~ NA,
#                           .default = as.numeric(DPESOPO)),
#         dife=DPESOPO-DPESOSE) %>% 
#  group_by(PMD_ANIO, PMD_MES, CAPACNUM) %>% 
#  summarize(dife=mean(dife, na.rm=T)) %>% 
#  mutate(peso=case_when(abs(dife)<=0.5 ~ 1,
#                        abs(dife)>0.5 ~ 0,
#                        is.na(dife) ~ NA))

MENSUALES_HD2 <- MENSUALES_HD %>% 
  right_join(INGRESOS_HD, by="CAPACNUM") %>% 
  left_join(PACIENTES, by="CAPACNUM") %>%
  #left_join(dist, by=c("CAPACNUM", "PMD_IMAE"="num_choice")) %>%
  #left_join(dife_peso, by=c("PMD_ANIO", "PMD_MES", "CAPACNUM")) %>% 
  select(CAPACNUM, ZPMD_IMAE, DDIAB, DCISQ, DEVP, PMD_ANIO, DTRABA, DFUMA, PMD_CANTBICA, 
         PMD_ANIO, PMD_MES, tipo_inst, PAC_SEXO, CASEDADA, 
         ESAZOPR1, ESAZOPO1, ESAZOPR2, EMAZOEM, ESFOSF, EMHEMOG, ZPMD_ESTADO, 
         COMP, COMP1, COMP2, COMP3, ESKTV,
         descom, coord, 
         PMD_IMAE,
         #peso,
         #dist, depto,
         PAC_FEC_NAC, fecha_solicitud, fecha_autorizacion) %>% 
  rename(ZCAIMAE=ZPMD_IMAE) %>% 
  unite("mes_mensual", c(PMD_ANIO,PMD_MES), sep="-", remove=FALSE) %>% 
  mutate(fecha_mensual = as.Date(paste(mes_mensual, "-01", sep=""))) %>% 
  filter(ZCAIMAE!="SENNIAD HEMO",
         ZCAIMAE!="IMAE A CONFIRMAR") %>% 
  mutate(
    edad = as.numeric(difftime(fecha_mensual, PAC_FEC_NAC, 
                               units = "days") / 365.25),
    ZCAIMAE=if_else(ZCAIMAE=="HOSPITAL ITALIANO", "UNIVERSAL", ZCAIMAE),
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
    ASSE=case_when(
      tipo_inst=="ASSE" ~ 1,
      .default = 0),
    IAMC_IAMPP=case_when(
      tipo_inst=="IAMC/IAMPP" ~ 1,
      .default = 0),
    CORPORATIVO=case_when(
      tipo_inst=="CORPORATIVO" ~ 1,
      .default = 0),
    PRIVADO=case_when(
      tipo_inst=="SEGURO PRIVADO" ~ 1,
      .default = 0),
    mujer=case_when(
      PAC_SEXO=="F" ~ 1,
      .default = 0),
    descom=case_when(
      descom=="S" ~ 1,
      descom=="N" ~ 0,
      descom=="D" ~ 0),
    coord=case_when(
      coord=="S" ~ 1,
      coord=="N" ~ 0,
      .default = 0),
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

# Reorder "anio" in increasing order
MENSUALES_HD2$anio <- factor(MENSUALES_HD2$PMD_ANIO, levels = sort(unique(MENSUALES_HD2$PMD_ANIO)))

# Reorder "ZCAIMAE" alphabetically
MENSUALES_HD2$ZCAIMAE <- factor(MENSUALES_HD2$ZCAIMAE, levels = sort(unique(MENSUALES_HD2$ZCAIMAE)))

# Set reference levels for anio, ZCAIMAE and CAPACNUM
MENSUALES_HD2$anio <- relevel(MENSUALES_HD2$anio, ref = "2004")
MENSUALES_HD2$ZCAIMAE <- relevel(MENSUALES_HD2$ZCAIMAE, ref = "ASOCIACION ESPAÑOLA")
MENSUALES_HD2$CAPACNUM <- relevel(as.factor(MENSUALES_HD2$CAPACNUM), ref="344677")

m_URR <- lm(URR65 ~ - 1 + 
              CASEDADA + meses + PMD_CANTBICA + descom +
              mujer + 
              DDIAB + DCISQ + DEVP + DTRABA + DFUMA +
              IAMC_IAMPP + PRIVADO + CORPORATIVO +
              ZCAIMAE:anio, 
            data=MENSUALES_HD2)

m_urea <- lm(urea17 ~ - 1 + 
               CASEDADA + meses + PMD_CANTBICA + descom +
               mujer + 
               DDIAB + DCISQ + DEVP + DTRABA + DFUMA +
               IAMC_IAMPP + PRIVADO + CORPORATIVO +
               ZCAIMAE:anio, 
             data=MENSUALES_HD2)

m_fosf <- lm(fosf55 ~ - 1 + 
               CASEDADA + meses + PMD_CANTBICA + descom +
               mujer + 
               DDIAB + DCISQ + DEVP + DTRABA + DFUMA +
               IAMC_IAMPP + PRIVADO + CORPORATIVO +
               ZCAIMAE:anio, 
             data=MENSUALES_HD2)

m_hemo <- lm(hemo10 ~ - 1 + 
               CASEDADA + meses + PMD_CANTBICA + descom +
               mujer + 
               DDIAB + DCISQ + DEVP + DTRABA + DFUMA +
               IAMC_IAMPP + PRIVADO + CORPORATIVO +
               ZCAIMAE:anio, 
             data=MENSUALES_HD2)

m_surv <- lm(surv ~ - 1 + 
               CASEDADA + meses + PMD_CANTBICA + descom +
               mujer + 
               DDIAB + DCISQ + DEVP + DTRABA + DFUMA +
               IAMC_IAMPP + PRIVADO + CORPORATIVO +
               ZCAIMAE:anio, 
             data=MENSUALES_HD2)

m_comp <- lm(comp ~ - 1 + 
               CASEDADA + meses + PMD_CANTBICA + descom +
               mujer + 
               DDIAB + DCISQ + DEVP + DTRABA + DFUMA +
               IAMC_IAMPP + PRIVADO + CORPORATIVO +
               ZCAIMAE:anio, 
             data=MENSUALES_HD2)

m_sept <- lm(sept ~ - 1 + 
               CASEDADA + meses + PMD_CANTBICA + descom +
               mujer + 
               DDIAB + DCISQ + DEVP + DTRABA + DFUMA +
               IAMC_IAMPP + PRIVADO + CORPORATIVO +
               ZCAIMAE:anio, 
             data=MENSUALES_HD2)

m_ktv <- lm(ktv12 ~ - 1 + 
              CASEDADA + meses + PMD_CANTBICA + descom +
              mujer + 
              DDIAB + DCISQ + DEVP + DTRABA + DFUMA +
              IAMC_IAMPP + PRIVADO + CORPORATIVO +
              ZCAIMAE:anio, 
            data=MENSUALES_HD2)

get_all_predictions <- function(model, data) {
  # Create a data frame with all combinations of "IMAE" and "anio"
  data
  combinations <- expand.grid(
    IMAE = levels(data$ZCAIMAE),  
    anio = levels(data$anio)      
  )
  
  # Convert "IMAE" and "anio" to factors
  combinations$IMAE <- as.factor(combinations$IMAE)
  combinations$anio <- as.factor(combinations$anio)
  
  # Create a data frame with mean values for other predictors
  mean_values <- data.frame(
    CASEDADA = mean(data$CASEDADA, na.rm = TRUE),
    meses = mean(data$meses, na.rm = TRUE),
    mujer = mean(data$mujer, na.rm = TRUE),
    DDIAB = mean(data$DDIAB, na.rm = TRUE),
    DCISQ = mean(data$DCISQ, na.rm = TRUE),
    DEVP = mean(data$DEVP, na.rm = TRUE),
    IAMC_IAMPP = mean(data$IAMC_IAMPP, na.rm = TRUE),
    PRIVADO = mean(data$PRIVADO, na.rm = TRUE),
    CORPORATIVO = mean(data$CORPORATIVO, na.rm = TRUE),
    DTRABA = mean(data$DTRABA, na.rm = TRUE),
    DFUMA = mean(data$DFUMA, na.rm = TRUE),
    PMD_CANTBICA = mean(data$PMD_CANTBICA, na.rm = TRUE),
    descom = mean(data$descom, na.rm = TRUE)
  )
  
  # Initialize an empty data frame to store predictions
  all_predictions <- data.frame()
  
  # Loop through all combinations and get predictions
  for (i in 1:nrow(combinations)) {
    mean_values$ZCAIMAE <- combinations$IMAE[i]
    mean_values$anio <- combinations$anio[i]
    predictions <- predict(model, newdata = mean_values)
    
    # Check if the coefficient for ZCAIMAE:anio is NA, and set predictions to NA accordingly
    if (is.na(coef(model)[paste0("ZCAIMAE", combinations$IMAE[i], ":anio", combinations$anio[i])])) {
      predictions <- NA
    }
    
    result <- data.frame(IMAE = combinations$IMAE[i], anio = combinations$anio[i], Prediction = predictions)
    all_predictions <- rbind(all_predictions, result)
  }
  
  return(all_predictions)
}

INGRESOS_HD2 <- read_csv("C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/MastersThesis/INGRESOS_HD2.csv")

tipo_imae <- INGRESOS_HD %>% ungroup() %>% select("ZCAIMAE", "tipo_imae") %>% unique()
IMAE_num <- read_csv("C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/MastersThesis/IMAE_num.csv") %>% filter(ZCAIMAE!="HOSPITAL ITALIANO")

pred_urea <- get_all_predictions(m_urea, MENSUALES_HD2) %>% rename(urea=Prediction)
pred_surv <- get_all_predictions(m_surv, MENSUALES_HD2) %>% rename(surv=Prediction)
pred_fosf <- get_all_predictions(m_fosf, MENSUALES_HD2) %>% rename(fosf=Prediction)
pred_hemo <- get_all_predictions(m_hemo, MENSUALES_HD2) %>% rename(hemo=Prediction)
pred_comp <- get_all_predictions(m_comp, MENSUALES_HD2) %>% rename(comp=Prediction)
pred_sept <- get_all_predictions(m_sept, MENSUALES_HD2) %>% rename(sept=Prediction)
#pred_peso <- get_all_predictions(m_peso, base) %>% rename(peso=Prediction)
pred_URR <- get_all_predictions(m_URR, MENSUALES_HD2) %>% rename(URR=Prediction)
pred_ktv <- get_all_predictions(m_ktv, MENSUALES_HD2) %>% rename(ktv=Prediction)
#pred_comp_dialisis <- get_all_predictions(m_comp_dialisis, base) %>% rename(comp_dialisis=Prediction)

quality <- left_join(pred_URR, pred_surv, by=c("IMAE", "anio")) %>% 
  #left_join(pred_fosf, by=c("IMAE", "anio")) %>%
  left_join(pred_hemo, by=c("IMAE", "anio")) %>%
  left_join(pred_comp, by=c("IMAE", "anio")) %>%
  left_join(pred_sept, by=c("IMAE", "anio")) %>%
  #left_join(pred_peso, by=c("IMAE", "anio")) %>%
  left_join(pred_urea, by=c("IMAE", "anio")) %>%
  left_join(pred_ktv, by=c("IMAE", "anio")) %>% 
  #left_join(pred_comp_dialisis, by=c("IMAE", "anio")) %>%
  left_join(tipo_imae, by=c("IMAE"="ZCAIMAE")) %>%
  mutate(tipo_imae2=case_when(tipo_choice=="INDEPENDIENTE" ~ "Indep",
                              tipo_choice=="PRIVADO" ~ "Priv Ins",
                              tipo_choice=="PUBLICO" ~ "Pub Ins")) %>% 
  left_join(IMAE_num, by=c("IMAE"="ZCAIMAE")) %>% 
  rename(ZCAIMAE=IMAE) %>% mutate(anio=as.double(as.character(anio))) %>% 
  select(-c(choice))


write.csv(
  quality,
  "C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/MastersThesis/quality.csv", 
  row.names=FALSE)

quality <- read_csv("quality.csv") %>% 
  mutate()

non_adj_quality <- MENSUALES_HD2 %>% 
  group_by(ZCAIMAE, anio) %>% 
  summarise(urea=mean(urea17, na.rm = T),
            surv=mean(surv, na.rm = T),
            fosf=mean(fosf55, na.rm = T),
            hemo=mean(hemo10, na.rm = T),
            #peso=mean(peso, na.rm = T),
            comp=mean(comp, na.rm = T),
            URR=mean(URR65, na.rm = T),
            ktv=mean(ktv12, na.rm = T),
            sept=mean(sept, na.rm = T)) %>%
  rename(IMAE=ZCAIMAE) %>% 
  left_join(tipo_imae, by=c("IMAE"="ZCAIMAE")) %>%
  mutate(tipo_imae2=case_when(tipo_imae=="INDEPENDIENTE" ~ "Indep",
                              tipo_imae=="PRIVADO" ~ "Priv Ins",
                              tipo_imae=="PUBLICO" ~ "Pub Ins")) %>% 
  left_join(IMAE_num, by=c("IMAE"="ZCAIMAE"))


adjusted <- quality %>%
  pivot_longer(cols = c("urea", "surv", "hemo", "comp", 
                        "sept", "URR", "ktv"), 
               names_to = "measure", values_to = "value") %>% 
  group_by(measure) %>% 
  summarise(Mean = round(mean(value, na.rm = TRUE), 2), 
            "Std Dev" = paste0("(", round(sd(value, na.rm = TRUE), 2), ")")) %>% 
  as.data.frame()

unadjusted <- non_adj_quality %>%
  pivot_longer(cols = c("urea", "surv", "hemo", 
                        "comp", "sept",  "URR", "ktv"), 
               names_to = "measure", values_to = "value") %>% 
  group_by(measure) %>% 
  summarise(Mean = round(mean(value, na.rm = TRUE), 2), 
            "Std Dev" = paste0("(", round(sd(value, na.rm = TRUE), 2), ")"))

labs <- c("Urea", "Survival", "Hemoglobin", 
          "No complications", "Septic infection", 
          "Urea Reduction Rate", "Kt/V")
names(labs) <- c("urea", "surv", "hemo", "comp", "sept", "URR", "ktv")

names <- cbind(names(labs), labs)%>% as.data.frame()
colnames(names) <- c("measure", "Names") 

mean_sd <- unadjusted %>% full_join(adjusted, by="measure") %>% 
  full_join(names, by="measure") %>%
  select(Names, everything(), -measure)

print(xtable::xtable(mean_sd, caption = "Means and Standard Deviations of Variables"), 
      caption.placement = "top", include.rownames = FALSE)

quality %>%
  filter(depto=="01",
         ZCAIMAE!="SENNIAD HEMO") %>% 
  pivot_longer(c(hemo, comp, URR, ktv)) %>% 
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
#ggsave("adjusted.png", dpi = 500, wid)

non_adj_quality %>%
  filter(depto=="01",
         IMAE!="SENNIAD HEMO") %>% 
  pivot_longer(c(hemo, comp, URR, ktv)) %>% 
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
#ggsave("unadjusted.png", dpi = 500, scale=3)

library(stargazer)
stargazer(m_urea, m_hemo, m_surv, m_comp, m_sept, m_ktv, m_URR,
          type="latex",
          keep = c("CASEDADA", "meses", "PMD_CANTBICA", "descom", "mujer",
                   "DDIAB", "DCISQ", "DEVP", "DTRABA", "DFUMA",
                   "IAMC_IAMPP", "PRIVADO", "CORPORATIVO"), 
          df=FALSE, omit.stat = c("ser","f", "rsq"), no.space = TRUE,
          report = "vc*",
          dep.var.labels = c("Urea", "Hemoglobin", "Survival", 
                             "Complications", "Infections", "Kt/V", "URR"),
          covariate.labels = c("Age", "Months on dialysis", "Monthly sessions", "Decompensated at start", 
                               "Female", "Diabetic",  "Cardiopathy", "Vascular perfipheral disease",
                               "Working", "Smoking",
                               "IAMC/IAMPP", "Private insurance", "Corporate insurance"),
          out="reg.tex",
          notes = "Dependent variables are dummies indicating adecuate levels of the measure.",
          notes.append = T,
          notes.align = "l")


mean_qual <- quality %>%
  filter(depto=="01",
         ZCAIMAE!="SENNIAD HEMO") %>% 
  rename(IMAE=ZCAIMAE) %>% 
  group_by(IMAE) %>% 
  summarise(URR = mean(URR, na.rm = TRUE),
            ktv = mean(ktv, na.rm = TRUE), 
            sept = mean(sept, na.rm = TRUE),
            surv = mean(surv, na.rm = TRUE),
            tipo_imae2 = first(tipo_imae2))


mean_non_adj_qual <- non_adj_quality %>%
  filter(depto=="01",
         IMAE!="SENNIAD HEMO") %>% 
  group_by(IMAE) %>% 
  summarise(URR = mean(URR, na.rm = TRUE),
            ktv = mean(ktv, na.rm = TRUE), 
            sept = mean(sept, na.rm = TRUE),
            surv = mean(surv, na.rm = TRUE),
            tipo_imae2 = first(tipo_imae2))


mean_qual %>% 
  mutate(IMAE=case_when(
    IMAE=="ASOCIACION ESPAÑOLA" ~ "ESPAÑO",
    IMAE=="CANMU" ~ "MEDICA",
    IMAE=="CASMU" ~ "CASMU",
    IMAE=="CE.DI.SA." ~ "CEDISA",
    IMAE=="HOSPITAL BRITANICO" ~ "BRITÁNI",
    IMAE=="HOSPITAL DE CLINICAS" ~ "CLÍNICAS",
    IMAE=="HOSPITAL EVANGELICO" ~ "EVANGÉL",
    IMAE=="HOSPITAL MACIEL" ~ "MACIEL",
    IMAE=="SMI - SERVICIO MEDICO INTEGRAL" ~ "SMI",
    IMAE=="CASA DE GALICIA" ~ "GALICIA",
    IMAE=="URUGUAYANA" ~ "URUGUAY",
    IMAE=="UNIVERSAL" ~ "UNIVERS",
    .default = as.character(IMAE)
  )) %>% 
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
  mutate(IMAE=case_when(
    IMAE=="ASOCIACION ESPAÑOLA" ~ "ESPAÑO",
    IMAE=="CANMU" ~ "MEDICA",
    IMAE=="CASMU" ~ "CASMU",
    IMAE=="CE.DI.SA." ~ "CEDISA",
    IMAE=="HOSPITAL BRITANICO" ~ "BRITÁNI",
    IMAE=="HOSPITAL DE CLINICAS" ~ "CLÍNICAS",
    IMAE=="HOSPITAL EVANGELICO" ~ "EVANGÉL",
    IMAE=="HOSPITAL MACIEL" ~ "MACIEL",
    IMAE=="SMI - SERVICIO MEDICO INTEGRAL" ~ "SMI",
    IMAE=="CASA DE GALICIA" ~ "GALICIA",
    IMAE=="URUGUAYANA" ~ "URUGUAY",
    IMAE=="UNIVERSAL" ~ "UNIVERS",
    .default = as.character(IMAE)
  ))  %>% 
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

quality %>% 
  mutate(anio=as.factor(anio)) %>%
  select(anio, ZCAIMAE, URR, depto) %>% 
  rename(IMAE=ZCAIMAE,
         Adjusted=URR) %>% 
  left_join(non_adj_quality, by=c("anio", "IMAE", "depto")) %>% 
  mutate(IMAE=if_else(IMAE=="CEDINA", "CE.DI.SA", IMAE),
         depto=if_else(IMAE=="CE.DI.SA", "01", depto),
         tipo_imae2=if_else(IMAE=="CE.DI.SA", "Indep", tipo_imae2)) %>% 
  filter(depto=="01",
         IMAE!="SENNIAD HEMO",
         !is.na(tipo_imae2)) %>% 
  rename(Unadjusted=URR) %>% 
  select(anio, IMAE, Adjusted, Unadjusted, tipo_imae2) %>% 
  pivot_longer(cols = c("Unadjusted", "Adjusted"), names_to = "Type", values_to = "Value") %>%
  ggplot(aes(y=Value, x=tipo_imae2, color=tipo_imae2)) +
  geom_boxplot() +
  facet_wrap(~Type, scale="free_y", labeller = labeller(name=labs)) +
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
