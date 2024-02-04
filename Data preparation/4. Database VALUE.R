library(tidyverse)
library(haven)
library(readr)

PACIENTES <- read_sav("C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/Databases/PACIENTES.sav")
MENSUALES_HD <- read_sav("C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/Databases/MENSUALES HD.sav")
INGRESOS_HD2 <- read_csv("INGRESOS_HD2.csv")

base <- left_join(MENSUALES_HD, PACIENTES, by="CAPACNUM") %>% 
  left_join(INGRESOS_HD2, by="CAPACNUM") %>%
  left_join(IMAE_num, by="ZCAIMAE") %>% 
  filter(depto=="01") %>%
  mutate_if(is.character, na_if,"") %>% 
  group_by(CAPACNUM) %>% 
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
                            .default = as.character(PAC_SEXO))) %>% 
  mutate(urea17=case_when(EMAZOEM<1.7 ~ 1,
                          EMAZOEM>=1.7 ~ 0),
         BMI=SCEFPE/(SCEFTA/100)**2,
         fosf55=case_when(ESFOSF<5.5 ~ 1,
                          ESFOSF>=5.5 ~ 0),
         #pas140=case_when(EMAZOEM<1.7 ~ 1,
         #                 EMAZOEM>=1.7 ~ 0),
         hemo10=case_when(EMHEMOG>=10 ~ 1,
                          EMHEMOG<10 ~ 0),
         morta=case_when(ZPMD_ESTADO!="FALLECIMIENTO" ~ 1,
                         ZPMD_ESTADO=="FALLECIMIENTO" ~ 0)) %>% 
  dummy_cols(select_columns = c("PAC_SEXO", "ZB1SRAZA", "DDIAB", "DCISQ", "DEVP", "B1SNIVEL"),
             ignore_na = T)

table(base$ZB1SRAZA)

# Reorder "anio" in increasing order
base$anio <- factor(base$PMD_ANIO, levels = sort(unique(base$PMD_ANIO)))

# Reorder "ZCAIMAE" alphabetically
base$ZCAIMAE <- factor(base$ZCAIMAE, levels = sort(unique(base$ZCAIMAE)))

# Set reference levels for anio and ZCAIMAE
base$anio <- relevel(base$anio, ref = "2004")
base$ZCAIMAE <- relevel(base$ZCAIMAE, ref = "ASOCIACION ESPAÑOLA")

summary(base$B1SNIVEL_)

# Fit the linear regression model
m_urea <- lm(urea17 ~ - 1 + 
               CASEDADA + 
               PAC_SEXO_F + 
               ZB1SRAZA_NEGRA + ZB1SRAZA_OTRA +
               DDIAB_S + DCISQ_S + DEVP_S + 
               B1SNIVEL_Primaria + B1SNIVEL_Secundaria + B1SNIVEL_Universidad +
               ZCAIMAE:anio, 
             data=base)

m_hemo <- lm(hemo10 ~ - 1 + 
               CASEDADA + 
               PAC_SEXO_F + 
               ZB1SRAZA_NEGRA + ZB1SRAZA_OTRA +
               DDIAB_S + DCISQ_S + DEVP_S + 
               B1SNIVEL_Primaria + B1SNIVEL_Secundaria + B1SNIVEL_Universidad +
               ZCAIMAE:anio, 
             data=base)

m_fosf <- lm(fosf55 ~ - 1 + 
               CASEDADA + 
               PAC_SEXO_F + 
               ZB1SRAZA_NEGRA + ZB1SRAZA_OTRA +
               DDIAB_S + DCISQ_S + DEVP_S + 
               B1SNIVEL_Primaria + B1SNIVEL_Secundaria + B1SNIVEL_Universidad +
               ZCAIMAE:anio, 
             data=base)

m_morta <- lm(morta ~ - 1 + 
               CASEDADA + 
               PAC_SEXO_F + 
               ZB1SRAZA_NEGRA + ZB1SRAZA_OTRA +
               DDIAB_S + DCISQ_S + DEVP_S + 
               B1SNIVEL_Primaria + B1SNIVEL_Secundaria + B1SNIVEL_Universidad +
               ZCAIMAE:anio, 
             data=base)

# Use tidy() to extract coefficients
coef_urea <- tidy(m_urea) %>%
  select(term, estimate) %>% 
  filter(str_detect(term, "ZCAIMAE") & str_detect(term, ":anio")) %>%
  separate(term, into = c("ZCAIMAE", "anio"), sep = ":") %>%
  mutate(anio = gsub("anio", "", anio),
         ZCAIMAE = gsub("ZCAIMAE", "", ZCAIMAE)) %>% 
  pivot_wider(names_from = "anio", values_from = "estimate")

coef_hemo <- tidy(m_hemo) %>%
  select(term, estimate) %>% 
  filter(str_detect(term, "ZCAIMAE") & str_detect(term, ":anio")) %>%
  separate(term, into = c("ZCAIMAE", "anio"), sep = ":") %>%
  mutate(anio = gsub("anio", "", anio),
         ZCAIMAE = gsub("ZCAIMAE", "", ZCAIMAE)) %>% 
  pivot_wider(names_from = "anio", values_from = "estimate")

coef_fosf <- tidy(m_fosf) %>%
  select(term, estimate) %>% 
  filter(str_detect(term, "ZCAIMAE") & str_detect(term, ":anio")) %>%
  separate(term, into = c("ZCAIMAE", "anio"), sep = ":") %>%
  mutate(anio = gsub("anio", "", anio),
         ZCAIMAE = gsub("ZCAIMAE", "", ZCAIMAE)) %>% 
  pivot_wider(names_from = "anio", values_from = "estimate")

coef_morta <- tidy(m_morta) %>%
  select(term, estimate) %>% 
  filter(str_detect(term, "ZCAIMAE") & str_detect(term, ":anio")) %>%
  separate(term, into = c("ZCAIMAE", "anio"), sep = ":") %>%
  mutate(anio = gsub("anio", "", anio),
         ZCAIMAE = gsub("ZCAIMAE", "", ZCAIMAE)) %>% 
  pivot_wider(names_from = "anio", values_from = "estimate")

quality <- tidy(model) %>%
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


########################

# Create a data frame with all combinations of "IMAE" and "anio"
combinations <- expand.grid(
  IMAE = levels(base$ZCAIMAE),  # Assuming ZCAIMAE is the variable name for "IMAE"
  anio = levels(base$anio)      # Assuming anio is the variable name for "anio"
)

# Convert "IMAE" and "anio" to factors
combinations$IMAE <- as.factor(combinations$IMAE)
combinations$anio <- as.factor(combinations$anio)

# Create a data frame with mean values for other predictors
mean_values <- data.frame(
  CASEDADA = mean(base$CASEDADA, na.rm = TRUE),
  PAC_SEXO_F = mean(base$PAC_SEXO_F, na.rm = TRUE),
  ZB1SRAZA_NEGRA = mean(base$ZB1SRAZA_NEGRA, na.rm = TRUE),
  ZB1SRAZA_OTRA = mean(base$ZB1SRAZA_OTRA, na.rm = TRUE),
  DDIAB_S = mean(base$DDIAB_S, na.rm = TRUE),
  DCISQ_S = mean(base$DCISQ_S, na.rm = TRUE),
  DEVP_S = mean(base$DEVP_S, na.rm = TRUE),
  B1SNIVEL_Primaria = mean(base$B1SNIVEL_Primaria, na.rm = TRUE),
  B1SNIVEL_Secundaria = mean(base$B1SNIVEL_Secundaria, na.rm = TRUE),
  B1SNIVEL_Universidad = mean(base$B1SNIVEL_Universidad, na.rm = TRUE)
)

# Initialize an empty data frame to store predictions
all_predictions <- data.frame()

# Loop through all combinations and get predictions
for (i in 1:nrow(combinations)) {
  mean_values$ZCAIMAE <- combinations$IMAE[i]
  mean_values$anio <- combinations$anio[i]
  predictions <- predict(m_urea, newdata = mean_values)
  result <- data.frame(IMAE = combinations$IMAE[i], anio = combinations$anio[i], Prediction = predictions)
  all_predictions <- rbind(all_predictions, result)
}

# Display the resulting data frame
print(all_predictions)

pred_urea <- all_predictions %>% 
  pivot_wider(names_from = "anio", values_from = "Prediction")
