

cor_quality_adj <- quality %>% select(-c(IMAE, anio, tipo_choice, tipo_imae2)) %>% cor(use="complete.obs")

cor_quality_non <- non_adj_quality %>%  ungroup() %>% 
  select(-c(IMAE, anio, tipo_choice, tipo_imae2)) %>% 
  cor(use="complete.obs")

# Fit the linear regression model

fe_urea <- lm(urea17 ~ - 1 + 
               CAPACNUM + ZCAIMAE:anio, data=base)

fe_hemo <- lm(hemo10 ~ - 1 + 
               CAPACNUM + ZCAIMAE:anio, data=base)

fe_fosf <- lm(fosf55 ~ - 1 + 
               CAPACNUM + ZCAIMAE:anio, data=base)

fe_surv <- lm(surv ~ - 1 + 
               CAPACNUM + ZCAIMAE:anio, data=base)

fe_comp <- lm(comp ~ - 1 + 
               CAPACNUM + ZCAIMAE:anio, data=base)

fe_sept <- lm(sept ~ - 1 + 
               CAPACNUM + ZCAIMAE:anio, data=base)

fe_peso <- lm(peso ~ - 1 + 
               CAPACNUM + ZCAIMAE:anio, data=base)

fe_URR <- lm(URR65 ~ - 1 + 
              CAPACNUM + ZCAIMAE:anio, data=base)

fe_ktv <- lm(ktv12 ~ - 1 + 
              CAPACNUM + ZCAIMAE:anio, data=base)

library(tidyverse)
library(broom)
coef_ktv <- tidy(fe_ktv) %>%
  select(term, estimate) %>% 
  filter(str_detect(term, "ZCAIMAE") & str_detect(term, ":anio")) %>%
  separate(term, into = c("ZCAIMAE", "anio"), sep = ":") %>%
  mutate(anio = gsub("anio", "", anio),
         IMAE = gsub("ZCAIMAE", "", ZCAIMAE)) %>% 
  rename(ktv=estimate) %>% 
  select(anio, ktv, IMAE)

coef_urea <- extract_coefficients(fe_urea)
coef_hemo <- extract_coefficients(fe_hemo)
coef_fosf <- extract_coefficients(fe_fosf)
coef_surv <- extract_coefficients(fe_surv)
coef_comp <- extract_coefficients(fe_comp)
coef_sept <- extract_coefficients(fe_sept)
coef_peso <- extract_coefficients(fe_peso)
coef_URR <- extract_coefficients(fe_URR)
coef_ktv <- extract_coefficients(fe_ktv)

fe_quality <- left_join(coef_urea, coef_surv, by=c("IMAE", "anio")) %>% 
  left_join(coef_fosf, by=c("IMAE", "anio")) %>%
  left_join(coef_hemo, by=c("IMAE", "anio")) %>%
  left_join(coef_comp, by=c("IMAE", "anio")) %>%
  left_join(coef_sept, by=c("IMAE", "anio")) %>%
  left_join(coef_peso, by=c("IMAE", "anio")) %>%
  left_join(coef_URR, by=c("IMAE", "anio")) %>%
  left_join(coef_ktv, by=c("IMAE", "anio"))

cor_quality_fe <- fe_quality %>% select(-c(IMAE, anio)) %>% cor(use="complete.obs")

URR_measure <- left_join(pred_URR, coef_URR, by=c("IMAE", "anio")) %>% 
  rename(adj_URR=URR) %>% 
  left_join(non_adj_quality[c("URR", "IMAE", "anio")], by=c("IMAE", "anio")) %>% 
  rename(non_URR=URR)

cor_URR <- URR_measure %>% select(-c(IMAE, anio)) %>% cor(use="complete.obs")

ktv_measure <- left_join(pred_ktv, coef_ktv, by=c("IMAE", "anio")) %>% 
  rename(adj_ktv=ktv) %>% 
  left_join(non_adj_quality[c("ktv", "IMAE", "anio")], by=c("IMAE", "anio")) %>% 
  rename(non_ktv=ktv)

cor_ktv <- ktv_measure %>% select(-c(IMAE, anio)) %>% cor(use="complete.obs")

urea_measure <- left_join(pred_urea, coef_urea, by=c("IMAE", "anio")) %>% 
  rename(adj_urea=urea) %>% 
  left_join(non_adj_quality[c("urea", "IMAE", "anio")], by=c("IMAE", "anio")) %>% 
  rename(non_urea=urea)

cor_urea <- urea_measure %>% select(-c(IMAE, anio)) %>% cor(use="complete.obs")

surv_measure <- left_join(pred_surv, coef_surv, by=c("IMAE", "anio")) %>% 
  rename(adj_surv=surv) %>% 
  left_join(non_adj_quality[c("surv", "IMAE", "anio")], by=c("IMAE", "anio")) %>% 
  rename(non_surv=surv)

cor_surv <- surv_measure %>% select(-c(IMAE, anio)) %>% cor(use="complete.obs")

sept_measure <- left_join(pred_sept, coef_sept, by=c("IMAE", "anio")) %>% 
  rename(adj_sept=sept) %>% 
  left_join(non_adj_quality[c("sept", "IMAE", "anio")], by=c("IMAE", "anio")) %>% 
  rename(non_sept=sept)

cor_sept <- sept_measure %>% select(-c(IMAE, anio)) %>% cor(use="complete.obs")

hemo_measure <- left_join(pred_hemo, coef_hemo, by=c("IMAE", "anio")) %>% 
  rename(adj_hemo=hemo) %>% 
  left_join(non_adj_quality[c("hemo", "IMAE", "anio")], by=c("IMAE", "anio")) %>% 
  rename(non_hemo=hemo)

cor_hemo <- hemo_measure %>% select(-c(IMAE, anio)) %>% cor(use="complete.obs")

peso_measure <- left_join(pred_peso, coef_peso, by=c("IMAE", "anio")) %>% 
  rename(adj_peso=peso) %>% 
  left_join(non_adj_quality[c("peso", "IMAE", "anio")], by=c("IMAE", "anio")) %>% 
  rename(non_peso=peso)

cor_peso <- peso_measure %>% select(-c(IMAE, anio)) %>% cor(use="complete.obs")

comp_measure <- left_join(pred_comp, coef_comp, by=c("IMAE", "anio")) %>% 
  rename(adj_comp=comp) %>% 
  left_join(non_adj_quality[c("comp", "IMAE", "anio")], by=c("IMAE", "anio")) %>% 
  rename(non_comp=comp)

cor_comp <- comp_measure %>% select(-c(IMAE, anio)) %>% cor(use="complete.obs")
