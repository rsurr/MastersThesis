
coef_surv <- tidy(m_surv) %>%
  select(term, estimate) %>% 
  filter(str_detect(term, "CAPACNUM")) %>%
  separate(term, into = c("CAPACNU", "CAPACNUM"), sep = "M") %>%
  select(CAPACNUM, estimate) %>% 
  mutate(CAPACNUM=as.factor(CAPACNUM))

a <- right_join(base, coef_surv, by="CAPACNUM") %>% select(CAPACNUM, estimate) %>% 
  group_by(CAPACNUM) %>% 
  summarise(n=n(),
            estimate=first(estimate),
            abs=abs(estimate))

b <- base %>% group_by(CAPACNUM) %>% summarise(n=n())
  

mean_values <- data.frame(
  CASEDADA = mean(base$CASEDADA, na.rm = TRUE),
  meses = mean(base$meses, na.rm = TRUE),
  PAC_SEXO_F = mean(base$PAC_SEXO_F, na.rm = TRUE),
  ZB1SRAZA_NEGRA = mean(base$ZB1SRAZA_NEGRA, na.rm = TRUE),
  ZB1SRAZA_OTRA = mean(base$ZB1SRAZA_OTRA, na.rm = TRUE),
  DDIAB_S = mean(base$DDIAB_S, na.rm = TRUE),
  DCISQ_S = mean(base$DCISQ_S, na.rm = TRUE),
  DEVP_S = mean(base$DEVP_S, na.rm = TRUE),
  B1SNIVEL_Primaria = mean(base$B1SNIVEL_Primaria, na.rm = TRUE),
  B1SNIVEL_Secundaria = mean(base$B1SNIVEL_Secundaria, na.rm = TRUE),
  B1SNIVEL_Universidad = mean(base$B1SNIVEL_Universidad, na.rm = TRUE),
  tipo_inst_IAMCIAMPP = mean(base$tipo_inst_IAMCIAMPP, na.rm = TRUE),
  tipo_inst_SEGUROPRIVADO = mean(base$tipo_inst_SEGUROPRIVADO, na.rm = TRUE),
  tipo_inst_CORPORATIVO = mean(base$tipo_inst_CORPORATIVO, na.rm = TRUE),
  ocupado = mean(base$ocupado, na.rm = TRUE),
  ECREAV = mean(base$ECREAV, na.rm = TRUE),
  CAPACNUM = "190033",
  ZCAIMAE="HOSPITAL DE CLINICAS",
  anio="2011"
)

predictions <- predict(m_surv, newdata = mean_values)

