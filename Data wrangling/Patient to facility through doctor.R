SESIONES_HD <- read_sav("Databases/SESIONES HD.sav") 

SESIONES_HD <-  SESIONES_HD %>% 
  unite("fecha", PMD_ANIO:PMD_MES, sep="-", remove=FALSE) %>% 
  mutate(Date = as.Date(paste(fecha, "-01", sep="")),
         mes = format_ISO8601(Date, precision = "ym"))

MEDICOS <- SESIONES_HD %>% 
  group_by(ZB1RMEDICO, ZPMD_IMAE) %>% 
  summarise(n=n()) %>%
  mutate(ZPMD_IMAE=ifelse(ZPMD_IMAE=="", NA, ZPMD_IMAE),
         ZB1RMEDICO=ifelse(ZB1RMEDICO=="" |
                             ZB1RMEDICO=="  No corresponde" |
                             ZB1RMEDICO=="   No corresponde",
                           NA, ZB1RMEDICO)) %>% 
  filter(!is.na(ZPMD_IMAE), !is.na(ZB1RMEDICO)) %>%
  group_by(ZB1RMEDICO) %>% 
  mutate(prop=round(n/sum(n), 4),
         imae_rank=row_number(desc(prop))) %>% 
  select(-c(n, prop)) %>% 
  pivot_wider(names_from = imae_rank, values_from = ZPMD_IMAE,
              names_prefix = "imae") %>% 
  select(ZB1RMEDICO, imae1, imae2, imae3, imae4, imae5, imae6,
         imae7, imae8, imae9, imae10, imae11, imae12, imae13, 
         imae14, imae15, imae16, imae17, imae18, imae19)


PACIENTES_MEDICOS <- left_join(INGRESOS_HD, MEDICOS, by=c("ZB1SMEDIC"="ZB1RMEDICO")) %>%
  filter(ZCASDEPAR=="MONTEVIDEO") %>% 
  mutate(med_imae=case_when(ZCAIMAE==imae1 |
                              ZCAIMAE==imae2 | 
                              ZCAIMAE==imae3 | 
                              ZCAIMAE==imae4 | 
                              ZCAIMAE==imae5 | 
                              ZCAIMAE==imae6 | 
                              ZCAIMAE==imae7 | 
                              ZCAIMAE==imae8 | 
                              ZCAIMAE==imae9 | 
                              ZCAIMAE==imae10 | 
                              ZCAIMAE==imae11 | 
                              ZCAIMAE==imae12 | 
                              ZCAIMAE==imae13 |
                              ZCAIMAE==imae14 |
                              ZCAIMAE==imae15 |
                              ZCAIMAE==imae16 |
                              ZCAIMAE==imae17 |
                              ZCAIMAE==imae18 |
                              ZCAIMAE==imae19
                            ~ 1, 
                            TRUE ~ 0))

mean(PACIENTES_MEDICOS$med_imae)
