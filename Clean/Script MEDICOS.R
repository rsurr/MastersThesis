
SESIONES_HD <- read_sav("~/Proyecto Tesis/Databases/SESIONES HD.sav")

MEDICOS <- SESIONES_HD %>%
  left_join(IMAE_num, by=c("ZPMD_IMAE"="ZCAIMAE")) %>% # Column with facility number
  rename(medimae=id) %>% # Name facility number column "medimae"
  filter(depto=="01", # Filter for facilities in Montevideo
         ZPMD_IMAE!="SENNIAD HEMO") %>% #Not pediatric
  group_by(ZB1RMEDICO, medimae, PMD_ANIO) %>% 
  summarise(n=n()) %>% # Get the number of sessions of doctors in each facility (per year)
  mutate(medimae=ifelse(medimae=="", NA, medimae), # Put NA if facility number blank
         ZB1RMEDICO=ifelse(ZB1RMEDICO=="" | # Put NA if doctor name blank
                             ZB1RMEDICO=="  No corresponde" |
                             ZB1RMEDICO=="   No corresponde",
                           NA, ZB1RMEDICO)) %>% 
  filter(!is.na(medimae), !is.na(ZB1RMEDICO)) %>% # Drop facility or doctor NA observations
  group_by(medimae, PMD_ANIO) %>% 
  dummy_cols(select_columns = "medimae") %>% # Dummies per facility (and one observation per facility-doctor-year)
  group_by(ZB1RMEDICO, PMD_ANIO) %>% 
  summarise_at(vars(starts_with("medimae")), funs(.= max(.))) %>% # Collapse to one observation per doctor-year
  select(-"medimae_.")

names(MEDICOS) <- gsub("[._]", "", names(MEDICOS))  # Remove "_" and "."

MEDICOS <- MEDICOS %>%
  mutate(across(starts_with("medimae"), ~ifelse(. == 1, as.integer(sub("medimae", "", cur_column())), .)))

write.csv(
  MEDICOS,
  "MEDICOS.csv", 
  row.names=FALSE)