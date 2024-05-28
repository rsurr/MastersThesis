
INFORMES <- read_sav("~/Proyecto Tesis/Databases/INFORMES_IMAES_HD.sav") %>% 
  unite("fecha2", CAANIO:CAMES, sep="-") %>% 
  mutate(fecha3=as.Date(paste(fecha2, "-01", sep="")),
         mes_solicitud=format(fecha3, "%Y-%m"))

write.csv(
  INFORMES,
  "INFORMES.csv", 
  row.names=FALSE)