SESIONES_HD <- read_sav("C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/Databases/SESIONES HD.sav") 

SESIONES_HD <-  SESIONES_HD %>% 
  unite("fecha", PMD_ANIO:PMD_MES, sep="-", remove=FALSE) %>% 
  mutate(Date = as.Date(paste(fecha, "-01", sep="")),
         mes = format_ISO8601(Date, precision = "ym"))

write.csv(SESIONES_HD, 
          "C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/MastersThesis/SESIONES_HD.csv", 
          row.names=FALSE)
