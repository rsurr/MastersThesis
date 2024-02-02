library(haven)
library(readr)
library(tidygeocoder)
library(tidyverse)
library(geosphere)

PACIENTES <- read_sav("C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/Databases/PACIENTES.sav")

# ArcGIS
pacientes_sesiones <- PACIENTES %>% filter(CAPACNUM %in% SESIONES_HD$CAPACNUM) %>% 
  mutate(dir=result <- paste(PAC_TV_DESC, " ", PAC_DIRECCION, " ", PAC_NPTA, ", ", PAC_LOCALIDAD, ", ", PAC_LOC_DESC, 
                             ", ", PAC_DEP_DESC, ", ", "URUGUAY")) %>%
  geocode(address = dir, method="arcgis")

write.csv(pacientes_sesiones, 
          "C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/Databases/pacientes_sesiones_ArcGIS.csv", row.names=F)

# Google
pacientes_sesionesGoogle <- PACIENTES %>% filter(CAPACNUM %in% SESIONES_HD$CAPACNUM) %>% 
  mutate(dir=result <- paste(PAC_TV_DESC, " ", PAC_DIRECCION, " ", PAC_NPTA, ", ", PAC_LOCALIDAD, ", ", PAC_LOC_DESC, 
                             ", ", PAC_DEP_DESC, ", ", "URUGUAY")) %>%
  geocode(address = dir, method="google")

write.csv(pacientes_sesionesGoogle, 
          "C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/Databases/pacientes_sesiones_Google.csv", row.names=F)

# Comparación
pacientes_sesiones_Google <- read_csv("C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/Databases/pacientes_sesiones_Google.csv") %>%
  rename(lat_Google=lat, long_Google=long)

pacientes_sesiones_ArcGIS <- read_csv("C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/Databases/pacientes_sesiones_ArcGIS.csv") %>%
  rename(lat_ArcGIS=lat, long_ArcGIS=long)

pacientes_sesiones_comparativo <- 
  inner_join(pacientes_sesiones_ArcGIS, pacientes_sesiones_Google,
             by="CAPACNUM") %>% 
  mutate(lat_comp=lat_ArcGIS-lat_Google,
         long_comp=long_ArcGIS-lat_Google) %>% 
  select(starts_with(c("lat", "long")))

# Coordenadas IMAES
IMAES_DIALISIS <- read_sav("C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/Databases/IMAES DIALISIS.sav")
Coord_IMAE <- read_csv("C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/Databases/Coord IMAE.csv")

IMAES <- left_join(IMAES_DIALISIS, Coord_IMAE, by=c("IMA_DESC"="descripción")) %>% 
  filter(!is.na(WKT)) %>% 
  mutate(long_Google = as.numeric(str_split(WKT, " ", simplify = T)[, 1]),
         lat_Google = as.numeric(str_split(WKT, " ", simplify = T)[, 2])) 

dist <- distm(pacientes_sesiones_Google[c("long_Google", "lat_Google")], 
              IMAES[c("long_Google", "lat_Google")], fun = distGeo) 

# The default unit of distance is meter
# distGeo: Distance on an ellipsoid (the geodesic)

#colnames(dist) <- as.vector(IMAES$IMA_DESC)

GEO <- cbind(pacientes_sesiones_Google, dist)

# Get the names of the variables
variable_names <- names(GEO)

# Get the index of the last 41 variables
last_41_indices <- (length(variable_names) - 40):length(variable_names)

# Add the prefix "dist" to the names of the last 41 variables
new_names <- paste0("dist", variable_names[last_41_indices])

# Rename the variables in the dataset
names(GEO)[last_41_indices] <- new_names

IMAE_num <- matrix(c(as.vector(IMAES$IMA_DESC), 1:41, as.vector(IMAES$IMA_DEP_ID)), ncol=3) %>% as.data.frame()
colnames(IMAE_num) <- c("ZCAIMAE", "choice", "depto")

write.csv(GEO, 
          "GEO.csv", row.names=F)

write.csv(IMAE_num, 
          "IMAE_num.csv", row.names=F)
