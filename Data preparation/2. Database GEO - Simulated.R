library(haven)
library(readr)
library(tidygeocoder)
library(tidyverse)
library(geosphere)
library(sf)

PACIENTES <- read_sav("C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/Databases/PACIENTES.sav")

pacientes_sesiones_Google <- read_csv("C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/Databases/pacientes_sesiones_Google.csv") %>%
  rename(lat_Google=lat, long_Google=long)

# Coordenadas IMAES
IMAES_DIALISIS <- read_sav("C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/Databases/IMAES DIALISIS.sav") %>% 
  filter(IMA_DESC!="HOSPITAL ITALIANO")
Coord_IMAE_Sim <- read_csv("C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/Databases/Coord IMAE - Simulated.csv") %>% 
  filter(descripción!="HOSPITAL ITALIANO")

IMAES_sim <- left_join(IMAES_DIALISIS, Coord_IMAE_Sim, by=c("IMA_DESC"="descripción")) %>% 
  filter(!is.na(WKT)) %>% 
  mutate(long_Google = as.numeric(str_split(WKT, " ", simplify = T)[, 1]),
         lat_Google = as.numeric(str_split(WKT, " ", simplify = T)[, 2])) %>% 
  arrange(IMA_ID) %>% 
  mutate(id=row_number())

distm_sim <- distm(pacientes_sesiones_Google[c("long_Google", "lat_Google")], 
               IMAES_sim[c("long_Google", "lat_Google")], fun = distGeo) 

# The default unit of distance is meter
# distGeo: Distance on an ellipsoid (the geodesic)

#colnames(dist) <- as.vector(IMAES$IMA_DESC)

GEO_sim <- cbind(pacientes_sesiones_Google, distm_sim)

# Get the names of the variables
variable_names_sim <- names(GEO_sim)

# Get the index of the last 40 variables
last_40_indices_sim <- (length(variable_names_sim) - 39):length(variable_names_sim)

# Add the prefix "dist" to the names of the last 41 variables
new_names_sim <- paste0("dist", variable_names_sim[last_40_indices_sim])

# Rename the variables in the dataset
names(GEO_sim)[last_40_indices_sim] <- new_names_sim

write.csv(GEO_sim, 
          "GEO - Simulated.csv", row.names=F)


