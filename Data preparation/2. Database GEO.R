library(haven)
library(readr)
library(tidygeocoder)
library(tidyverse)
library(geosphere)
library(sf)

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

IMAE_num <- matrix(c(as.vector(IMAES$IMA_DESC), 1:41, 
                     as.vector(IMAES$IMA_DEP_ID)), ncol=3) %>% 
  as.data.frame()
colnames(IMAE_num) <- c("ZCAIMAE", "choice", "depto")

IMAE_num <- INGRESOS_HD %>%
  group_by(ZCAIMAE) %>% 
  summarize(tipo_imae=first(tipo_imae)) %>%
  left_join(IMAE_num, by="ZCAIMAE") %>% 
  rename(tipo_choice=tipo_imae, tipo_imae=choice)

write.csv(GEO, 
          "GEO.csv", row.names=F)

write.csv(IMAE_num, 
          "IMAE_num.csv", row.names=F)


# MAPA ------------------
montevideo <- st_read("C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/MastersThesis/mapas vectoriales 2011/ine_ccz_mvd.shp")

# Define the CRS for montevideo
crs_montevideo <- st_crs("+proj=utm +zone=21 +south +datum=WGS84 +units=m +no_defs")

# Set the CRS for montevideo
st_crs(montevideo) <- crs_montevideo

# Convert 'IMAES' dataframe to an 'sf' object
IMAES_sf <- st_as_sf(IMAES, coords = c("long_Google", "lat_Google"), 
                     crs = st_crs("+proj=longlat +datum=WGS84"))  %>% 
  mutate(ZCAIMAE=IMA_DESC,
         ) %>% 
  left_join(IMAE_num, by="ZCAIMAE") %>% 
  filter(IMA_DEP_ID=="01",
         ZCAIMAE!="SENNIAD HEMO",
         ZCAIMAE!="SANATORIO AMERICANO")

pacientes_sf <- DATA %>% 
  filter(!is.na(long_Google), !is.na(lat_Google),
         lat_Google<(-34.7),
         long_Google<(-56)) %>% 
            st_as_sf(coords = c("long_Google", "lat_Google"), 
                     crs = st_crs("+proj=longlat +datum=WGS84")) %>% 
  filter(depto=="01",
         ZCAIMAE!="SENNIAD HEMO",
         ZCAIMAE!="SANATORIO AMERICANO") 

# Plot using ggplot
ggplot() +
  # Plot montevideo polygons
  geom_sf(data = montevideo, fill = "lightgrey", color = "white") +
  # Plot points from pacientes_sf
  geom_sf(data = pacientes_sf, color = "#3b528b", size = 0.1) +
  # Plot points from IMAES_sf
  geom_sf(data = IMAES_sf, color = "red", size = 1) +
  facet_wrap(~ZCAIMAE) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),  # Remove x-axis label
        axis.title.y = element_blank(),  # Remove x-axis label
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        strip.background = element_blank()  # Remove background from facet titles
        )
ggsave("maps_imae.png", dpi = 500, scale=3)

# Plot using ggplot
ggplot() +
  # Plot montevideo polygons
  geom_sf(data = montevideo, fill = "lightgrey", color = "white") +
  # Plot points from pacientes_sf
  geom_sf(data = pacientes_sf, color = "#3b528b", size = 0.1) +
  # Plot points from IMAES_sf
  geom_sf(data = IMAES_sf, color = "red", size = 1) +
  facet_wrap(~tipo_choice) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),  # Remove x-axis label
        axis.title.y = element_blank(),  # Remove x-axis label
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        strip.background = element_blank()  # Remove background from facet titles
  )
ggsave("maps_type.png", dpi = 500, scale=3)

labs <- c("Insurance has provider", "Insurance doesn't have proider")
names(labs) <- c("1","0")

# Plot using ggplot
ggplot() +
  # Plot montevideo polygons
  geom_sf(data = montevideo, fill = "lightgrey", color = "white") +
  # Plot points from pacientes_sf
  geom_sf(data = pacientes_sf, color = "#3b528b", size = 0.1) +
  # Plot points from IMAES_sf
  geom_sf(data = IMAES_sf, color = "red", size = 1) +
  facet_wrap(~tiene_imae, labeller = labeller(tiene_imae=labs)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),  # Remove x-axis label
        axis.title.y = element_blank(),  # Remove x-axis label
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        strip.background = element_blank()  # Remove background from facet titles
  )
ggsave("maps_market.png", dpi = 500, scale=3)

pacientes_tipopac_sf <- 
  pacientes_sf %>% 
  filter(!is.na(tipo_pac))

labs <- c("Active", "Inactive")
names(labs) <- c("Activo","Inactivo")

# Plot using ggplot
ggplot() +
  # Plot montevideo polygons
  geom_sf(data = montevideo, fill = "lightgrey", color = "white") +
  # Plot points from pacientes_sf
  geom_sf(data = pacientes_tipopac_sf, 
          color = "#3b528b", size = 0.1) +
  # Plot points from IMAES_sf
  geom_sf(data = IMAES_sf, color = "red", size = 1) +
  facet_wrap(~tipo_pac, labeller = labeller(tipo_pac=labs)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),  # Remove x-axis label
        axis.title.y = element_blank(),  # Remove x-axis label
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        strip.background = element_blank()  # Remove background from facet titles
  )
ggsave("maps_patient.png", dpi = 500, scale=3)


