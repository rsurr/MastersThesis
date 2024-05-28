
pacientes_sesiones_Google <- read_csv("C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/Databases/pacientes_sesiones_Google.csv") %>%
  rename(lat_Google=lat, long_Google=long)

# Coordenadas IMAES
IMAES_DIALISIS <- read_sav("C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/Databases/IMAES DIALISIS.sav") %>% 
  filter(IMA_DESC!="HOSPITAL ITALIANO")

Coord_IMAE <- read_csv("C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/Databases/Coord IMAE.csv") %>% 
  filter(descripción!="HOSPITAL ITALIANO")

IMAES <- left_join(IMAES_DIALISIS, Coord_IMAE, by=c("IMA_DESC"="descripción")) %>% 
  filter(!is.na(WKT)) %>% 
  mutate(long_Google = as.numeric(str_split(WKT, " ", simplify = T)[, 1]),
         lat_Google = as.numeric(str_split(WKT, " ", simplify = T)[, 2])) %>% 
  arrange(IMA_ID) %>% 
  mutate(id=row_number())

distm <- distm(pacientes_sesiones_Google[c("long_Google", "lat_Google")], 
               IMAES[c("long_Google", "lat_Google")], fun = distGeo) 

GEO <- cbind(pacientes_sesiones_Google, distm)

# Get the names of the variables
variable_names <- names(GEO)

# Get the index of the last 40 variables
last_40_indices <- (length(variable_names) - 39):length(variable_names)

# Add the prefix "dist" to the names of the last 41 variables
new_names <- paste0("dist", variable_names[last_40_indices])

# Rename the variables in the dataset
names(GEO)[last_40_indices] <- new_names

IMAE_num <- matrix(c(as.vector(IMAES$IMA_DESC),
                     as.vector(IMAES$id),
                     as.vector(IMAES$IMA_DEP_ID),
                     as.vector(IMAES$IMA_ID)), ncol=4) %>% 
  as.data.frame() %>% 
  rename(ZCAIMAE=V1, id=V2, depto=V3, num_choice=V4)

rm(Coord_IMAE, distm, GEO, IMAES, IMAES_DIALISIS, pacientes_sesiones_Google)

write.csv(IMAE_num, 
          "IMAE_num.csv", row.names=F)