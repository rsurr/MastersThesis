library(readr)
library(mlogit)
library(tidyverse)
library(peakRAM)
library(haven)
library(kableExtra)



DATA_INGRESOS <- INGRESOS_HD2 %>% 
  left_join(GEO, by="CAPACNUM") %>% 
  left_join(IMAE_num, by=c("ZCAIMAE"="ZCAIMAE")) %>%
  #left_join(quality_reg, by=c("anio_solicitud"="anio")) %>%
  filter(depto=="01",
         ZCAIMAE!="SENNIAD HEMO") %>% 
  filter(!is.na(dist18))  %>% # FILTRO PROVISORIO SACANDO PACIENTES CON DIST NA (60 OBS)
  filter(!is.na(long_Google), !is.na(lat_Google),
         lat_Google<(-34.7),
         long_Google<(-56),
         lat_Google>(-34.8625582510362),
         long_Google>(-56.168713335456474)) %>% 
  mutate(chain=
               case_when(ZCAIMAE %in% IMAE_DIAVERUM ~ "DIAVERUM",
                         ZCAIMAE %in% IMAE_CENEU ~ "CENEU",
                         ZCAIMAE %in% IMAE_NEPHROS ~ "NEPHROS",
                         ZCAIMAE %in% IMAE_SARI ~ "SARI",
                         ZCAIMAE %in% IMAE_PUBLICO ~ "PUBLICO",
                         ZCAIMAE %in% IMAE_PRIVADO ~ "PRIVADO",
                         TRUE ~ NA))

pacientes_sf <- DATA_INGRESOS %>% 
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
  facet_wrap(~chain) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),  # Remove x-axis label
        axis.title.y = element_blank(),  # Remove x-axis label
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        strip.background = element_blank()  # Remove background from facet titles
  )


result_tiene <- DATA_INGRESOS %>%
  filter(tiene_imae==1) %>% 
  group_by(ZCAIMAE) %>%
  summarise(Frequency = n()) %>%
  mutate(Percentage = round(Frequency / sum(Frequency) * 100, 2),
         ZCAIMAE=str_to_title(ZCAIMAE)) %>%
  arrange(desc(Frequency)) %>%
  add_row(ZCAIMAE = "Total", Frequency = sum(.$Frequency), Percentage = 100)

result_notiene <- DATA_INGRESOS %>%
  filter(tiene_imae==0) %>% 
  group_by(ZCAIMAE) %>%
  summarise(Frequency = n()) %>%
  mutate(Percentage = round(Frequency / sum(Frequency) * 100, 2),
         ZCAIMAE=str_to_title(ZCAIMAE)) %>%
  arrange(desc(Frequency)) %>%
  add_row(ZCAIMAE = "Total", Frequency = sum(.$Frequency), Percentage = 100) 

inner_join(result_tiene, result_notiene, by="ZCAIMAE") %>%
  kable("latex", booktabs = TRUE) %>% 
  writeLines("shares_noreste.tex")


result_INST <- DATA_INGRESOS %>%
  group_by(ZCASINST) %>%
  summarise(Frequency = n()) %>%
  mutate(Percentage = round(Frequency / sum(Frequency) * 100, 2),
         ZCASINST=str_to_title(ZCASINST)) %>%
  arrange(desc(Frequency)) %>%
  add_row(ZCASINST = "Total", Frequency = sum(.$Frequency), Percentage = 100)

result_INST %>%
  kable("latex", booktabs = TRUE) %>% 
  writeLines("inst_noreste.tex")



SESIONES_HD %>%
  group_by(DCOMP) %>%
  summarise(Frequency = n()) %>%
  mutate(Percentage = round(Frequency / sum(Frequency) * 100, 2)) %>%
  arrange(desc(Frequency)) %>%
  kable("latex", booktabs = TRUE) %>% 
  writeLines("COMP.tex")

SESIONES_HD %>%
  filter(DCOMP=="S") %>% 
  group_by(ZDCOMPD0) %>%
  summarise(Frequency = n()) %>%
  mutate(Percentage = round(Frequency / sum(Frequency) * 100, 2)) %>%
  filter(Percentage>1) %>% 
  arrange(desc(Frequency)) %>%
  kable("latex", booktabs = TRUE) %>% 
  writeLines("COMP0.tex")

SESIONES_HD %>%
  filter(DCOMP=="S") %>% 
  group_by(ZDCOMPD1) %>%
  summarise(Frequency = n()) %>%
  mutate(Percentage = round(Frequency / sum(Frequency) * 100, 2)) %>%
  filter(Percentage>1) %>% 
  arrange(desc(Frequency)) %>%
  kable("latex", booktabs = TRUE) %>% 
  writeLines("COMP1.tex")

SESIONES_HD %>%
  filter(DCOMP=="S") %>% 
  group_by(ZDCOMPD2) %>%
  summarise(Frequency = n()) %>%
  mutate(Percentage = round(Frequency / sum(Frequency) * 100, 2)) %>%
  filter(Percentage>1) %>% 
  arrange(desc(Frequency)) %>%
  kable("latex", booktabs = TRUE) %>% 
  writeLines("COMP2.tex")

library(viridis)

DATA_INGRESOS %>%
  left_join(IMAE_num2, by="ZCAIMAE") %>% 
  group_by(ZCAIMAE) %>%
  summarise(Frequency = n(), tipo_imae=first(tipo_imae)) %>%
  mutate(Percentage = round(Frequency / sum(Frequency) * 100, 2)) %>% 
  ggplot(aes(y = Percentage, x = reorder(ZCAIMAE, -Percentage), fill = tipo_imae)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_fill_viridis_d(end = 0.8, name = "Provider type") +  # Adding legend title
  theme(
    legend.position = "bottom",
    axis.title.x = element_blank(),  # Remove x-axis label
    axis.title.y = element_blank(),  # Remove x-axis label
    panel.background = element_rect(fill = "white"),  # Change background to white
    panel.grid.major = element_line(color = "gray"),  # Change major gridlines to gray
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    strip.background = element_blank()  # Remove background from facet titles
  ) +
  scale_x_discrete(labels = function(x) substr(x, nchar(x)-7, nchar(x))) +
  coord_cartesian(ylim = c(0, 1))

