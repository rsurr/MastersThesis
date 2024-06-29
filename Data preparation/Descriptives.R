library(readr)
INGRESOS_HD <- read_csv("INGRESOS_HD2.csv")

SESIONES_HD <- read_sav("C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/Databases/SESIONES HD.sav") 

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
  #filter(ZCASDEPAR=="MONTEVIDEO") %>% 
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
                            TRUE ~ 0)) %>% 
  group_by(CAPACNUM) %>% 
  slice_max(CASEDADA, with_ties = FALSE)

PACIENTES <- SESIONES_HD %>% 
  group_by(CAPACNUM, ZPMD_IMAE) %>% 
  summarise(n=n()) %>%
  mutate(ZPMD_IMAE=ifelse(ZPMD_IMAE=="", NA, ZPMD_IMAE)) %>% 
  filter(!is.na(ZPMD_IMAE), !is.na(CAPACNUM)) %>%
  group_by(CAPACNUM) %>% 
  mutate(prop=round(n/sum(n), 4),
         cantidad=n(),
         imae_rank=row_number(desc(prop)))

PACIENTES2 <- PACIENTES  %>% 
  select(-c(n, prop)) %>% 
  pivot_wider(names_from = imae_rank, values_from = ZPMD_IMAE,
              names_prefix = "imae") %>% 
  select(CAPACNUM, imae1, imae2, imae3, imae4, imae5, imae6,
         imae7, imae8, imae9, imae10, imae11, imae12, imae13) %>% 
  left_join(INGRESOS_HD, PACIENTES, by="CAPACNUM") %>%
  #filter(ZCASDEPAR=="MONTEVIDEO") %>% 
  mutate(primera_eleccion=case_when(ZCAIMAE==imae1
                            ~ 1, 
                            TRUE ~ 0)) %>% 
  mutate(med_imae=ifelse(is.na(med_imae), 0, med_imae))

PACIENTES_n <- PACIENTES %>% group_by(CAPACNUM) %>% top_n(1, prop) 

library(vtable)
sumtable(PACIENTES2, vars=c("med_imae",
                            "primera_eleccion"), 
         #out="latex"
         )

sumtable(PACIENTES_n, vars=c("n",
                            "prop",
                            "cantidad"), 
         #out="latex"
)

# By facility type
## Sin condicionar
INGRESOS_HD %>% 
  group_by(tipo_imae) %>% 
  summarize(inst_imae=mean(inst_imae)*100) %>% 
  ggplot(aes(y=inst_imae, x=tipo_imae, fill=tipo_imae)) + geom_bar(stat="identity") +
  ggtitle("Percentage of patients starting dialysis at their provider",
          subtitle="By facility type") + # Title 
  ylab("Percentage") + # Y axis label
  xlab("Facility type") + # X axis label
  scale_fill_viridis(discrete = T, begin = 0.80, end = 0.5)+
  theme_minimal() + # Theme
  theme(legend.position = 'none') # Legends position

ggsave(filename = "Facility.png", dpi=700)


## Condicionando
INGRESOS_HD %>% 
  group_by(tipo_imae) %>% 
  filter(tiene_imae==1) %>% 
  summarize(inst_imae=mean(inst_imae)*100) %>% 
  ggplot(aes(y=inst_imae, x=tipo_imae, fill=tipo_imae)) + geom_bar(stat="identity") +
  ggtitle("Percentage of patients starting dialysis at their provider",
          subtitle="By facility type. Conditioned to provider having a facility") + # Title 
  ylab("Percentage") + # Y axis label
  xlab("Facility type") + # X axis label
  scale_fill_viridis(discrete = T, begin = 0.80, end = 0.5)+
  theme_minimal() + # Theme
  theme(legend.position = 'none') # Legends position

#-------------------------------------------------------------
# By provider type
## Sin condicionar
INGRESOS_HD %>% 
  group_by(tipo_inst) %>% 
  summarize(inst_imae=mean(inst_imae)*100) %>% 
  ggplot(aes(y=inst_imae, x=tipo_inst, fill=tipo_inst)) + geom_bar(stat="identity") +
  ggtitle("Percentage of patients starting dialysis at their provider",
          subtitle= "By provider type") + # Title 
  ylab("Percentage") + # Y axis label
  ylim(0, 100) +
  xlab("Provider type") + # X axis label
  scale_fill_viridis(discrete = T, begin = 0.80, end = 0.5)+
  theme_minimal() + # Theme
  theme(legend.position = 'none') # Legends position

ggsave(filename = "Provider.png", dpi=700)


## Condicionando
INGRESOS_HD %>%
  filter(tiene_imae==1) %>%
  filter(tipo_inst!="CORPORATIVO") %>% 
  group_by(tipo_inst) %>% 
  summarize(inst_imae=mean(inst_imae)*100) %>% 
  ggplot(aes(y=inst_imae, x=tipo_inst, fill=tipo_inst)) + geom_bar(stat="identity") +
  ggtitle("Percentage of patients starting dialysis at their provider",
          subtitle="By provider type. Conditioned to provider having a facility") + # Title 
  ylab("Percentage") + # Y axis label
  ylim(0, 100) +
  xlab("Provider type") + # X axis label
  scale_fill_viridis(discrete = T, begin = 0.80, end = 0.5)+
  theme_minimal() + # Theme
  theme(legend.position = 'none') # Legends position

ggsave(filename = "Provider_con.png", dpi=700)

#-------------------------------------------------------------

#-------------------------------------------------------------
# By pacient type
## Sin condicionar
INGRESOS_HD %>% 
  filter(tipo_inst!="CORPORATIVO") %>% 
  group_by(tipo_pac) %>% 
  summarize(inst_imae=mean(inst_imae)*100) %>% 
  ggplot(aes(y=inst_imae, x=tipo_pac, fill=tipo_pac)) + geom_bar(stat="identity") +
  ggtitle("Percentage of patients starting dialysis at their provider",
          subtitle= "By patient type") + # Title 
  ylab("Percentage") + # Y axis label
  ylim(0, 100) +
  xlab("Patient type") + # X axis label
  scale_fill_viridis(discrete = T, begin = 0.80, end = 0.5)+
  theme_minimal() + # Theme
  theme(legend.position = 'none') # Legends position

ggsave(filename = "Pacient.png", dpi=700)

## Condicionando
INGRESOS_HD %>%
  filter(tiene_imae==1) %>%
  filter(tipo_inst!="CORPORATIVO") %>% 
  group_by(tipo_pac) %>% 
  summarize(inst_imae=mean(inst_imae)*100) %>% 
  ggplot(aes(y=inst_imae, x=tipo_pac, fill=tipo_pac)) + geom_bar(stat="identity") +
  ggtitle("Percentage of patients starting dialysis at their provider",
          subtitle="By patient type. Conditioned to provider having a facility") + # Title 
  ylab("Percentage") + # Y axis label
  ylim(0, 100) +
  xlab("Patient type") + # X axis label
  scale_fill_viridis(discrete = T, begin = 0.80, end = 0.5)+
  theme_minimal() + # Theme
  theme(legend.position = 'none') # Legends position

ggsave(filename = "Pacient_con.png", dpi=700)

#-------------------------------------------------------------

# Providers que tienen facility
## By provider type
INGRESOS_HD %>% 
  group_by(tipo_inst) %>% 
  filter(tipo_inst!="CORPORATIVO") %>% 
  summarize(tiene_imae=mean(tiene_imae)*100) %>% 
  ggplot(aes(y=tiene_imae, x=tipo_inst, fill=tipo_inst)) + geom_bar(stat="identity") +
  ggtitle("Percentage of patients whose provider has a facility", # Title
          subtitle="By provider type") + # Title 
  ylab("Percentage") + # Y axis label
  ylim(0, 100) +
  xlab("Provider type") + # X axis label
  scale_fill_viridis(discrete = T, begin = 0.80, end = 0.5)+
  theme_minimal() + # Theme
  theme(legend.position = 'none') # Legends position


#By facility type
INGRESOS_HD %>% 
  group_by(tipo_imae) %>% 
  summarize(tiene_imae=mean(tiene_imae)*100) %>% 
  ggplot(aes(y=tiene_imae, x=tipo_imae, fill=tipo_imae)) + geom_bar(stat="identity") +
  ggtitle("Percentage of patients whose provider has a facility", # Title
          subtitle="By facility type") + # Title 
  ylab("Percentage") + # Y axis label
  ylim(0, 100) +
  xlab("Facility type") + # X axis label
  scale_fill_viridis(discrete = T, begin = 0.80, end = 0.5)+
  theme_minimal() + # Theme
  theme(legend.position = 'none') # Legends position

#----------------------------------------------------------


PACIENTES_MEDICOS %>% 
  group_by(tipo_imae) %>% 
  filter(inst_imae==0) %>% 
  summarize(med_imae=mean(med_imae)*100) %>% 
  ggplot(aes(y=med_imae, x=tipo_imae, fill=tipo_imae)) + geom_bar(stat="identity") +
  ggtitle("Percentage of patients starting dialysis at a facility where their doctor has worked",
          subtitle="By facility type") + # Title 
  ylab("Percentage") + # Y axis label
  ylim(0, 100) +
  xlab("Facility type") + # X axis label
  scale_fill_viridis(discrete = T, begin = 0.80, end = 0.5)+
  theme_minimal() + # Theme
  theme(legend.position = 'none') # Legends position

ggsave(filename = "Medico_Pacient_by Provider_Con.png", dpi=700)


## By provider type
# Sin condicionar
PACIENTES_MEDICOS %>% 
  group_by(tipo_inst) %>% 
  summarize(med_imae=mean(med_imae)*100) %>% 
  ggplot(aes(y=med_imae, x=tipo_inst, fill=tipo_inst)) + geom_bar(stat="identity") +
  ggtitle("Percentage of patients starting dialysis at a facility where their doctor has worked",
          subtitle="By provider type") + # Title 
  ylab("Percentage") + # Y axis label
  ylim(0, 100) +
  xlab("Facility type") + # X axis label
  scale_fill_viridis(discrete = T, begin = 0.80, end = 0.5)+
  theme_minimal() + # Theme
  theme(legend.position = 'none') # Legends position


# Condicionando
PACIENTES_MEDICOS %>% 
  group_by(tipo_inst) %>% 
  filter(inst_imae==0, tiene_imae==1) %>% 
  summarize(med_imae=mean(med_imae)*100) %>% 
  ggplot(aes(y=med_imae, x=tipo_inst, fill=tipo_inst)) + geom_bar(stat="identity") +
  ggtitle("Percentage of patients starting dialysis at a facility where their doctor has worked",
          subtitle="By provider type. Conditioned to patient not attending facility from provider") + # Title 
  ylab("Percentage") + # Y axis label
  ylim(0, 100) +
  xlab("Facility type") + # X axis label
  scale_fill_viridis(discrete = T, begin = 0.80, end = 0.5)+
  theme_minimal() + # Theme
  theme(legend.position = 'none') # Legends position

ggsave(filename = "Medico_Pacient_by Provider_Con.png", dpi=700)


