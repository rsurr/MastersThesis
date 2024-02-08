library(tidyverse)
library(haven)
library(readr)

PACIENTES <- read_sav("C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/Databases/PACIENTES.sav")
MENSUALES_HD <- read_sav("C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/Databases/MENSUALES HD.sav") %>% 
  mutate_if(is.character, na_if,"") %>% 
  group_by(CAPACNUM) %>% 
  fill(DDIAB, DCISQ, DEVP) %>% 
  mutate(DDIAB=case_when(DDIAB=="D" ~ NA,
                         .default = as.character(DDIAB)),
         DCISQ=case_when(DCISQ=="D" ~ NA,
                         .default = as.character(DCISQ)),
         DEVP=case_when(DEVP=="D" ~ NA,
                        .default = as.character(DEVP)))
INGRESOS_HD2 <- read_csv("INGRESOS_HD2.csv")

# HEMOGLOBINA
hemo <- MENSUALES_HD %>% 
  group_by(PMD_IMAE, PMD_ANIO, PMD_MES) %>% 
  summarize(hemo=mean(EMHEMOG, na.rm=T)) 

# UREA
urea <- MENSUALES_HD %>% 
  group_by(PMD_IMAE, PMD_ANIO, PMD_MES) %>% 
  summarize(urea=mean(EMAZOEM, na.rm=T)) 

hemo %>% 
  ggplot(aes(y=hemo, x=as.factor(PMD_IMAE))) + geom_bar(stat = "identity")

# COMPLICACIONES
comp <- MENSUALES_HD %>% 
  group_by(PMD_IMAE, PMD_ANIO, PMD_MES) %>% 
  mutate(complicacion=if_else(COMP=="S", 1, 0)) %>% 
  summarize(comp=mean(complicacion, na.rm=T)) 

comp %>% 
  ggplot(aes(y=comp, x=fct_reorder(as.factor(PMD_IMAE), comp))) + 
    geom_bar(stat = "identity")

# INFECCIONES
#[5] "112- infeccion del sitio de salida"                    
#[6] "026- infección"                                       
#[11] "153- septicemia debida a otras causas o infeccion d" 
#[18] "173- septicemia debida a infecc. Del catéter"          
#[19] "030- infección urinaria"                               
#[20] "035- otra infección" 
#[22] "032- infección pulmonar"                               
#[37] "034- infección cutánea"                                
#[38] "171- infección del catéter"
#[43] "150- septicemia debida a inf. De la f.a.v" 
#[58] "123- infección del túnel subcutáneo"                   
#[61] "151- septicemia debida a peritonitis"
#[80] "152- septicemia debida a arteriop. Periférica"
#[95] "053- hepatitis b"                                      

# INFECCIONES SEPTICAS
#[11] "153- septicemia debida a otras causas o infeccion d" 184010162
#[18] "173- septicemia debida a infecc. Del catéter"  184010183        
#[43] "150- septicemia debida a inf. De la f.a.v" 184010159
#[61] "151- septicemia debida a peritonitis" 184010160
#[80] "152- septicemia debida a arteriop. Periférica" 184010161

# SEPSIS
cod_septi <- c(184010162, 184010183, 184010159, 184010160, 184010161)

septi <- MENSUALES_HD %>% 
  group_by(PMD_IMAE, PMD_ANIO, PMD_MES) %>% 
  mutate(septi=if_else(COMP1 %in% cod_septi |
                         COMP2 %in% cod_septi |
                         COMP3 %in% cod_septi, 1, 0)) %>% 
  summarize(septi=mean(septi, na.rm=T)) 

septi %>% 
  ggplot(aes(y=septi, x=fct_reorder(as.factor(PMD_IMAE), septi))) + 
  geom_bar(stat = "identity")

# FALLECIMIENTO
muerte <- MENSUALES_HD %>% 
  group_by(PMD_IMAE, PMD_ANIO, PMD_MES) %>% 
  mutate(muerte=if_else(ZPMD_ESTADO=="FALLECIMIENTO", 1, 0)) %>% 
  summarize(muerte=mean(muerte, na.rm=T)) 

muerte %>% 
  ggplot(aes(y=muerte, x=fct_reorder(as.factor(PMD_IMAE), muerte))) + 
  geom_bar(stat = "identity")

## SESIONES
SESIONES_HD <- read_sav("Proyecto Tesis/Databases/SESIONES HD.sav")

# REUTILIZACION
reut <- SESIONES_HD %>% 
  group_by(PMD_IMAE, PMD_ANIO, PMD_MES) %>% 
  mutate(reut=if_else(DUSOF=="Re uso", 1, 0)) %>% 
  summarise(reut=mean(reut)) 

reut %>% 
  ggplot(aes(y=reut, x=fct_reorder(as.factor(PMD_IMAE), reut))) + 
  geom_bar(stat = "identity")

join <- reut %>% 
  left_join(muerte, by=c("PMD_IMAE", "PMD_ANIO", "PMD_MES")) %>% 
  left_join(comp, by=c("PMD_IMAE", "PMD_ANIO", "PMD_MES")) %>% 
  left_join(septi, by=c("PMD_IMAE", "PMD_ANIO", "PMD_MES")) %>% 
  left_join(hemo, by=c("PMD_IMAE", "PMD_ANIO", "PMD_MES")) %>% 
  left_join(urea, by=c("PMD_IMAE", "PMD_ANIO", "PMD_MES"))

centro <- join %>% group_by(PMD_IMAE) %>% summarize(reut=mean(reut, na.rm=T),
                                                  muerte=mean(muerte, na.rm=T),
                                                  comp=mean(comp, na.rm=T),
                                                  septi=mean(septi, na.rm=T),
                                                  hemo=mean(hemo, na.rm=T),
                                                  urea=mean(urea, na.rm=T)) %>% 
  left_join(IMAES_DIALISIS, by=c("PMD_IMAE"="IMA_ID"))  %>% 
  mutate(Montevideo=if_else(IMA_DEP_ID=="01", "Montevideo", "Interior")) %>% 
  pivot_longer(c(reut, muerte, comp, septi, urea, hemo)) %>% 
  filter(!is.na(Montevideo))
  
labs <- c("Reutilización (frec)", "Mortalidad (frec)", "Complicaciones (frec)", "Sepsis (frec)", "Urea (media)", "Hemoglobina (media)")
names(labs) <- c("reut", "muerte", "comp", "septi", "urea", "hemo")

library(viridis)
library(hrbrthemes)
centro %>% ggplot(aes(y=value, x=Montevideo, color=name)) + geom_boxplot() +
  scale_color_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
    legend.position="none",
    panel.spacing = unit(0, "lines")) +
  facet_wrap(~name, scale="free_y", labeller = labeller(name=labs))

## BOXPLOTS
# REUTILIZACIÓN
centro %>% ggplot(aes(y=reut, x=IMA_TIPO)) + geom_boxplot()
centro %>% ggplot(aes(y=reut, x=Montevideo)) + geom_boxplot()

# MUERTE
centro %>% ggplot(aes(y=muerte, x=IMA_TIPO)) + geom_boxplot()
centro %>% ggplot(aes(y=muerte, x=Montevideo)) + geom_boxplot()

# COMPLICACIONES
centro %>% ggplot(aes(y=comp, x=IMA_TIPO)) + geom_boxplot()
centro %>% ggplot(aes(y=comp, x=Montevideo)) + geom_boxplot()

# SEPTIS
centro %>% ggplot(aes(y=septi, x=IMA_TIPO)) + geom_boxplot()
centro %>% ggplot(aes(y=septi, x=Montevideo)) + geom_boxplot()





