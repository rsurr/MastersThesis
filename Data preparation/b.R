library(tidyverse)
n_distinct(INGRESOS_HD$CAPACNUM)


library(haven)
SESIONES_HD <- read_sav("Proyecto Tesis/Databases/SESIONES HD.sav")
n_distinct(SESIONES_HD$CAPACNUM)


library(haven)
PACIENTES <- read_sav("Proyecto Tesis/Databases/PACIENTES.sav")
View(PACIENTES)
n_distinct(PACIENTES$PAC_DIRECCION)


# Pacientes deben ser todos los del FNR? 
# Ingresos deben ser todos los crónicos?
# Sesiones deben ser todos los que se hicieron diálisis