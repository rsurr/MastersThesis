library(fastDummies)
library(readr)

source("1. Database INGRESOS_HD.R")
source("2. Database GEO.R")

INGRESOS_HD <- read_csv("INGRESOS_HD2.csv")
GEO <- read_csv("GEO.csv")
IMAE_num <- read_csv("IMAE_num.csv")

DATA <- left_join(INGRESOS_HD, GEO, by=c("CAPACNUM")) %>% 
  left_join(IMAE_num, by="ZCAIMAE")

#%>% 
  dummy_cols(select_columns = "ZCAIMAE")

logit <- INGRESOS_psd %>% select(CAPACNUM, CASEDADA, CASEXO, ZCAIMAE)

library(mlogit)
