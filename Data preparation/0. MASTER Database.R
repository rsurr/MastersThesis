library(readr)
library(mlogit)
library(dplyr)

source("1. Database INGRESOS_HD.R")
source("2. Database GEO.R")

INGRESOS_HD2 <- read_csv("INGRESOS_HD2.csv")
GEO <- read_csv("GEO.csv") %>% 
  select(CAPACNUM, starts_with("dist"))
IMAE_num <- read_csv("IMAE_num.csv")

DATA <- left_join(INGRESOS_HD2, GEO, by=c("CAPACNUM")) %>% 
  left_join(IMAE_num, by="ZCAIMAE")

DATA1 <- left_join(INGRESOS_HD2, GEO, by=c("CAPACNUM")) %>% 
  left_join(IMAE_num, by="ZCAIMAE") %>% 
  filter(tiene_imae==1)

DATA0 <- left_join(INGRESOS_HD2, GEO, by=c("CAPACNUM")) %>% 
  left_join(IMAE_num, by="ZCAIMAE") %>% 
  filter(tiene_imae==0)
  
mlogit1 <- dfidx(DATA1, idx="CAPACNUM", choice="choice", 
              varying=grep('^dist|^medimae|^inst', names(DATA)), sep="")

mlogit0 <- dfidx(DATA0, idx="CAPACNUM", choice="choice", 
                 varying=grep('^dist|^medimae|^inst', names(DATA)), sep="")

#mlogit <- mlogit.data(DATA,
#                      choice = 'choice',
#                      shape = 'wide',
#                      id.var = "CAPACNUM",
#                      varying = 12:93,
#                      sep="")

library(peakRAM)
peakRAM({
  model1 <- mlogit(choice ~ dist + medimae + inst | 
                     CASEDADA + CASEXO + ZCASINST +
                     ZCASDEPAR + ZB1SRAZA + ZB1SOCUP0 + B1SNIVEL, 
                   data = mlogit1)
})

peakRAM({
  model0 <- mlogit(choice ~ dist + medimae + inst | 
                     CASEDADA + CASEXO + ZCASINST +
                     ZCASDEPAR + ZB1SRAZA + ZB1SOCUP0 + B1SNIVEL, 
                   data = mlogit0)
})

model1 <- mlogit(choice ~ dist + medimae + inst | 
                  CASEDADA + CASEXO + ZCASINST +
                  ZCASDEPAR + ZB1SRAZA + ZB1SOCUP0 + B1SNIVEL, 
                data = mlogit1)

model0 <- mlogit(choice ~ dist + medimae + inst | 
                   CASEDADA + CASEXO + ZCASINST +
                   ZCASDEPAR + ZB1SRAZA + ZB1SOCUP0 + B1SNIVEL, 
                 data = mlogit0)

model <- mlogit(choice ~ dist, 
               data = mlogit)

summary(model)

