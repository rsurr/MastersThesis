aborrar <- mlogitdta %>% group_by(ID) %>% summarize(choice=mean(choice))
b <- mlogitdta %>% filter(chain=="DIAVERUM")
table(base$B1SNIVEL)


# Descompensacion uremica

table(INGRESOS_HD$SCDESU)
table(c(INGRESOS_HD$SCINGC))
INGRESOS_HD$SCINGOB
