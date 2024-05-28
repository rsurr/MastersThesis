
library(fixest)

dP <- SESIONES_HD %>%
  rename(ZCAIMAE=ZPMD_IMAE) %>% 
  mutate(DPESOSE=case_when(DPESOSE==999.00 ~ NA,
                           .default = as.numeric(DPESOSE)),
         DPESOPO=case_when(DPESOPO==999.00 ~ NA,
                           .default = as.numeric(DPESOPO)),
         dife_post=DPESOPO-DPESOSE,
         dife_pre=DPESOPR-DPESOSE,
         peso=case_when(abs(dife_post)<=0.5 ~ 1,
                        abs(dife_post)>0.5 ~ 0,
                        is.na(dife_post) ~ NA)) %>% 
  select(peso, CAPACNUM, DPESOPR, DPESOPO, ZCAIMAE, PMD_ANIO, dife_post, 
         dife_pre)

# Reorder "anio" in increasing order
dP$anio <- factor(dP$PMD_ANIO, levels = sort(unique(dP$PMD_ANIO)))

# Reorder "ZCAIMAE" alphabetically
dP$ZCAIMAE <- factor(dP$ZCAIMAE, levels = sort(unique(dP$ZCAIMAE)))

# Set reference levels for anio, ZCAIMAE and CAPACNUM
dP$anio <- relevel(dP$anio, ref = "2004")
dP$ZCAIMAE <- relevel(dP$ZCAIMAE, ref = "ASOCIACION ESPAÑOLA")
dP$CAPACNUM <- relevel(as.factor(dP$CAPACNUM), ref="344677")

dP <- dP %>% unite("ZCAIMAEanio", ZCAIMAE:PMD_ANIO, sep=":", remove = FALSE) 
m_dP1 <- feols(peso ~ DPESOPR | ZCAIMAEanio + CAPACNUM, dP)
m_dP2 <- feols(peso ~ dife_pre | ZCAIMAEanio + CAPACNUM, dP)
m_dP3 <- feols(dife_post ~ DPESOPR | ZCAIMAEanio + CAPACNUM, dP)
m_dP4 <- feols(dife_post ~ dife_pre | ZCAIMAEanio + CAPACNUM, dP)
m_dP5 <- feols(DPESOPO ~ DPESOPR | ZCAIMAEanio + CAPACNUM, dP)

delta_p1 <- fixef(m_dP1)$ZCAIMAEanio %>% as.data.frame() %>% 
  rownames_to_column("ZCAIMAEanio")
colnames(delta_p1) <- c("ZCAIMAEanio", "delta_p1")

delta_p2 <- fixef(m_dP2)$ZCAIMAEanio %>% as.data.frame() %>% 
  rownames_to_column("ZCAIMAEanio")
colnames(delta_p2) <- c("ZCAIMAEanio", "delta_p2")

delta_p3 <- fixef(m_dP3)$ZCAIMAEanio %>% as.data.frame() %>% 
  rownames_to_column("ZCAIMAEanio")
colnames(delta_p3) <- c("ZCAIMAEanio", "delta_p3")

delta_p4 <- fixef(m_dP4)$ZCAIMAEanio %>% as.data.frame() %>% 
  rownames_to_column("ZCAIMAEanio")
colnames(delta_p4) <- c("ZCAIMAEanio", "delta_p4")

delta_p5 <- fixef(m_dP5)$ZCAIMAEanio %>% as.data.frame() %>% 
  rownames_to_column("ZCAIMAEanio")
colnames(delta_p5) <- c("ZCAIMAEanio", "delta_p5")

DELTA_P <- delta_p1 %>% 
  left_join(delta_p2, join_by("ZCAIMAEanio")) %>%
  left_join(delta_p3, join_by("ZCAIMAEanio")) %>%
  left_join(delta_p4, join_by("ZCAIMAEanio")) %>%
  left_join(delta_p5, join_by("ZCAIMAEanio")) %>%
  separate(ZCAIMAEanio, sep = ":", into = c("ZCAIMAE", "anio")) %>% 
  mutate(anio_solicitud=as.double(anio))

write.csv(
  DELTA_P,
  "DELTA_P.csv", 
  row.names=FALSE)

