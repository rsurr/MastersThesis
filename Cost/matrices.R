library(readr)
library(tidyverse)
library(fastDummies)
library(matlib)

matrices <- read_csv("~/Proyecto Tesis/MastersThesis/Cost/matrices.csv") %>%
  filter(!is.na(ID_CAIMAE), ZCAIMAE!="SARI") %>% 
  filter(anio_solicitud==2005) %>% 
  arrange(ID_CAIMAE) %>% 
  #arrange(anio_solicitud) %>% 
  #group_by(ID_CAIMAE) %>%
  #summarise(ZCAIMAE=first(ZCAIMAE),
  #          ID_CAIMAE=first(ID_CAIMAE),
  #          chain=first(chain),
  #          s_obs2=mean(s_obs2, na.rm = TRUE),
  #          P=mean(P, na.rm = TRUE),
  #          across(starts_with("dSdQ"), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(chain = case_when(chain %in% c("PRIVADO", "NEPHROS", "SARI", "PUBLICO") 
                           ~ as.character(ID_CAIMAE),
                           .default=chain)) %>% 
  dummy_cols(select_columns = "chain") %>% 
  mutate(
    dSdQ_67=dSdQ_67*chain_CENEU,
    dSdQ_62=dSdQ_62*chain_CENEU,
    dSdQ_65=dSdQ_65*chain_CENEU,
    dSdQ_87=dSdQ_87*chain_CENEU,
    
    dSdQ_64=dSdQ_64*chain_DIAVERUM,
    dSdQ_60=dSdQ_60*chain_DIAVERUM,
    
    #Privados
    dSdQ_2=dSdQ_2*chain_2,
    dSdQ_88=dSdQ_88*chain_88,
    dSdQ_70=dSdQ_70*chain_70,
    dSdQ_69=dSdQ_69*chain_69,
    dSdQ_66=dSdQ_66*chain_66,
    dSdQ_57=dSdQ_57*chain_57,
    dSdQ_61=dSdQ_61*chain_61,
    dSdQ_95=dSdQ_95*chain_95,
    
    #Nephros
    dSdQ_86=dSdQ_86*chain_86,
    
    #Sari
    #dSdQ_68=dSdQ_68*chain_68,
    
    #Publicos
    dSdQ_72=dSdQ_72*chain_72,
    dSdQ_40=dSdQ_40*chain_40,
  )

# Números para mutate
matrices %>% filter(chain=="CENEU") %>% select(ID_CAIMAE) %>% distinct()
matrices %>% filter(chain=="DIAVERUM") %>% select(ID_CAIMAE) %>% distinct()
matrices %>% filter(chain=="PRIVADO") %>% select(ID_CAIMAE) %>% distinct()
matrices %>% filter(chain=="NEPHROS") %>% select(ID_CAIMAE) %>% distinct()
matrices %>% filter(chain=="SARI") %>% select(ID_CAIMAE) %>% distinct()
matrices %>% filter(chain=="PUBLICO") %>% select(ID_CAIMAE) %>% distinct()

# Omega
omega <- matrices %>% 
  select(starts_with("dSdQ"), -dSdQ_68) %>% 
  as.matrix()

aux <- !apply(omega, 1, function(x) all(is.na(x)))
# Remove rows where all elements are NA
omega <- omega[aux, ]
# Remove columns where all elements are NA
omega <- omega[, aux]

omega_1 <- inv(omega)

# Quality
Q <- matrices %>% select("URR") %>% 
  as.matrix()
Q <- Q[aux]

# S
S <- as.matrix(matrices$s_obs2)
S <- S[aux]

# Theta
diag <- diag(1, 15)

# P
P <- t(matrices$P)
P <- P[aux]

# Z
Z <- matrices %>% select("p_diab") %>% 
  as.matrix()
Z <- Z[aux]

x <- cbind(Q, P, S, omega_1, Z)

g1 <- function(tet, x)
{
  Q <- x[,1]
  P <- x[,2]
  S <- x[,3]
  omega_1 <- x[,4:18]
  Z <- x[,19]
  f <- Z*(P - (tet[1] + tet[2]*Q) - omega_1 %*% (tet[2] * diag(1, 15) %*% S))
  return(f)
}

print(res <- gmm(g1,x, c(3000,2000)))

summary(res)

P - matrix(1, nrow=15) - omega_1 %*% (diag(1, 15)  %*% S)

a <- matrix(1, nrow=15) 
b <- diag(1, 15)

a * b
  
  mutate(dSdQ_2=case_when(num_choice==2 ~ dSdQ_2,
                          .default=0),
         dSdQ_88=case_when(num_choice==88 ~ dSdQ_88,
                           .default=0),
         dSdQ_70=case_when(num_choice==70 ~ dSdQ_70,
                           .default=0),
         dSdQ_69=case_when(num_choice==69 ~ dSdQ_69,
                           .default=0),
         dSdQ_88=case_when(num_choice==88 ~ dSdQ_88,
                           .default=0),
         dSdQ_88=case_when(num_choice==88 ~ dSdQ_88,
                           .default=0),
         dSdQ_88=case_when(num_choice==88 ~ dSdQ_88,
                           .default=0),
         dSdQ_88=case_when(num_choice==88 ~ dSdQ_88,
                           .default=0),
         )
  
