library(readr)
library(tidyverse)
library(fastDummies)
library(matlib)
library(gmm)

matrices <- read_csv("~/Proyecto Tesis/MastersThesis/Cost/matrices_t.csv") %>%
  filter(!is.na(ID_CAIMAE)) %>% 
  arrange(ID_CAIMAE)

# Multiproduct
multiproduct <- matrices %>% 
  select(ID_CAIMAE, anio_solicitud, chain) %>% 
  arrange(ID_CAIMAE) %>% 
  group_by(ID_CAIMAE) %>% 
  mutate(one = 1) %>% 
  ungroup() %>% 
  mutate(variable = paste(ID_CAIMAE, anio_solicitud, sep = "_")) %>% 
  pivot_wider(names_from = variable, values_from = one) %>% 
  replace(is.na(.), 0) 

multiproduct %>% 
  filter(chain == "CENEU") %>% 
  pull(ID_CAIMAE) %>% 
  unique()

multiproduct %>% 
  filter(chain == "DIAVERUM") %>% 
  pull(ID_CAIMAE) %>% 
  unique()

multiproduct <- multiproduct %>%
  
  # CENEU (antes de 2013)
  mutate(across(matches(paste0("^", c(62, 65, 87), "_")), 
                ~ if_else(ID_CAIMAE %in% c(62, 65, 87) & anio_solicitud<2013, 1, .))) %>% 
  
  # CENEU (CE.DI.SA (67) se suma en 2013)
  mutate(across(matches(paste0("^", c(62, 65, 67, 87), "_")), 
                ~ if_else(ID_CAIMAE %in% c(62, 65, 67, 87) & anio_solicitud>=2013, 1, .))) %>% 
  
  # CENEU (ASOCIACIÓN ESPAÑOLA (2) se suma en 2015)
  mutate(across(matches(paste0("^", c(2, 62, 65, 67, 87), "_")), 
                ~ if_else(ID_CAIMAE %in% c(2, 62, 65, 67, 87) & anio_solicitud>=2015, 1, .))) %>% 
  
  # DIAVERUM (RENIS (60) se suma en 2009)
  mutate(across(matches(paste0("^", c(60, 64), "_")), 
                ~ if_else(ID_CAIMAE %in% c(60, 64) & anio_solicitud>=2009, 1, .))) %>%
  
  select(-ID_CAIMAE, -anio_solicitud, -chain) %>% 
  
  as.matrix()

# Omega
omega <- matrices %>% 
  replace(is.na(.), 0) %>% 
  select(starts_with("dSdQ")) %>% 
  as.matrix()

aux <- !apply(omega, 1, function(x) all(x==0))
# Remove rows where all elements are NA
omega <- omega[aux, ]
# Remove columns where all elements are NA
omega <- omega[, aux]

omega_1 <- inv(omega)

# Omega_1 x multiproduct

multiproduct <- multiproduct[aux, ]
multiproduct <- multiproduct[, aux]

omega_1 <- matrixcalc::hadamard.prod(multiproduct, omega_1) 

# Quality
Q <- matrices %>% select("URR") %>% 
  as.matrix()
Q <- Q[aux]

# S
S <- as.matrix(matrices$s_obs2)
S <- S[aux]

# Theta
n_aux <- aux[aux==TRUE] %>% length()
diag <- diag(1, n_aux)

# P
P <- as.matrix(matrices$P)
P <- P[aux]

# Z
Z <- matrices %>% select("p_diab", "p_devp", "p_dcisq") %>% 
  data.matrix()
Z <- Z[aux,]

# n_centro
n_centro <- as.matrix(matrices$n_centro)
n_centro <- n_centro[aux]

x <- cbind(Z, P)
head(x)

g1 <- function(tet, x) {
  Z_1 <- x[,1]
  Z_2 <- x[,2]
  Z_3 <- x[,3]
  
  f_1 <- Z_1 * (P - tet[1] - tet[2] * Q - omega_1 %*% (tet[2] * diag(1, n_aux) %*% S))
  f_2 <- Z_2 * (P - tet[1] - tet[2] * Q - omega_1 %*% (tet[2] * diag(1, n_aux) %*% S))
  f_3 <- Z_3 * (P - tet[1] - tet[2] * Q - omega_1 %*% (tet[2] * diag(1, n_aux) %*% S))
  
  f <- cbind(f_1, 
             f_2, 
             f_3)
  
  return(f)
}

print(res <- gmm(g1,x, c(3000, 2000)))
summary(res)

g2 <- function(tet, x) {
  Z_1 <- x[,1]
  Z_2 <- x[,2]
  Z_3 <- x[,3]
  P <- x[,4]
  
  f_1 <- Z_1 * (P - tet[1] - tet[2] * Q - n_centro ^ tet[3] - 
                  omega_1 %*% (tet[2] * diag(1, n_aux) %*% S))
  f_2 <- Z_2 * (P - tet[1] - tet[2] * Q - n_centro ^ tet[3] - 
                  omega_1 %*% (tet[2] * diag(1, n_aux) %*% S))
  f_3 <- Z_3 * (P - tet[1] - tet[2] * Q - n_centro ^ tet[3] - 
                  omega_1 %*% (tet[2] * diag(1, n_aux) %*% S))
  f_4 <- P   * (P - tet[1] - tet[2] * Q - n_centro ^ tet[3] - 
                  omega_1 %*% (tet[2] * diag(1, n_aux) %*% S))
  
  f <- cbind(f_1, 
             f_2, 
             f_3,
             f_4)
  
  return(f)
}

print(res2 <- gmm(g2, x, c(3000, 2000, 1)))
summary(res2)

