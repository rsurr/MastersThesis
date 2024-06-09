library(nleqslv)
library(readr)

#### CON SARI ####

constantes <- read_csv("~/Proyecto Tesis/MastersThesis/Cost/constantes.csv")

# MC variable ####

dSdQ <- constantes$dSdQ
P <- constantes$P[1]
b_URR <- constantes$b_URR[1]
b_a0 <- constantes$b_a0[1]
b_a2 <- constantes$b_a2[1]

URR <- unlist(constantes$URR)


variableMC <- function(x) {
  y <- numeric(18)
  
  y[1] <- (P-b_a0-b_a2*x[1])*dSdQ - b_a2*exp(b_constant[1]+b_URR*x[1])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17])+exp(b_constant[18]+b_URR*x[18]))
  
  y[2] <- (P-b_a0-b_a2*x[2])*dSdQ - b_a2*exp(b_constant[2]+b_URR*x[2])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17])+exp(b_constant[18]+b_URR*x[18]))
  
  y[3] <- (P-b_a0-b_a2*x[3])*dSdQ - b_a2*exp(b_constant[3]+b_URR*x[3])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17])+exp(b_constant[18]+b_URR*x[18]))
  
  y[4] <- (P-b_a0-b_a2*x[4])*dSdQ - b_a2*exp(b_constant[4]+b_URR*x[4])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17])+exp(b_constant[18]+b_URR*x[18]))
  
  y[5] <- (P-b_a0-b_a2*x[5])*dSdQ - b_a2*exp(b_constant[5]+b_URR*x[5])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17])+exp(b_constant[18]+b_URR*x[18]))
  
  y[6] <- (P-b_a0-b_a2*x[6])*dSdQ - b_a2*exp(b_constant[6]+b_URR*x[6])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17])+exp(b_constant[18]+b_URR*x[18]))
  
  y[7] <- (P-b_a0-b_a2*x[7])*dSdQ - b_a2*exp(b_constant[7]+b_URR*x[7])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17])+exp(b_constant[18]+b_URR*x[18]))
  
  y[8] <- (P-b_a0-b_a2*x[8])*dSdQ - b_a2*exp(b_constant[8]+b_URR*x[8])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17])+exp(b_constant[18]+b_URR*x[18]))
  
  y[9] <- (P-b_a0-b_a2*x[9])*dSdQ - b_a2*exp(b_constant[9]+b_URR*x[9])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17])+exp(b_constant[18]+b_URR*x[18]))
  
  y[10] <- (P-b_a0-b_a2*x[10])*dSdQ - b_a2*exp(b_constant[10]+b_URR*x[10])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17])+exp(b_constant[18]+b_URR*x[18]))
  
  y[11] <- (P-b_a0-b_a2*x[11])*dSdQ - b_a2*exp(b_constant[11]+b_URR*x[11])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17])+exp(b_constant[18]+b_URR*x[18]))
  
  y[12] <- (P-b_a0-b_a2*x[12])*dSdQ - b_a2*exp(b_constant[12]+b_URR*x[12])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17])+exp(b_constant[18]+b_URR*x[18]))
  
  y[13] <- (P-b_a0-b_a2*x[13])*dSdQ - b_a2*exp(b_constant[13]+b_URR*x[13])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17])+exp(b_constant[18]+b_URR*x[18]))
  
  y[14] <- (P-b_a0-b_a2*x[14])*dSdQ - b_a2*exp(b_constant[14]+b_URR*x[14])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17])+exp(b_constant[18]+b_URR*x[18]))
  
  y[15] <- (P-b_a0-b_a2*x[15])*dSdQ - b_a2*exp(b_constant[15]+b_URR*x[15])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17])+exp(b_constant[18]+b_URR*x[18]))
  
  y[16] <- (P-b_a0-b_a2*x[16])*dSdQ - b_a2*exp(b_constant[16]+b_URR*x[16])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17])+exp(b_constant[18]+b_URR*x[18]))
  
  y[17] <- (P-b_a0-b_a2*x[17])*dSdQ - b_a2*exp(b_constant[17]+b_URR*x[17])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17])+exp(b_constant[18]+b_URR*x[18]))
  
  y[18] <- (P-b_a0-b_a2*x[18])*dSdQ - b_a2*exp(b_constant[18]+b_URR*x[18])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17])+exp(b_constant[18]+b_URR*x[18]))
  
  
  y
}

xstart <- URR
fstart <- variableMC(xstart)
xstart
fstart

nleqslv(xstart, variableMC, control=list(btol=.01))

#### SIN SARI ####

constantes <- read_csv("~/Proyecto Tesis/MastersThesis/Cost/constantes.csv") %>% 
  filter(ZCAIMAE!="SARI")

# MC variable ####

dSdQ <- constantes$dSdQ
P <- constantes$P[1]
b_URR <- constantes$b_URR[1]
b_a0 <- constantes$b_a0[1]
b_a2 <- constantes$b_a2[1]

URR <- unlist(constantes$URR)


variableMC <- function(x) {
  y <- numeric(17)
  
  y[1] <- (P-b_a0-b_a2*x[1])*dSdQ - b_a2*exp(b_constant[1]+b_URR*x[1])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17]))
  
  y[2] <- (P-b_a0-b_a2*x[2])*dSdQ - b_a2*exp(b_constant[2]+b_URR*x[2])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17]))
  
  y[3] <- (P-b_a0-b_a2*x[3])*dSdQ - b_a2*exp(b_constant[3]+b_URR*x[3])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17]))
  
  y[4] <- (P-b_a0-b_a2*x[4])*dSdQ - b_a2*exp(b_constant[4]+b_URR*x[4])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17]))
  
  y[5] <- (P-b_a0-b_a2*x[5])*dSdQ - b_a2*exp(b_constant[5]+b_URR*x[5])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17]))
  
  y[6] <- (P-b_a0-b_a2*x[6])*dSdQ - b_a2*exp(b_constant[6]+b_URR*x[6])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17]))
  
  y[7] <- (P-b_a0-b_a2*x[7])*dSdQ - b_a2*exp(b_constant[7]+b_URR*x[7])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17]))
  
  y[8] <- (P-b_a0-b_a2*x[8])*dSdQ - b_a2*exp(b_constant[8]+b_URR*x[8])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17]))
  
  y[9] <- (P-b_a0-b_a2*x[9])*dSdQ - b_a2*exp(b_constant[9]+b_URR*x[9])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17]))
  
  y[10] <- (P-b_a0-b_a2*x[10])*dSdQ - b_a2*exp(b_constant[10]+b_URR*x[10])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17]))
  
  y[11] <- (P-b_a0-b_a2*x[11])*dSdQ - b_a2*exp(b_constant[11]+b_URR*x[11])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17]))
  
  y[12] <- (P-b_a0-b_a2*x[12])*dSdQ - b_a2*exp(b_constant[12]+b_URR*x[12])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17]))
  
  y[13] <- (P-b_a0-b_a2*x[13])*dSdQ - b_a2*exp(b_constant[13]+b_URR*x[13])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17]))
  
  y[14] <- (P-b_a0-b_a2*x[14])*dSdQ - b_a2*exp(b_constant[14]+b_URR*x[14])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17]))
  
  y[15] <- (P-b_a0-b_a2*x[15])*dSdQ - b_a2*exp(b_constant[15]+b_URR*x[15])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17]))
  
  y[16] <- (P-b_a0-b_a2*x[16])*dSdQ - b_a2*exp(b_constant[16]+b_URR*x[16])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17]))
  
  y[17] <- (P-b_a0-b_a2*x[17])*dSdQ - b_a2*exp(b_constant[17]+b_URR*x[17])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17]))
  
  
  y
}

xstart <- URR
fstart <- variableMC(xstart)
xstart
fstart

nleqslv(xstart, variableMC, control=list(btol=.01))


# MC Constante ####

a_constant <- constantes$a_constant
b_constant <- constantes$b_constant
b_URR <- constantes$b_URR[1]
b_a2 <- constantes$b_a2[1]
URR <- unlist(constantes$URR)


constantMC <- function(x) {
  y <- numeric(17)

  
  y[1] <- a_constant[1] - b_a2*exp(b_constant[1]+b_URR*x[1])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17]))
  
  y[2] <- a_constant[2] - b_a2*exp(b_constant[2]+b_URR*x[2])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17]))
  
  y[3] <- a_constant[3] - b_a2*exp(b_constant[3]+b_URR*x[3])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17]))
  
  y[4] <- a_constant[4] - b_a2*exp(b_constant[4]+b_URR*x[4])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17]))
  
  y[5] <- a_constant[5] - b_a2*exp(b_constant[5]+b_URR*x[5])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17]))
  
  y[6] <- a_constant[6] - b_a2*exp(b_constant[6]+b_URR*x[6])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17]))
  
  y[7] <- a_constant[7] - b_a2*exp(b_constant[7]+b_URR*x[7])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17]))
  
  y[8] <- a_constant[8] - b_a2*exp(b_constant[8]+b_URR*x[8])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17]))
  
  y[9] <- a_constant[9] - b_a2*exp(b_constant[9]+b_URR*x[9])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17]))
  
  y[10] <- a_constant[10] - b_a2*exp(b_constant[10]+b_URR*x[10])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17]))
  
  y[11] <- a_constant[11] - b_a2*exp(b_constant[11]+b_URR*x[11])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17]))
  
  y[12] <- a_constant[12] - b_a2*exp(b_constant[12]+b_URR*x[12])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17]))
  
  y[13] <- a_constant[13] - b_a2*exp(b_constant[13]+b_URR*x[13])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17]))
  
  y[14] <- a_constant[14] - b_a2*exp(b_constant[14]+b_URR*x[14])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17]))
  
  y[15] <- a_constant[15] - b_a2*exp(b_constant[15]+b_URR*x[15])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17]))
  
  y[16] <- a_constant[16] - b_a2*exp(b_constant[16]+b_URR*x[16])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17]))
  
  y[17] <- a_constant[17] - b_a2*exp(b_constant[17]+b_URR*x[17])/
    (1+exp(b_constant[1]+b_URR*x[1])+exp(b_constant[2]+b_URR*x[2])+
       exp(b_constant[3]+b_URR*x[3])+exp(b_constant[4]+b_URR*x[4])+
       exp(b_constant[5]+b_URR*x[5])+exp(b_constant[6]+b_URR*x[6])+
       exp(b_constant[7]+b_URR*x[7])+exp(b_constant[8]+b_URR*x[8])+
       exp(b_constant[9]+b_URR*x[9])+exp(b_constant[10]+b_URR*x[10])+
       exp(b_constant[11]+b_URR*x[11])+exp(b_constant[12]+b_URR*x[12])+
       exp(b_constant[13]+b_URR*x[13])+exp(b_constant[14]+b_URR*x[14])+
       exp(b_constant[15]+b_URR*x[15])+exp(b_constant[16]+b_URR*x[16])+
       exp(b_constant[17]+b_URR*x[17]))
  
  y
}

xstart <- URR
fstart <- constantMC(xstart)
xstart
fstart

nleqslv(xstart, constantMC, control=list(btol=.01))

# Otro ####

nleqslv(xstart, constantMC, control=list(trace=1,btol=.01,delta="cauchy"))


# gen equation = a_constant - b_a1*(exp(b_constant+URR*b_URR)/
                                     
 y[1] <- 196.60757 - 2861.064*exp(7.466598 + x[1]*2.940758)/(1+exp(7.466598 + x[1]*2.940758)+exp(8.847630 + x[2]*2.940758))
 y[2] <- 437.23553 - 2861.064*exp(8.847630 + x[2]*2.940758)/(1+exp(7.466598 + x[1]*2.940758)+exp(8.847630 + x[2]*2.940758))
 
# 196.60757 - 2861.064*exp(7.466598 + 0.8491556*2.940758)/(1+exp(7.466598 + 0.8491556*2.940758)+exp(8.847630 + 0.7229833*2.940758))
# 437.23553 - 2861.064*exp(8.847630 + x[2]*2.940758)/(1+exp(7.466598 + x[1]*2.940758)+exp(8.847630 + x[2]*2.940758))

# a_constant - b_a1*(exp(b_constant+URR*b_URR)/(1+))

dslnex <- function(x) {
  y <- numeric(2)
  y[1] <- 196.60757 - 2861.064*exp(7.466598 + x[1]*2.940758)/(1+exp(7.466598 + x[1]*2.940758)+exp(8.847630 + x[2]*2.940758))
  y[2] <- 437.23553 - 2861.064*exp(8.847630 + x[2]*2.940758)/(1+exp(7.466598 + x[1]*2.940758)+exp(8.847630 + x[2]*2.940758))
  y
}

xstart <- c(0.8491556,0.7229833)
fstart <- dslnex(xstart)
xstart
fstart

nleqslv(xstart, dslnex, control=list(btol=.01))




dslnex <- function(x) {
  y <- numeric(2)
  y[1] <- x[1]^2 + x[2]^2 - 2
  y[2] <- exp(x[1]-1) + x[2]^3 - 2
  y
}

xstart <- c(2,0.5)
fstart <- dslnex(xstart)
xstart
fstart

nleqslv(xstart, dslnex, control=list(btol=.01))


