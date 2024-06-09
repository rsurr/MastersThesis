# Load necessary libraries
library(readr)
library(nleqslv)

# Read constants from CSV file
constantes <- read_csv("~/Proyecto Tesis/MastersThesis/Cost/constantes.csv") %>% 
  filter(ZCAIMAE!="SARI")

# MC variable ####
dSdQ <- constantes$dSdQ
P <- constantes$P[1]
b_URR <- constantes$b_URR[1]
b_a0 <- constantes$b_a0[1]
b_a2 <- constantes$b_a2[1]
URR <- unlist(constantes$URR)
b_constant <- unlist(constantes$b_constant) # Assuming b_constant is also in constantes

# Function to calculate variableMC
variableMC <- function(x) {
  # Check if the length of x matches the expected length
  if (length(x) != 17) {
    stop("Input vector x must have a length of 17.")
  }
  
  # Vector to store results
  y <- numeric(17)
  
  # Vectorized computation
  for (i in 1:17) {
    exp_terms <- exp(b_constant + b_URR * x)
    denominator <- sum(exp_terms)
    y[i] <- (P - b_a0 - b_a2 * x[i]) * dSdQ - b_a2 * exp_terms[i] / (1 + denominator)
  }
  
  return(y)
}

# Initialize starting values
xstart <- URR
fstart <- variableMC(xstart)

# Print initial values and function output for debugging
print(xstart)
print(fstart)

# Solve the system of nonlinear equations
solution <- nleqslv(xstart, variableMC, control=list(btol=0.01))

# Print the solution
print(solution)

# Calculate exp_terms[i] / (1 + denominator) using solution values
solution_values <- solution$x
exp_terms <- exp(b_constant + b_URR * solution_values)
denominator <- sum(exp_terms)
result_terms <- exp_terms / (1 + denominator)

# Print the computed terms
print(result_terms)
