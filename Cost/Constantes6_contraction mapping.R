# Load necessary libraries
library(readr)
library(nleqslv)
library(ggplot2)

# Read constants from CSV file
constantes_nobs <- read_csv("~/Proyecto Tesis/MastersThesis/Cost/constantes n_obs.csv") %>% 
  filter(ZCAIMAE != "SARI")

constantes <- na.omit(constantes_nobs)

# Initial MC variable definitions
dSdQ <- constantes$dSdQ
P <- constantes$P[1]
b_URR <- constantes$b_URR[1]
b_a0 <- constantes$b_a0[1]
b_a2 <- constantes$b_a2[1]
b_a3 <- constantes$b_a3[1]
URR <- unlist(constantes$URR)
indep <- unlist(constantes$indep)
b_constant <- unlist(constantes$b_constant) 
b_n_obs <- unlist(constantes$b_n_obs) 
n_obs <- unlist(constantes$n_obs)
n_obs_total <- constantes$n_obs_total[1]

# Function to calculate variableMC
variableMC <- function(x) {
  y <- numeric(length(x))
  for (i in 1:length(x)) {
    exp_terms <- exp(b_constant + b_URR * x + b_n_obs * n_obs)
    denominator <- sum(exp_terms)
    y[i] <- (P - b_a0 - b_a2 * x[i] - b_a3 * indep[i]) * dSdQ[i] - b_a2 * exp_terms[i] / (1 + denominator)
  }
  return(y)
}

max_distances <- numeric()


# Loop for updating n_obs
for (iteration in 1:1000) { # Change the number of iterations as needed
  
  # Initialize starting values
  xstart <- URR
  fstart <- variableMC(xstart)
  
  # Print initial values and function output for debugging
  print(paste("Iteration:", iteration))
  print(xstart)
  print(fstart)
  
  # Solve the system of nonlinear equations
  solution <- nleqslv(xstart, variableMC, control=list(btol=0.01))
  
  # Print the solution
  print(solution)
  
  # Calculate exp_terms[i] / (1 + denominator) using solution values
  solution_values <- solution$x
  exp_terms <- exp(b_constant + b_URR * solution_values + b_n_obs * n_obs)
  denominator <- sum(exp_terms)
  result_terms <- exp_terms / (1 + denominator)
  new_n_obs <- result_terms * n_obs_total
  
  # Print the computed terms
  print(result_terms)
  
  max_distance <- max(abs(new_n_obs - n_obs))
  max_distances <- c(max_distances, max_distance)
  
  # Update n_obs and URR for the next iteration
  n_obs <- new_n_obs
  URR <- solution_values
  
}

max_distances

  # Optional: Generate and save plots for each iteration
  data <- cbind(constantes, solution_values, result_terms)
  
  ggplot(data, aes(x = s_obs2, y = result_terms)) +
    geom_point() +
    geom_smooth(method = "lm", se = F) +
    labs(x = "Actual share", y = "Predicted share") +
    theme_minimal() 
    ggsave(paste0("predicted_share_iteration_", iteration, ".png"))
  
  ggplot(data, aes(x = URR, y = solution_values)) +
    geom_point() +
    geom_smooth(method = "lm", se = F) +
    labs(x = "Actual quality", y = "Predicted quality") +
    theme_minimal() 
    ggsave(paste0("predicted_quality_iteration_", iteration, ".png"))

