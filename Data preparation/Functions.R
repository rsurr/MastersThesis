get_all_predictions <- function(model, data) {
  # Create a data frame with all combinations of "IMAE" and "anio"
  combinations <- expand.grid(
    IMAE = levels(data$ZCAIMAE),  # Assuming ZCAIMAE is the variable name for "IMAE"
    anio = levels(data$anio)      # Assuming anio is the variable name for "anio"
  )
  
  # Convert "IMAE" and "anio" to factors
  combinations$IMAE <- as.factor(combinations$IMAE)
  combinations$anio <- as.factor(combinations$anio)
  
  # Create a data frame with mean values for other predictors
  mean_values <- data.frame(
    CASEDADA = mean(base$CASEDADA, na.rm = TRUE),
    meses = mean(base$meses, na.rm = TRUE),
    PAC_SEXO_F = mean(base$PAC_SEXO_F, na.rm = TRUE),
    ZB1SRAZA_NEGRA = mean(base$ZB1SRAZA_NEGRA, na.rm = TRUE),
    ZB1SRAZA_OTRA = mean(base$ZB1SRAZA_OTRA, na.rm = TRUE),
    DDIAB_S = mean(base$DDIAB_S, na.rm = TRUE),
    DCISQ_S = mean(base$DCISQ_S, na.rm = TRUE),
    DEVP_S = mean(base$DEVP_S, na.rm = TRUE),
    B1SNIVEL_Primaria = mean(base$B1SNIVEL_Primaria, na.rm = TRUE),
    B1SNIVEL_Secundaria = mean(base$B1SNIVEL_Secundaria, na.rm = TRUE),
    B1SNIVEL_Universidad = mean(base$B1SNIVEL_Universidad, na.rm = TRUE),
    tipo_inst_IAMCIAMPP = mean(base$tipo_inst_IAMCIAMPP, na.rm = TRUE),
    tipo_inst_SEGUROPRIVADO = mean(base$tipo_inst_SEGUROPRIVADO, na.rm = TRUE),
    tipo_inst_CORPORATIVO = mean(base$tipo_inst_CORPORATIVO, na.rm = TRUE),
    ocupado = mean(base$ocupado, na.rm = TRUE),
    ECREAV = mean(base$ECREAV, na.rm = TRUE)
    )
  
  # Initialize an empty data frame to store predictions
  all_predictions <- data.frame()
  
  # Loop through all combinations and get predictions
  for (i in 1:nrow(combinations)) {
    mean_values$ZCAIMAE <- combinations$IMAE[i]
    mean_values$anio <- combinations$anio[i]
    predictions <- predict(model, newdata = mean_values)
    
    # Check if the coefficient for ZCAIMAE:anio is NA, and set predictions to NA accordingly
    if (is.na(coef(model)[paste0("ZCAIMAE", combinations$IMAE[i], ":anio", combinations$anio[i])])) {
      predictions <- NA
    }
    
    result <- data.frame(IMAE = combinations$IMAE[i], anio = combinations$anio[i], Prediction = predictions)
    all_predictions <- rbind(all_predictions, result)
  }
  
  return(all_predictions)
}

library(dplyr)
library(tidyr)
library(stringr)

extract_coefficients <- function(model) {
  model_name <- deparse(substitute(model))
  
  coef_df <- tidy(model) %>%
    select(term, estimate) %>% 
    filter(str_detect(term, "ZCAIMAE") & str_detect(term, ":anio")) %>%
    separate(term, into = c("ZCAIMAE", "anio"), sep = ":") %>%
    mutate(anio = gsub("anio", "", anio),
           IMAE = gsub("ZCAIMAE", "", ZCAIMAE)) %>% 
    rename(!!model_name := estimate) %>% 
    select(anio, !!model_name, IMAE)
  
  return(coef_df)
}

# Example usage:
# Assuming 'fe_ktv' is your model object
coefficients_df <- extract_coefficients(fe_ktv)
