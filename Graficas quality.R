# UREA
ggplot(quality, aes(x = tipo_imae, y = urea, fill = tipo_imae)) +
  geom_boxplot(alpha = 0.7, color = "black") +  # Use alpha for transparency
  labs(x = "tipo_imae",
       y = "Urea") +
  scale_fill_manual(values = c("PRIVADO" = "#66C2A5", "INDEPENDIENTE" = "#FC8D62", "PUBLICO" = "#8DA0CB")) +
  theme_minimal() +
  theme(legend.position = "none")  

# HEMO
ggplot(quality, aes(x = tipo_imae, y = hemo, fill = tipo_imae)) +
  geom_boxplot(alpha = 0.7, color = "black") +  # Use alpha for transparency
  labs(x = "tipo_imae",
       y = "Hemo") +
  scale_fill_manual(values = c("PRIVADO" = "#66C2A5", "INDEPENDIENTE" = "#FC8D62", "PUBLICO" = "#8DA0CB")) +
  theme_minimal() +
  theme(legend.position = "none")  

# MORTA
ggplot(quality, aes(x = tipo_imae, y = morta, fill = tipo_imae)) +
  geom_boxplot(alpha = 0.7, color = "black") +  # Use alpha for transparency
  labs(x = "tipo_imae",
       y = "Survival") +
  scale_fill_manual(values = c("PRIVADO" = "#66C2A5", "INDEPENDIENTE" = "#FC8D62", "PUBLICO" = "#8DA0CB")) +
  theme_minimal() +
  theme(legend.position = "none")  

# FOSF
ggplot(quality, aes(x = tipo_imae, y = fosf, fill = tipo_imae)) +
  geom_boxplot(alpha = 0.7, color = "black") +  # Use alpha for transparency
  labs(x = "tipo_imae",
       y = "Fosf") +
  scale_fill_manual(values = c("PRIVADO" = "#66C2A5", "INDEPENDIENTE" = "#FC8D62", "PUBLICO" = "#8DA0CB")) +
  theme_minimal() +
  theme(legend.position = "none")  


# COMP
ggplot(quality, aes(x = tipo_imae, y = comp, fill = tipo_imae)) +
  geom_boxplot(alpha = 0.7, color = "black") +  # Use alpha for transparency
  labs(x = "tipo_imae",
       y = "Comp") +
  scale_fill_manual(values = c("PRIVADO" = "#66C2A5", "INDEPENDIENTE" = "#FC8D62", "PUBLICO" = "#8DA0CB")) +
  theme_minimal() +
  theme(legend.position = "none")  

# SEPT
ggplot(quality, aes(x = tipo_imae, y = sept, fill = tipo_imae)) +
  geom_boxplot(alpha = 0.7, color = "black") +  # Use alpha for transparency
  labs(x = "tipo_imae",
       y = "Sept") +
  scale_fill_manual(values = c("PRIVADO" = "#66C2A5", "INDEPENDIENTE" = "#FC8D62", "PUBLICO" = "#8DA0CB")) +
  theme_minimal() +
  theme(legend.position = "none")  

# PESO
ggplot(quality, aes(x = tipo_imae, y = peso, fill = tipo_imae)) +
  geom_boxplot(alpha = 0.7, color = "black") +  # Use alpha for transparency
  labs(x = "tipo_imae",
       y = "Peso") +
  scale_fill_manual(values = c("PRIVADO" = "#66C2A5", "INDEPENDIENTE" = "#FC8D62", "PUBLICO" = "#8DA0CB")) +
  theme_minimal() +
  theme(legend.position = "none") 

# PESO
coef_ktv %>% 
filter(ZCAIMAE!="HOSPITAL BRITANICO") %>% 
ggplot(aes(x = tipo_imae2, y = estimate, fill = tipo_imae2)) +
  geom_boxplot(alpha = 0.7, color = "black") +  # Use alpha for transparency
  labs(x = "tipo_imae",
       y = "Peso") +
  theme_minimal() +
  theme(legend.position = "none") 


labs <- c("Urea", "Survival", "Phosphorus", "Hemoglobin", "Complication", "Septic infection", "Weight", "URR")
names(labs) <- c("urea", "surv", "fosf", "hemo", "comp", "sept", "peso", "URR")

quality %>%
  pivot_longer(c(urea, surv, fosf, hemo, comp, sept, peso, URR)) %>% 
  #filter(IMAE!="HOSPITAL BRITANICO") %>% 
  ggplot(aes(y=value, x=tipo_imae2, color=tipo_imae2)) +
  geom_boxplot() +
  facet_wrap(~name, scale="free_y", labeller = labeller(name=labs)) +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),  # Remove x-axis label
    axis.title.y = element_blank(),  # Remove x-axis label
    panel.background = element_rect(fill = "white"),  # Change background to white
    panel.grid.major = element_line(color = "gray"),  # Change major gridlines to gray
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    strip.background = element_blank()  # Remove background from facet titles
  )
 

