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
