labs <- c("Urea", "Survival", "Phosphorus", "Hemoglobin", 
          "No complications", "Septic infection", "Weight", 
          "Urea Reduction Rate", "Kt/V")
names(labs) <- c("urea", "surv", "fosf", "hemo", "comp", "sept", "peso", "URR", "ktv")

labs_io <- c("Input", "Outcome: intermediate", "Outcome: final")
names(labs_io) <- c("input", "output_int", "output_fin")

names <- cbind(names(labs), labs)%>% as.data.frame()
colnames(names) <- c("measure", "Names") 



quality %>%
  pivot_longer(c(urea, surv, fosf, hemo, comp, sept, peso, URR, ktv)) %>% 
  ggplot(aes(y=value, x=tipo_imae2, color=tipo_imae2)) +
  geom_boxplot() +
  facet_wrap(~name, scale="free_y", labeller = labeller(name=labs)) +
  coord_cartesian(ylim = c(0, 1)) +  # Restrict y-axis from 0 to 1
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),  # Remove x-axis label
    axis.title.y = element_blank(),  # Remove x-axis label
    panel.background = element_rect(fill = "white"),  # Change background to white
    panel.grid.major = element_line(color = "gray"),  # Change major gridlines to gray
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    strip.background = element_blank()  # Remove background from facet titles
  ) +
  scale_color_viridis_d(end=0.8)
ggsave("adjusted.png", dpi = 500, scale=3)

non_adj_quality %>%
  pivot_longer(c(urea, surv, fosf, hemo, comp, sept, peso, URR, ktv)) %>% 
  ggplot(aes(y=value, x=tipo_imae2, color=tipo_imae2)) +
  geom_boxplot() +
  facet_wrap(~name, scale="free_y", labeller = labeller(name=labs)) +
  coord_cartesian(ylim = c(0, 1)) +  # Restrict y-axis from 0 to 1
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),  # Remove x-axis label
    axis.title.y = element_blank(),  # Remove x-axis label
    panel.background = element_rect(fill = "white"),  # Change background to white
    panel.grid.major = element_line(color = "gray"),  # Change major gridlines to gray
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    strip.background = element_blank()  # Remove background from facet titles
  ) +
  scale_color_viridis_d(end=0.8)
ggsave("unadjusted.png", dpi = 500, scale=3)

quality_input_outcome %>%
  pivot_longer(c(input, output_int, output_fin)) %>% 
  ggplot(aes(y=value, x=tipo_imae2, color=tipo_imae2)) +
  geom_boxplot() +
  facet_wrap(~name, scale="free_y", labeller = labeller(name=labs_io)) +
  coord_cartesian(ylim = c(0, 1)) +  # Restrict y-axis from 0 to 1
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),  # Remove x-axis label
    axis.title.y = element_blank(),  # Remove x-axis label
    panel.background = element_rect(fill = "white"),  # Change background to white
    panel.grid.major = element_line(color = "gray"),  # Change major gridlines to gray
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    strip.background = element_blank()  # Remove background from facet titles
  ) +
  scale_color_viridis_d(end=0.8)
ggsave("input_outcome.png", dpi = 500, scale=3)

mean_qual <- quality %>%
  group_by(IMAE) %>% 
  summarise(URR = mean(URR, na.rm = TRUE),
            ktv = mean(ktv, na.rm = TRUE), 
            sept = mean(sept, na.rm = TRUE), 
            tipo_imae2 = first(tipo_imae2))


mean_non_adj_qual <- non_adj_quality %>%
  group_by(IMAE) %>% 
  summarise(URR = mean(URR, na.rm = TRUE),
            ktv = mean(ktv, na.rm = TRUE), 
            sept = mean(sept, na.rm = TRUE), 
            tipo_imae2 = first(tipo_imae2))

library(ggplot2)
library(viridis)

mean_qual %>% 
  ggplot(aes(y = URR, x = reorder(IMAE, -URR), fill = tipo_imae2)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_fill_viridis_d(end = 0.8, name = "Provider type") +  # Adding legend title
  theme(
    legend.position = "bottom",
    axis.title.x = element_blank(),  # Remove x-axis label
    axis.title.y = element_blank(),  # Remove x-axis label
    panel.background = element_rect(fill = "white"),  # Change background to white
    panel.grid.major = element_line(color = "gray"),  # Change major gridlines to gray
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    strip.background = element_blank()  # Remove background from facet titles
  ) +
  scale_x_discrete(labels = function(x) substr(x, nchar(x)-7, nchar(x))) +
  coord_cartesian(ylim = c(0, 1))
ggsave("mean_adjusted.png", dpi = 500, scale=3)


mean_non_adj_qual %>% 
  ggplot(aes(y = URR, x = reorder(IMAE, -URR), fill = tipo_imae2)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_fill_viridis_d(end = 0.8, name = "Provider type") +  # Adding legend title
  theme(
    legend.position = "bottom",
    axis.title.x = element_blank(),  # Remove x-axis label
    axis.title.y = element_blank(),  # Remove x-axis label
    panel.background = element_rect(fill = "white"),  # Change background to white
    panel.grid.major = element_line(color = "gray"),  # Change major gridlines to gray
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    strip.background = element_blank()  # Remove background from facet titles
  ) +
  scale_x_discrete(labels = function(x) substr(x, nchar(x)-7, nchar(x))) +
  coord_cartesian(ylim = c(0, 1))
ggsave("mean_unadjusted.png", dpi = 500, scale=3)

mean_non_adj_qual <- mean_non_adj_qual %>% mutate(type="Unadjusted")
mean_qual <- mean_qual %>% mutate(type="Risk-adjusted")

mean <- rbind(mean_qual, mean_non_adj_qual)


mean_qual %>% 
  ggplot(aes(y = ktv, x = reorder(IMAE, -ktv), fill = tipo_imae2)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

mean_qual %>% 
  ggplot(aes(y = sept, x = reorder(IMAE, -sept), fill = tipo_imae2)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

quality %>%
  pivot_longer(c(urea, surv, fosf, hemo, comp, sept, peso, URR, ktv)) %>% 
  ggplot(aes(y = value, x = reorder(IMAE, -value), fill = tipo_imae2)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

