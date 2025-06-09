
# Climate plot

# Load climate data
climate_data <- fread("mortlake_weather.csv") %>%
  mutate(
    month = factor(month, levels = c(
      "January", "February", "March", "April", "May", "June", "July", "August",
      "September", "October", "November", "December"
    )),
    month = factor(month.abb[as.numeric(month)], levels = month.abb)
  )
# Create climate plot
climate_plot <- ggplot(climate_data, aes(x = month)) +
  geom_bar(aes(y = ave_rain, color = "Precipitation"), stat = "identity", fill = "#51A3CC") +
  geom_line(aes(y = mean_max, color = "Max temperature"), group = 1, size = 1) +
  geom_line(aes(y = mean_min, color = "Min temperature"), group = 1, size = 1) +
  scale_color_manual(
    values = c(
      "Precipitation" = "#51A3CC",
      "Max temperature" = "#D82632",
      "Min temperature" = "gold"
    ),
    breaks = c("Precipitation", "Max temperature", "Min temperature")
  ) +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "Mean temperature (°C)")) +
  labs(
    y = "Mean precipitation (mm)",
    x = "Month"
  ) +
  theme_minimal() +
  theme(
    panel.border = element_rect(colour = "grey90", linewidth = 0.5, fill = NA),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank(),
    aspect.ratio = 1
  )

# Save climate plot
ggsave(
  "climate_plot.jpeg",
  plot = climate_plot,
  width = 12,
  height = 8,
  units = "cm",
  dpi = 300
)
ggsave(
  "climate_plot.tiff",
  plot = climate_plot,
  width = 12,
  height = 8,
  units = "cm",
  dpi = 300
)
