
# Required packages
library(cowplot)
library(gridExtra)
library(grid)
library(gtable)
library(patchwork)
library(tidyverse)

# Read data
data <- data.table::fread("data_bubble_plot.txt") %>%
  mutate(
    status = factor(status, levels = c("Native", "Exotic")),
    Month = factor(
      Month, 
      levels = c("October", "November", "December", "January")
    ),
    # Reorder 'months_flowered' for consistent groupings
    months_flowered = factor(
      months_flowered,
      levels = c(
        "October" ,"October,November","October,November,December", 
        "October,November,December,January",  "October,December", 
        "October,January", "October,December,January" , "November", 
        "November,December", "November,December,January" , 
        "December" , "December,January" , "January"
      ))
  ) %>%
  # Re-create the species ordering that was lost when saving/reading CSV
  arrange(months_flowered, Species) %>%
  mutate(
    # Reverse the order of 'Species' for correct plotting order
    Species = factor(Species, levels = rev(unique(Species)))
  )

# Filter for native species and values not equal to 0
native_data <- subset(data, status == "Native" & value != 0)

# Create a data frame for species with 0 values for all months
species_with_zeros <- data %>%
  filter(status == "Native") %>%
  group_by(Species) %>%
  mutate(total_value = sum(value)) %>%
  filter(total_value == 0)

# Bubble Plot for Native Species
bubble_native <- ggplot(native_data, aes(x = Month, y = Species, size = value, fill = life_form)) +
  geom_point(
    data = species_with_zeros,
    aes(x = Month, y = Species, size = value, fill = life_form),
    color = "white", shape = 21, alpha = 0
  ) +
  geom_point(alpha = 0.9, shape = 21, color = "black") +
  scale_size_continuous(range = c(1, 6), breaks = c(1, 20, 40, 60, 80)) +                       
  scale_fill_manual(
    values = c(
      Chamaephyte = "#1b9e77", Geophyte = "#fc8d62", Hemicryptophyte = "#8da0cb",
      Phanerophyte = "#e7298a", Therophyte = "#a6d854")
  ) +
  labs(
    title = "Native species", 
    fill = "Life form", 
    size = "Number of flowering observations"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 40, hjust = 1, vjust = 0.9),
    axis.text.y = element_text(size = 7, face = "italic"),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_rect(colour = "grey", fill = NA, size = 0.5),
    plot.title = element_text(hjust = 0.5, size = 11),
    plot.margin = margin(5, 5, 5, 5),
    legend.position = c(0, 0),
    legend.justification = c(0.375, -0.125),
    legend.direction = "vertical"
  ) +
  guides(
    fill = guide_legend(override.aes = list(size = 6)),
    size = guide_legend(keyheight = 1, keywidth = 1)
  )

print(bubble_native)

# Filter for exotic species and values not equal to 0
non_native_data <- subset(data, status == "Exotic" & value != 0)

# Create a data frame for species with 0 values for all months
species_with_zeros_exotic <- data %>%
  filter(status == "Exotic") %>%
  group_by(Species) %>%
  mutate(total_value = sum(value)) %>%
  filter(total_value == 0)

# Bubble Plot for Exotic Species
bubble_exotic <- ggplot(non_native_data, aes(x = Month, y = Species, size = value, fill = life_form)) +
  geom_point(
    data = species_with_zeros_exotic,
    aes(x = Month, y = Species, size = value, fill = life_form),
    color = "white", shape = 21, alpha = 0
  ) +
  geom_point(alpha = 0.9, shape = 21, color = "black") +
  scale_size_continuous(range = c(1, 6), breaks = c(1, 20, 40, 60, 80)) +
  scale_fill_manual(
    values = c(
      Chamaephyte = "#1b9e77", Geophyte = "#fc8d62",Hemicryptophyte = "#8da0cb",
      Phanerophyte = "#e7298a", Therophyte = "#a6d854")
  ) +
  labs(
    title = "Exotic species"
  ) +
  theme_bw() +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 40, hjust = 1, vjust = 0.9),
    axis.text.y = element_text(size = 7, face = "italic"),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_rect(colour = "grey", fill = NA, size = 0.5),
    plot.title = element_text(hjust = 0.5, size = 11),
    plot.margin = margin(5, 5, 0, 0),
    legend.position = "none"
  )

print(bubble_exotic)

# Get the ggplot grob
g <- ggplotGrob(
  bubble_native
)

# Extract the legend
legends <- gtable::gtable_filter(g, "guide-box")

# Display all legend components
grid.newpage()
gridExtra::grid.arrange(legends)

# Arrange exotic species plot and legend
exotic_legend <- cowplot::plot_grid(
  bubble_exotic, legends,
  ncol = 1,
  heights = c(2, 1)
)

# Arrange plots and add legend below
exotic_legend <- cowplot::plot_grid(
  bubble_exotic, legends,
  ncol = 1
)

# Display the exotic plot
grid.newpage()
grid::grid.draw(exotic_legend)

# Remove the legend from the native plot
bubble_native_no_legend <- bubble_native + theme(legend.position = "none")

# Combine all the plots
combined_plots <- gridExtra::arrangeGrob(
  grobs = list(bubble_native_no_legend, exotic_legend),
  ncol = 2,
  widths = c(1.1, 1)
)

# Display the exotic plot
grid.newpage()
grid::grid.draw(combined_plots)

# Save the plots
ggsave(
  "figure_1.jpeg",
  plot = combined_plots,
  width = 15.75,
  height = 24,
  units = "cm"
)
ggsave(
  "figure_1.tiff",
  plot = combined_plots,
  width = 15.75,
  height = 24,
  units = "cm"
)
