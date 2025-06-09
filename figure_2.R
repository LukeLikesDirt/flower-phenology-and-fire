###############################################################################*
# ANALYSIS OF STATUS FLOWERING PATTERNS
###############################################################################*

# 1. SETUP AND DATA PREPARATION ##############################################

## 1.1 Load required packages ------------------------------------------------
library(data.table)
library(ggridges)
library(boot)
library(emmeans)
library(patchwork)
library(ggtext)
library(tidyverse)
source("functions.R")

## 1.2 Import and prepare data -----------------------------------------------
# Read the raw data
data <- fread("data_status.txt") %>%
  # Fill missing combinations with 0
  complete(status, month, site, fill = list(richness_peak_flowering = 0))

# Define the levels for correct ordering
unique_months <- c("November", "December", "January", "October")

# Convert month to a factor with specified levels
data$month <- factor(data$month, levels = unique_months)

# Set effect coding (sum contrasts) for month factor
contrasts(data$month) <- contr.sum(length(unique_months))

# Add names to contrasts for interpretability
colnames(contrasts(data$month)) <- unique_months[1:3]

# Check the contrasts to confirm correct setup
print(contrasts(data$month))

# Get unique status in the data
unique_status <- unique(data$status)
print(paste("Life forms in data:", paste(unique_status, collapse = ", ")))

# 2. MODEL FITTING AND ANALYSIS #############################################

## 2.1 Fit Generalised Linear Models ----------------------------------------

# Fit GLM by life form
fit_glm_by_status <- function(status_value) {
  subset_data <- data %>% filter(status == status_value)
  
  glm(
    richness_peak_flowering ~ month,
    family = poisson,
    data = subset_data
  )
}

# Fit models for each life form
models_list <- list()
for(status in unique_status) {
  models_list[[status]] <- fit_glm_by_status(status)
}

# Check model assumptions
DHARMa::simulateResiduals(models_list$Exotic, plot = TRUE)
DHARMa::simulateResiduals(models_list$Native, plot = TRUE)

## 2.2 Compute marginal means and pairwise comparisons -----------------------

# Get results for all life forms
emmeans_results <- list()

for(status in names(models_list)) {
  results <- get_emmeans_status(models_list[[status]], status)
  emmeans_results[[status]] <- results$means
}

# Combine results
emmeans_combined <- bind_rows(emmeans_results) %>%
  mutate(
    status = factor(status),
    month = factor(month, levels = c("October", "November", "December", "January"))
  )

# 3. BOOTSTRAPPING EFFECT SIZES #############################################

bootstrap_results <- list()

for(status in names(models_list)) {
  cat("Bootstrapping effects for:", status, "\n")
  status_data <- data %>% filter(status == !!status)
  
  # Bootstrapping with error handling
  tryCatch({
    bootstrap_results[[status]] <- bootstrap_glm_effects(
      models_list[[status]],
      status_data
    )
    cat("  Completed successfully:\n")
    cat("    ", bootstrap_results[[status]]$n_successful, "successful samples\n")
  }, error = function(e) {
    cat("  Error in bootstrapping:", e$message, "\n")
  })
}

# 4. VISUALISATION ############################################################

## 4.1 Set global theme -------------------------------------------------------
theme_set(theme_minimal())

global_theme <- theme(
  panel.border = element_rect(colour = "grey90", linewidth = 0.5, fill = NA),
  plot.title = element_text(hjust = 0.5, size = 10),
  axis.title = element_text(size = 9),
  axis.text = element_text(size = 8),
  legend.position = "none",
  plot.margin = margin(3, 3, 3, 3),
  aspect.ratio = 1
)

# Define consistent color palettes
month_colors <- c(
  "October" = "#E69F00", 
  "November" = "#56B4E9", 
  "December" = "#009E73", 
  "January" = "#CC79A7"
)

month_fills <- c(
  "Oct" = "#E69F00", 
  "Nov" = "#56B4E9", 
  "Dec" = "#009E73", 
  "Jan" = "#CC79A7"
)

## 4.2 Prepare data for plotting ---------------------------------------------

# Prepare observed data for plotting
observed_values <- data %>%
  mutate(
    month = factor(month, levels = c("October", "November", "December", "January")),
    status = factor(status, levels = c("Native", "Exotic"))
  )

# Prepare effects data for plotting
effects_data <- bind_rows(lapply(names(bootstrap_results), function(status) {
  x <- bootstrap_results[[status]]$effects
  x$status <- status
  x
})) %>%
  mutate(
    Month = factor(Month, levels = c("Oct", "Nov", "Dec", "Jan")),
    status = factor(status, levels = c("Native", "Exotic"))
  )

# Prepare density data for plotting
density_data <- bind_rows(lapply(names(bootstrap_results), function(status) {
  x <- bootstrap_results[[status]]$density_data
  x$status <- status  # Explicitly add status column
  x
})) %>%
  mutate(
    Month = factor(Month, levels = c("Oct", "Nov", "Dec", "Jan")),
    status = factor(status, levels = c("Native", "Exotic"))
  )

# Prepare intercept data for plotting
intercept_data <- bind_rows(lapply(names(bootstrap_results), function(status) {
  tibble(
    status = factor(status, levels = c("Native", "Exotic")),
    intercept = bootstrap_results[[status]]$intercept
  )
}))

## 4.3 Create plots for life forms --------------------------------------------

# Plot means
plot_status_means <- ggplot() +
  # Plot observed values
  geom_point(
    data = observed_values,
    aes(x = month, y = richness_peak_flowering, colour = month),
    shape = 20,
    size = 1.25,
    alpha = 0.66,
    position = position_jitter(width = 0.2, seed = 1969)
  ) +
  # Plot fitted means and confidence intervals
  geom_errorbar(
    data = emmeans_combined,
    aes(x = month, ymin = asymp.LCL, ymax = asymp.UCL),
    width = 0,
    linewidth = 0.8
  ) +
  geom_point(
    data = emmeans_combined,
    aes(x = month, y = mean, fill = month),
    shape = 21,
    size = 2,
    stroke = 0.6
  ) +
  scale_color_manual(values = month_colors) +
  scale_fill_manual(values = month_colors) +
  labs(
    x = NULL,
    y = "Number of species",
    tag = "(**a**)"
  ) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_blank(),
    plot.tag = element_markdown(),
    plot.tag.position = c(0.03, 0.95),
  ) +
  global_theme +
  facet_wrap(~ status, nrow = 1)

# Plot effects
plot_status_effects <- ggplot() +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "grey30"
  ) +
  ggridges::geom_density_ridges2(
    data = density_data, 
    aes(x = Effect, y = Month, height = Density, fill = Month),
    stat = "identity", 
    scale = 0.66, 
    alpha = 0.6, 
    linewidth = 0.1,
    colour = alpha('grey30', 0.5)
  ) +
  geom_errorbar(
    data = effects_data,
    aes(x = effect, y = Month, xmin = lower, xmax = upper),
    width = 0, linewidth = 0.8
  ) +
  geom_point(
    data = effects_data,
    aes(x = effect, y = Month),
    shape = 21, fill = "white", stroke = 0.5, size = 1.5
  ) +
  scale_fill_manual(values = month_fills) +
  coord_flip() +
  labs(
    x = "Mean effect (log ratio)",
    y = NULL,
    tag = "(**b**)"
  ) +
  geom_richtext(
    data = intercept_data,
    aes(y = "Oct", x = -Inf, label = paste0("&beta;<sub>0</sub> = ", intercept)),
    hjust = 0.15, vjust = 0.1, size = 2.5,
    fill = NA, label.color = NA,
    inherit.aes = FALSE
  ) +
  theme(
    strip.text = element_blank(),
    plot.tag = element_markdown(),
  ) +
  global_theme +
  facet_wrap(~ status, nrow = 1)

# Create final combined figure
final_plot <- plot_status_means / plot_status_effects

# Display the plot
print(final_plot)

# Save the plot
ggsave(
  "figure_2.jpeg", 
  bg = "white",
  final_plot,
  width = 9,
  height = 9,
  units = "cm",
  dpi = 300
)
ggsave(
  "figure_2.tiff", 
  bg = "white",
  final_plot,
  width = 9,
  height = 9,
  units = "cm"
)

# 5. SUMMARY STATISTICS AND EXPORT ###########################################

# Summarise bootstrap results
bootstrap_summary <- map_dfr(names(bootstrap_results), function(status) {
  bootstrap_results[[status]]$effects %>%
    mutate(status = status) %>%
    select(status, Month, effect, lower, upper)
})

# Export results
fwrite(bootstrap_summary, "figure_2_bootstrap_summary.txt", sep = "\t")
fwrite(emmeans_combined, "figure_2_marginal_means.txt", sep = "\t")
