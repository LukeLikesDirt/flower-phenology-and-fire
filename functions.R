# Function to extract emmeans results for GLM
get_emmeans_lifeform <- function(model, lifeform_value) {
  # Get emmeans
  emm <- emmeans::emmeans(
    model,
    list(pairwise ~ month),
    adjust = "tukey",
    type = "response"
  )
  
  # Extract and format emmeans
  means_df <- emm$`emmeans of month` %>%
    as_tibble() %>%
    rename(mean = rate) %>%
    mutate(lifeform = lifeform_value)
  
  return(list(means = means_df))
}

get_emmeans_status <- function(model, status_value) {
  # Get emmeans
  emm <- emmeans::emmeans(
    model,
    list(pairwise ~ month),
    adjust = "tukey",
    type = "response"
  )
  
  # Extract and format emmeans
  means_df <- emm$`emmeans of month` %>%
    as_tibble() %>%
    rename(mean = rate) %>%
    mutate(status = status_value)
  
  return(list(means = means_df))
}

# Function to extract coefficients from a GLM fitted on a bootstrap sample
extract_coefs <- function(data, indices, model_formula, model_family) {
  # Resample the data with replacement
  d <- data[indices, ]
  
  # --- Bootstrap sample checks to ensure model stability ---
  
  # 1. Ensure all months are represented in the bootstrap sample
  if (length(unique(d$month)) < length(levels(data$month))) return(rep(NA, 3))
  
  # 2. Ensure there is variation in the response variable for at least 4 months
  if (sum(tapply(d$richness_peak_flowering, d$month, var, na.rm = TRUE) > 0, na.rm = TRUE) < 4) return(rep(NA, 3))
  
  # 3. Ensure each month has at least 4 observations
  if (any(table(d$month) < 4)) return(rep(NA, 3))
  
   # --- Try fitting the GLM; catch errors or convergence issues ---
  tryCatch({
    model <- glm(model_formula, family = model_family, data = d,
                 control = list(maxit = 100, epsilon = 1e-8))

    # If model did not converge, return NA
    if (!model$converged) return(rep(NA, 3))

    # Extract coefficients (excluding intercept)
    coefs <- coef(model)[-1]

    # Check coefficient validity: length, no NA or Inf
    if (length(coefs) != 3 || any(is.na(coefs)) || any(is.infinite(coefs))) return(rep(NA, 3))

    # Return bootstrap sample coefficients
    return(coefs)
  }, error = function(e) {
    # Return NA vector if model fitting fails
    return(rep(NA, 3))
  })
}

# October effect (excluded in model)
get_oct_effect_glm <- function(coefs) {
  -sum(coefs)
}

# Extract emmeans for the model
extract_emmeans <- function(model) {
  emm <- emmeans::emmeans(model, ~ month, type = "response")
  as.numeric(emmeans::summary(emm)$emmean)
}

# Bootstrapping function
bootstrap_glm_effects <- function(model, model_data, nsim = 10000) {
  set.seed(1986)

  model_formula <- formula(model)
  model_family <- family(model)

  boot_results <- boot(
    data = model_data,
    statistic = extract_coefs,
    R = nsim,
    model_formula = model_formula,
    model_family = model_family
  )

  boot_df <- as.data.frame(boot_results$t)
  colnames(boot_df) <- c("Nov", "Dec", "Jan")
  boot_df_clean <- boot_df[complete.cases(boot_df), ]

  oct_effects <- apply(boot_df_clean, 1, get_oct_effect_glm)

  # Function for 95% percentile CI
  ci <- function(x) quantile(x, c(0.025, 0.975), na.rm = TRUE)

  # Summary table based on bootstrap means
  effect_tibble <- tibble(
    Month = c("Oct", "Nov", "Dec", "Jan"),
    effect = c(mean(oct_effects), colMeans(boot_df_clean)),
    lower = c(ci(oct_effects)[1], ci(boot_df_clean$Nov)[1], ci(boot_df_clean$Dec)[1], ci(boot_df_clean$Jan)[1]),
    upper = c(ci(oct_effects)[2], ci(boot_df_clean$Nov)[2], ci(boot_df_clean$Dec)[2], ci(boot_df_clean$Jan)[2])
  )

  # Density data
  density_data <- bind_rows(
    with(density(oct_effects, adjust = 3.0), tibble(Effect = x, Density = y, Month = "Oct")),
    with(density(boot_df_clean$Nov, adjust = 3.0), tibble(Effect = x, Density = y, Month = "Nov")),
    with(density(boot_df_clean$Dec, adjust = 3.0), tibble(Effect = x, Density = y, Month = "Dec")),
    with(density(boot_df_clean$Jan, adjust = 3.0), tibble(Effect = x, Density = y, Month = "Jan"))
  ) %>%
    mutate(Month = factor(Month, levels = c("Oct", "Nov", "Dec", "Jan")))

  # Intercept estimate and 95% CI
  intercept_ci <- tryCatch({
    confint(model)[1, ]
  }, error = function(e) {
    c(coef(model)[1] - 1.96 * summary(model)$coefficients[1, 2],
      coef(model)[1] + 1.96 * summary(model)$coefficients[1, 2])
  })

  intercept_formatted <- sprintf(
    "%.2f [%.2f, %.2f]",
    coef(model)[1],
    intercept_ci[1],
    intercept_ci[2]
  )

  return(list(
    effects = effect_tibble,
    density_data = density_data,
    intercept = intercept_formatted,
    boot_results = boot_results,
    n_successful = nrow(boot_df_clean)
  ))
}

