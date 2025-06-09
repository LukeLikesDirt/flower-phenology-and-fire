require(data.table)
require(tidyverse)

# Load phenology data
fortrans <- fread("bin/clean_phenology_binary_flw.csv")
colnames(fortrans) <- sub("_", "\\.", colnames(fortrans))
colnames(fortrans) <- sub(" ", "", colnames(fortrans))
pivot_flower <- pivot_longer(fortrans, cols = c(-sIte.code, -plot.number, -fensham.quadrat), names_sep = "\\.", names_to = c("Month", "Species"))

# Load life form data
spp_info <- fread("bin/life_forms.csv")

# Join data
peak_flowering_data <- left_join(
  # Differences in types of joining functions:
  # left_join will keep species found in the left (here pivot_flower) dataframe.
  # right_join will keep species found in the right (here spp_info) dataframe.
  # inner_join will keep matching species found in both dataframes.
  # full_join will keep all species from both dataframes, regardless is they are
  # fond in both dataframes.
  pivot_flower,
  spp_info,
  # Remember to define the joining variable
  by = "Species"
) %>%
  group_by(sIte.code, Month, status, Species) %>%
  summarise(total_flowering = sum(value), .groups = 'drop') %>%
  filter(total_flowering > 0) %>%
  group_by(sIte.code, Species) %>%
  mutate(peak_mnth = as.integer(total_flowering == max(total_flowering))) %>%
  ungroup(.) %>%
  # Summarise the number of species that have thier peak flowering in a
  # given month
  group_by(sIte.code, status, Month) %>%
  summarise(
    richness_peak = sum(peak_mnth), .groups = 'drop'
  ) %>%
  mutate(
    Month = factor(Month, levels = c("November", "December", "January", "October"))
  ) %>%
  rename(
    site = sIte.code,
    month = Month,
    richness_peak_flowering = richness_peak
  ) %>%
  fwrite(., "data_status.txt", sep = "\t")
