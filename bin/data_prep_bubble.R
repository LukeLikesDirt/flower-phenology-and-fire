require(tidyverse)
#Organising Data ----
#cleanphenologydata uses the data from Keoghs collapsed work after I went back through and cleaned out double ups,it also has mandi's neat heading!

fortrans <- read.csv("bin/clean_phenology_binary_flw.csv")

colnames(fortrans) <- sub("_", "\\.", colnames(fortrans))
colnames(fortrans) <- sub(" ", "", colnames(fortrans))
pivot_flower <- pivot_longer(fortrans, cols = c(-sIte.code, -plot.number, -fensham.quadrat), names_sep = "\\.", names_to = c("Month", "Species"))

spp_info <- read.csv("bin/life_forms.csv")

entire_set <- left_join(pivot_flower, spp_info)
entire_set$flowering <- ifelse(entire_set$value == 0, "Not flowering", "Flowering")

new_data <- aggregate(value ~ Species + Month + status, data=entire_set, FUN = sum)

#this cleans the new data to make it binary again : if the summed value = 0 then keep 0, if not, make 1.  
#As a result each spp has a value of 0 or 1 for each month. 1= flowered 0= not flowered

new_data$flowered <- ifelse(new_data$value == 0, 0, 1)
new_data$Month <- factor(new_data$Month, levels = c("October", "November", "December", "January"))

# Sort the data by the ordered "Month" variable
new_data <- new_data[order(new_data$Month), ]
#ordering native first
new_data$status <- factor(new_data$status, levels = c("Native", "Exotic"))

#Creating Dataframe with spp.info, and only "Species", "Month", "value", "flowered", "life_form" and "family", "status"----
#### Merge the "life_form" column from spp_info into new_data based on "Species"
new_data_merge_spp_info <- merge(new_data, spp_info[, c("Species", "life_form","family")], by = "Species", all.x = TRUE)


#####New Bubble---------

# Step 1: Prepare the data and group species by months_flowered
# Join the unique months each species flowers in
flowering_combinations <- new_data_merge_spp_info %>%
  filter(flowered == 1) %>% 
  group_by(Species) %>%
  summarize(months_flowered = paste(sort(unique(Month)), collapse = ","), .groups = 'drop')

# Merge the groupings back into the main dataframe
bubble_df <- left_join(new_data_merge_spp_info, flowering_combinations, by = "Species")

# Reorder 'months_flowered' for consistent groupings
bubble_df$months_flowered <- factor(bubble_df$months_flowered, 
                                    levels = c("October" ,"October,November","October,November,December", 
                                               "October,November,December,January",  "October,December", 
                                               "October,January", "October,December,January" , "November", 
                                               "November,December", "November,December,January" , 
                                               "December" , "December,January" , "January"))

# Order species within each 'months_flowered' group
bubble_df <- bubble_df %>%
  arrange(months_flowered, Species) %>%  # Order by 'months_flowered' and 'Species'
  mutate(
    Species = str_replace_all(Species, "_", " ") %>%          # Replace underscores with spaces
      str_to_sentence(),                                      # Capitalize first letter of each word
    Species = str_extract(Species, "^\\w+ \\w+"),             # Extract the first two words
    Species = str_replace_all(Species, "\\bsp\\b", "sp."),    # Replace 'sp' with 'sp.'
    Species = factor(Species, levels = rev(unique(Species))) # Reverse the order of unique 'Species'
  )

# Save the data for the bubble plot
write.csv(bubble_df, "data_bubble_plot.csv", row.names = FALSE)
