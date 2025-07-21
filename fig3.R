# ============================================================================ #
# Figure 3: Accuracy of BirdNET Species Predictions Based on Manual Checking
# ---------------------------------------------------------------------------- #
# This script loads manually confirmed BirdNET predictions and creates a bar 
# chart showing the proportion of correct identifications per species. 
# Additionally, it checks confidence ranges for species that were missed 
# due to the filtering threshold.
# 
# Author: Andrew J. Fairbairn
# Date: 18.07.2024
# ============================================================================ #

# ---------------------------------------------------------------------------- #
# Load Required Libraries
# ---------------------------------------------------------------------------- #
library(ggplot2)
library(dplyr)
library(ggrepel)
library(data.table)

# ---------------------------------------------------------------------------- #
# Load and Prepare Data
# ---------------------------------------------------------------------------- #

# Load confirmed classification results
df <- read.csv("confirmed_resuts.csv")

# Recode manual labels: "F" = Incorrect, "U" and "T" = Correct
df$MANUAL.ID <- dplyr::recode(df$MANUAL.ID, "F" = "Incorrect", "U" = "Correct", "T" = "Correct")
df$MANUAL.ID <- factor(df$MANUAL.ID, levels = c("Incorrect", "Correct"))

# Compute proportion of each label per species
df$total_n <- ave(df$n, df$scientific_name, FUN = sum)
df$proportion <- df$n / df$total_n

# Summarise proportions by species and correctness
df_summary <- df %>%
  group_by(scientific_name, MANUAL.ID) %>%
  summarise(
    proportion = sum(proportion),
    total_n = sum(n),
    .groups = "drop"
  )

# Highlight a species of interest
df_summary$label <- ifelse(
  df_summary$scientific_name == "Turdus philomelos",
  paste0("* ", df_summary$scientific_name),
  df_summary$scientific_name
)

# ---------------------------------------------------------------------------- #
# Create Plot
# ---------------------------------------------------------------------------- #

# Define colors for correctness
color_mapping <- c("Correct" = "darkgreen", "Incorrect" = "orange")

# Bar chart
p <- ggplot(df_summary, aes(x = scientific_name, y = proportion, fill = MANUAL.ID)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = color_mapping) +
  geom_text(aes(label = total_n, y = 0), vjust = -0.7, size = 2, color = "white") +
  labs(
    x = "Species",
    y = "Proportion of vocalisations",
    fill = "BirdNET\nidentification"
  ) +
  theme_classic() +
  theme(
    text = element_text(size = 8, family = "TT Arial"),
    axis.text.x = element_text(
      angle = 45, hjust = 1, size = 8,
      face = "italic",
      color = ifelse(df_summary$scientific_name == "Turdus philomelos", "red", "black")
    ),
    legend.background = element_rect(fill = "transparent")
  ) +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

# Display and save figure
print(p)
ggsave(
  filename = "../results/figure_3.tiff",
  plot = p,
  width = 132, height = 90,
  units = "mm",
  dpi = 600,
  compression = "lzw"
)

# ============================================================================ #
# Investigate Confidence Threshold for Missed Species
# ---------------------------------------------------------------------------- #
# Checks confidence values for five species missing from the filtered dataset
# at confidence threshold 0.54. This helps assess whether they could be 
# recovered by lowering the threshold.
# ============================================================================ #

# Load full BirdNET results
resultsBN <- fread("BirdNET.csv")

# Filter results based on key parameters
full_results <- resultsBN %>%
  filter(week == TRUE, sensitivity == 1.5, overlap == 0)

filtered_results <- full_results %>%
  filter(confidence >= 0.54)

# Print unique species in filtered set
unique_species <- unique(filtered_results$scientific_name)
print(unique_species)

# Count all species occurrences (for context)
name_counts <- filtered_results %>%
  count(scientific_name, sort = TRUE)

# Define species of interest
missing_species <- c(
  "Passer montanus",
  "Sitta europaea",
  "Columba palumbus",
  "Regulus regulus",
  "Streptopelia decaocto"
)

# Examine confidence ranges for each species
confidence_ranges <- lapply(missing_species, function(species) {
  range(full_results$confidence[full_results$scientific_name == species])
})

# Name and display confidence ranges
names(confidence_ranges) <- missing_species
print(confidence_ranges)

# ---------------------------------------------------------------------------- #
# End of Script
# ---------------------------------------------------------------------------- #