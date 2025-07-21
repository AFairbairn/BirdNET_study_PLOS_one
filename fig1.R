# ============================================================================ #
# Figure 1: F1 score by week inclusion ("yes"/"no"), faceted by level
# ---------------------------------------------------------------------------- #
# This script creates a boxplot comparing BirdNET F1 scores between models 
# with and without the week variable included. The results are faceted by 
# temporal aggregation level (minute, day, week, dataset), using cleaned 
# output from the BirdNET parameter evaluations.
#
# Requires the results file from evaluate_birdnet.R
#
# Author: Andrew J. Fairbairn
# Date: 2025-07-18
# ============================================================================ #

# Load required libraries
library(ggplot2)
library(dplyr)

# Load cleaned results data
results_df <- read.csv("../results/comparison_results.csv")

# Generate the boxplot
week_fig <- results_df %>%
  ggplot(aes(x = week, y = f1_score, fill = week)) +
  geom_boxplot() +
  scale_fill_manual(values = c("FALSE" = "#E69F00", "TRUE" = "#56B4E9")) +  # Custom color palette
  facet_grid(~level, scales = "free") +
  labs(
    x = "Week Included",
    y = "F1 Score"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 8, family = "TT Arial"),
    strip.text = element_text(face = "bold")
  )

# Save the figure
ggsave(
  filename = "../results/figure1.tiff",
  plot = week_fig,
  width = 132,
  height = 132,
  units = "mm",
  dpi = 600,
  compression = "lzw"
)

# ---------------------------------------------------------------------------- #
# End of Script
# ---------------------------------------------------------------------------- #