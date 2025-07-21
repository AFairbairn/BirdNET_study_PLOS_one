# ============================================================================ #
# Figure 2 & Mixed-Effects Modeling of BirdNET Parameter Effects on F1 Scores
# ---------------------------------------------------------------------------- #
# This script fits a linear mixed-effects model to predict F1 scores as a 
# function of BirdNET parameters (sensitivity, overlap, minConf), including 
# interaction effects and random effects per evaluation run.
#
# Requires the results file from evaluate_birdnet.R
# 
# Author: Andrew J. Fairbairn
# Date: 18.07.2025
# ============================================================================ #

# ---------------------------------------------------------------------------- #
# Load Required Libraries
# ---------------------------------------------------------------------------- #
library(ggplot2)
library(nlme)
library(dplyr)
library(patchwork)

# ---------------------------------------------------------------------------- #
# Load and Prepare Data
# ---------------------------------------------------------------------------- #
comparison_results <- read.csv("../results/comparison_results.csv")

# Convert to appropriate types and create 'run' identifier
comparison_results <- comparison_results %>%
  mutate(
    level = factor(level, levels = c("minute", "day", "week", "dataset")),
    week = factor(week),
    run = factor(paste(week, sensitivity, overlap, sep = "_"))
  )

# ---------------------------------------------------------------------------- #
# Fit Mixed-Effects Model
# ---------------------------------------------------------------------------- #
mod <- lme(
  f1_score ~ level + sensitivity + overlap + I(minConf^2) +
    level:sensitivity + level:overlap + level:I(minConf^2) +
    sensitivity:overlap + sensitivity:I(minConf^2) + overlap:I(minConf^2),
  random = ~1 | run/level,
  data = comparison_results
)

# ---------------------------------------------------------------------------- #
# Model Output and Diagnostics
# ---------------------------------------------------------------------------- #
summary(mod)
anova(mod)  # Note: 'level' term has inflated degrees of freedom due to nesting

# Optional diagnostic plots
# plot(mod)
# qqnorm(mod)

# ---------------------------------------------------------------------------- #
# Generate Prediction Grid
# ---------------------------------------------------------------------------- #
new_data <- expand.grid(
  level = c("minute", "day", "week", "dataset"),
  week = c("yes", "no"),
  sensitivity = seq(0.5, 1.5, 0.01),
  overlap = seq(0.0, 2.9, 0.01),
  minConf = seq(0.1, 0.9, 0.1)
)

# Ensure factor levels match model
new_data$level <- factor(new_data$level, levels = c("minute", "day", "week", "dataset"))

# Predict F1 scores from model
new_data$predicted_f1_score <- predict(mod, newdata = new_data, level = 0, allow.new.levels = TRUE)

# ---------------------------------------------------------------------------- #
# Visualization 1: Color by Overlap
# ---------------------------------------------------------------------------- #
plot_overlap <- new_data %>%
  filter(week == "yes", sensitivity == 1) %>%
  ggplot(aes(x = minConf, y = predicted_f1_score, color = overlap, group = overlap)) +
  geom_line() +
  facet_grid(~level) +
  labs(x = "Minimum Confidence", y = "Predicted F1 Score") +
  theme_minimal() +
  scale_color_gradient(low = "black", 
                       high = "cadetblue1", 
                       limits = c(0, 2.9),  # Ensure full range is shown
                       breaks = c(0, 1, 2, 2.9),  # Explicitly include 2.9
                       labels = c("0", "1", "2", "2.9")) +
  guides(color = guide_colorbar(direction = "horizontal")) +
  theme(
    text = element_text(size = 8, family = "TT Arial"),
    legend.key.height = unit(0.5, "cm"),
    legend.position = "top",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  )

# ---------------------------------------------------------------------------- #
# Visualization 2: Color by Sensitivity
# ---------------------------------------------------------------------------- #
plot_sensitivity <- new_data %>%
  filter(week == "yes", overlap == 0) %>%
  ggplot(aes(x = minConf, y = predicted_f1_score, color = sensitivity, group = sensitivity)) +
  geom_line() +
  facet_grid(~level) +
  labs(x = "Minimum Confidence", y = NULL) +
  scale_color_gradient(low = "black", high = "orange") +
  theme_minimal() +
  guides(color = guide_colorbar(direction = "horizontal")) +
  theme(
    text = element_text(size = 8, family = "TT Arial"),
    legend.key.height = unit(0.5, "cm"),
    legend.position = "top",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  )

# ---------------------------------------------------------------------------- #
# Combine and Save Figure
# ---------------------------------------------------------------------------- #
final_plot <- plot_overlap + plot_sensitivity
ggsave(
  filename = "../results/figure_2.tiff",
  plot = final_plot,
  width = 132,
  height = 80,
  units = "mm",
  dpi = 600,
  compression = "lzw"
)

# ---------------------------------------------------------------------------- #
# End of Script
# ---------------------------------------------------------------------------- #
