# ============================================================================ #
# BirdNET Evaluation Script for AAD 2021 Data
# ---------------------------------------------------------------------------- #
# Description:
# This script evaluates BirdNET species identification performance using a range
# of parameters. It compares predictions against expert annotations using
# metrics such as precision, recall, and F1-score across the different temporal 
# aggregations (file, day, session, full dataset).
#
# Outputs:
# - CSV of metrics for all parameter combinations
# - Top-performing settings by F1 and recall
# - Combined comparison of best and default settings
#
# Author: Andrew J. Fairbairn
# Date: 18.07.2025
# ============================================================================ #

# ---------------------------------------------------------------------------- #
library(data.table)
library(dplyr)
library(tidyr)
library(pbapply)
library(jsonlite)
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Load Data
# ---------------------------------------------------------------------------- #

# Expert labels (AAD 2021 East region)
expert <- read.csv("expert.csv")
expert$date <- data.table::as.IDate(expert$date, format = "%d/%m/%Y")

# BirdNET predictions
resultsBN <- fread("BirdNET.csv")

# ---------------------------------------------------------------------------- #
# Define Evaluation Function
# ---------------------------------------------------------------------------- #

calculate_metrics_dt <- function(predicted_species, tagged_species) {
  setDT(predicted_species)
  setDT(tagged_species)
  
  # Create keys for efficient joins
  setkeyv(predicted_species, names(predicted_species))
  setkeyv(tagged_species, names(tagged_species))
  
  # True positives (intersection)
  tp <- fsetdiff(predicted_species, tagged_species, all = FALSE)
  false_positives <- nrow(tp)
  
  fn <- fsetdiff(tagged_species, predicted_species, all = FALSE)
  false_negatives <- nrow(fn)
  
  true_positives <- nrow(predicted_species) - false_positives
  
  precision <- true_positives / (true_positives + false_positives)
  recall    <- true_positives / (true_positives + false_negatives)
  f1_score  <- 2 * (precision * recall) / (precision + recall)
  
  return(data.frame(
    precision = precision,
    recall = recall,
    f1_score = f1_score,
    true_positives = true_positives,
    false_positives = false_positives,
    false_negatives = false_negatives,
    false_positive_species = I(list(unique(tp$scientific_name))),
    false_negative_species = I(list(unique(fn$scientific_name)))
  ))
}

# ---------------------------------------------------------------------------- #
# Setup Parameters and Groupings
# ---------------------------------------------------------------------------- #

# Aggregation levels for analysis
grouping_levels <- list(
  "minute" = "filename",
  "day" = "date",
  "week" = "session",
  "dataset" = NULL
)

# Hyperparameter grid
hyperparams <- expand.grid(
  week = c(TRUE, FALSE),
  sensitivity = c(0.5, 1.0, 1.5),
  overlap = c(0.0, 1.0, 2.0, 2.9),
  minconf = seq(0.1, 0.9, by = 0.01)
)

# ---------------------------------------------------------------------------- #
# Run Evaluation
# ---------------------------------------------------------------------------- #

# Convert to data.table
setDT(resultsBN)
setDT(expert)

# Initialize results list
results_list <- list()

# Loop over grouping levels
for (level_name in names(grouping_levels)) {
  level_columns <- grouping_levels[[level_name]]
  results_list[[level_name]] <- list()
  
  # Deduplicate expert labels ONCE
  aggregated_human <- unique(na.omit(expert[, c("scientific_name", ..level_columns)]))
  
  for (i in 1:nrow(hyperparams)) {
    # Extract current hyperparameters
    wk <- hyperparams$week[i]
    sen <- hyperparams$sensitivity[i]
    ovlp <- hyperparams$overlap[i]
    minc <- hyperparams$minconf[i]
    
    # Filter predictions
    filtered_ml <- resultsBN[
      week == wk & sensitivity == sen & overlap == ovlp & confidence >= minc,
      c("scientific_name", ..level_columns)
    ]
    
    # Deduplicate predictions
    aggregated_ml <- unique(na.omit(filtered_ml))
    
    # Calculate metrics
    metrics <- calculate_metrics_dt(aggregated_ml, aggregated_human)
    
    # Build key
    key <- paste0("wk_", wk, "_sen_", sen, "_ovlp_", ovlp, "_minc_", minc)
    results_list[[level_name]][[key]] <- metrics
  }
}

# ---------------------------------------------------------------------------- #
# Combine and Save All Results
# ---------------------------------------------------------------------------- #

results_df <- rbindlist(lapply(names(results_list), function(level_name) {
  inner_list <- results_list[[level_name]]
  
  data.table(
    level = level_name,
    hyperparameters = names(inner_list),
    precision = sapply(inner_list, function(m) m$precision),
    recall = sapply(inner_list, function(m) m$recall),
    f1_score = sapply(inner_list, function(m) m$f1_score),
    true_positives = sapply(inner_list, function(m) m$true_positives),
    false_positives = sapply(inner_list, function(m) m$false_positives),
    false_negatives = sapply(inner_list, function(m) m$false_negatives),
    false_positive_species = I(lapply(inner_list, function(m) m$false_positive_species[[1]])),
    false_negative_species = I(lapply(inner_list, function(m) m$false_negative_species[[1]]))
  )
}), fill = TRUE)

# Convert list-columns to JSON for export
results_df[, false_positive_species := sapply(false_positive_species, jsonlite::toJSON)]
results_df[, false_negative_species := sapply(false_negative_species, jsonlite::toJSON)]

results_df <- results_df %>%
  extract(hyperparameters, c("week", "sensitivity", "overlap", "minConf"),
          "wk_([^_]+)_sen_([^_]+)_ovlp_([^_]+)_minc_(.+)")

# Save full results
write.csv(results_df, "../results/comparison_results.csv", row.names = FALSE)

# ---------------------------------------------------------------------------- #
# Extract Top Results
# ---------------------------------------------------------------------------- #

# Extract top F1 scores per grouping
top_f1 <- results_df %>%
  filter(true_positives > 0) %>%
  group_by(level) %>%
  top_n(n = 1, wt = f1_score)

# Save top F1 scores
write.csv(top_f1, "../results/top_f1.csv", row.names = FALSE)

# Default configuration performance
default_f1 <- results_df %>%
  filter(true_positives > 0,
         sensitivity == 1.0,
         overlap == 0.0,
         minConf == 0.1) %>%
  group_by(level) %>%
  top_n(n = 1, wt = f1_score)

# Combine and save comparison
combined_df <- bind_rows(top_f1, default_f1) %>%
  arrange(level, sensitivity)

write.csv(combined_df, "../results/combined_f1.csv", row.names = FALSE)

# ---------------------------------------------------------------------------- #
# End of Script
# ---------------------------------------------------------------------------- #

