#!/usr/bin/env Rscript
# Script to run interpretability sections from category.qmd and family.qmd

# Load required libraries
library(ranger)
library(dplyr)
library(caret)
library(parallel)
library(lime)
library(fastshap)
library(ggplot2)
library(stringr)

# Check if processed data exists, if not, we need to run preprocessing first
if (!file.exists("data/processed/andmal_after.rds")) {
  cat("ERROR: Processed data not found. Please run preprocessing.qmd first.\n")
  cat("Attempting to create processed data from raw files...\n")
  
  # Minimal preprocessing - just load and save
  library(readr)
  library(tidyr)
  library(purrr)
  
  data_dir <- "data/raw/AndMal2020-dynamic-BeforeAndAfterReboot"
  files <- list.files(path = data_dir, pattern = "_Cat\\.csv$", full.names = TRUE)
  
  cat(sprintf("Found %d CSV files\n", length(files)))
  
  andmal <- tibble(path = files) %>%
    mutate(
      file = basename(path),
      Category_file = str_replace(file, "_(before|after)_reboot_Cat\\.csv", ""),
      reboot_state = if_else(
        str_detect(file, "before_reboot"),
        "Before reboot",
        "After reboot"
      ),
      data = map(path, readr::read_csv, show_col_types = FALSE)
    ) %>%
    unnest(data) %>%
    mutate(
      Category = factor(Category),
      Family = factor(Family),
      Category_file = factor(Category_file),
      reboot_state = factor(reboot_state, levels = c("Before reboot", "After reboot"))
    )
  
  andmal_after <- andmal %>% filter(reboot_state == "After reboot")
  
  # Create directory if needed
  if (!dir.exists("data/processed")) {
    dir.create("data/processed", recursive = TRUE)
  }
  
  saveRDS(andmal_after, "data/processed/andmal_after.rds")
  cat("Saved processed data\n")
}

# Load processed data
cat("Loading processed data...\n")
andmal_after <- readRDS("data/processed/andmal_after.rds")
cat(sprintf("Loaded dataset: %d rows, %d columns\n", nrow(andmal_after), ncol(andmal_after)))

# Now we can proceed with interpretability
cat("\n=== Starting Interpretability Analysis ===\n")
cat("This will run the interpretability sections from category.qmd and family.qmd\n")
cat("Note: This is a simplified version. For full analysis, render the .qmd files.\n")

