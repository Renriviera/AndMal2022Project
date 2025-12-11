#!/usr/bin/env Rscript
# Script to retrain all random forest models excluding Hash column

# Load libraries
library(ranger)
library(dplyr)
library(caret)
library(parallel)
library(stringr)

# Configuration
num_threads <- 8L
num_trees <- 500L
train_test_split <- 0.8

# Load processed data
cat("Loading processed data...\n")
andmal_after <- readRDS("data/processed/andmal_after.rds")
cat(sprintf("Loaded dataset: %d rows, %d columns\n", nrow(andmal_after), ncol(andmal_after)))

# Define metadata columns (excluding Hash which is an identifier)
metadata_cols <- c("Category", "Family", "Category_file", "reboot_state", "path", "file", "Hash")

# Identify feature columns
all_cols <- names(andmal_after)
metadata_cols_present <- intersect(metadata_cols, all_cols)
feature_cols_model1 <- setdiff(all_cols, metadata_cols_present)
feature_cols_model1 <- feature_cols_model1[!str_detect(feature_cols_model1, "rank|color|fam_color|rank_in_cat")]

# Ensure Hash is not in features
feature_cols_model1 <- setdiff(feature_cols_model1, "Hash")

cat(sprintf("Feature columns (excluding Hash): %d\n", length(feature_cols_model1)))
cat(sprintf("Excluded metadata columns: %s\n", paste(metadata_cols_present, collapse = ", ")))

# Create train/test split
set.seed(42)
caret_available <- require("caret", quietly = TRUE)

if (caret_available) {
  train_index <- caret::createDataPartition(
    andmal_after$Category,
    p = train_test_split,
    list = FALSE,
    times = 1
  )
  train_data <- andmal_after[train_index, ]
  test_data <- andmal_after[-train_index, ]
} else {
  categories <- unique(andmal_after$Category)
  train_indices <- c()
  for (cat in categories) {
    cat_indices <- which(andmal_after$Category == cat)
    n_train <- round(length(cat_indices) * train_test_split)
    train_cat_indices <- sample(cat_indices, n_train)
    train_indices <- c(train_indices, train_cat_indices)
  }
  train_data <- andmal_after[train_indices, ]
  test_data <- andmal_after[-train_indices, ]
}

cat(sprintf("Training set: %d rows\n", nrow(train_data)))
cat(sprintf("Test set: %d rows\n", nrow(test_data)))

# Prepare data for category model
train_model1_x <- as.data.frame(train_data[, feature_cols_model1, drop = FALSE])
train_model1_y <- train_data$Category
test_model1_x <- as.data.frame(test_data[, feature_cols_model1, drop = FALSE])
test_model1_y <- test_data$Category

# Calculate mtry
mtry_model1 <- floor(sqrt(length(feature_cols_model1)))

cat("\n=== Training Category Model ===\n")
cat(sprintf("Number of trees: %d\n", num_trees))
cat(sprintf("mtry: %d\n", mtry_model1))
cat(sprintf("Number of threads: %d\n", num_threads))

# Train category model
start_time <- Sys.time()
rf_category <- ranger(
  x = train_model1_x,
  y = train_model1_y,
  num.trees = num_trees,
  mtry = mtry_model1,
  min.node.size = 1,
  num.threads = num_threads,
  classification = TRUE,
  probability = TRUE,
  importance = "impurity",
  verbose = TRUE
)
end_time <- Sys.time()
training_time <- difftime(end_time, start_time, units = "mins")
cat(sprintf("Category model training completed in %.2f minutes\n", as.numeric(training_time)))

# Evaluate category model
pred_model1 <- predict(rf_category, test_model1_x)
pred_categories <- pred_model1$predictions
pred_categories_class <- colnames(pred_categories)[apply(pred_categories, 1, which.max)]
pred_categories_class <- factor(pred_categories_class, levels = levels(test_model1_y))

if (caret_available) {
  cm_model1 <- caret::confusionMatrix(pred_categories_class, test_model1_y)
  cat(sprintf("Category Model Accuracy: %.4f\n", cm_model1$overall["Accuracy"]))
} else {
  cm_table1 <- table(Predicted = pred_categories_class, Actual = test_model1_y)
  accuracy1 <- sum(diag(cm_table1)) / sum(cm_table1)
  cat(sprintf("Category Model Accuracy: %.4f\n", accuracy1))
}

# Save category model
if (!dir.exists("data/models")) {
  dir.create("data/models", recursive = TRUE)
}
saveRDS(rf_category, "data/models/rf_category_model.rds")
cat("Saved: data/models/rf_category_model.rds\n")

# ============================================================================
# Train Family Models
# ============================================================================

cat("\n=== Training Family Models ===\n")

target_categories <- c("Adware", "Riskware", "Trojan")
category_models <- list()
feature_cols_family <- feature_cols_model1

mtry_family <- floor(sqrt(length(feature_cols_family)))

for (category in target_categories) {
  cat(sprintf("\n--- Training %s Family Model ---\n", category))
  
  # Filter data for this category
  category_data <- andmal_after %>%
    filter(Category == category)
  
  if (nrow(category_data) == 0) {
    cat(sprintf("  WARNING: No samples found for category '%s'. Skipping.\n", category))
    next
  }
  
  if (length(unique(category_data$Family)) < 2) {
    cat(sprintf("  WARNING: Category '%s' has fewer than 2 families. Skipping.\n", category))
    next
  }
  
  # Create train/test split
  set.seed(42)
  if (caret_available) {
    category_train_index <- caret::createDataPartition(
      category_data$Family,
      p = train_test_split,
      list = FALSE,
      times = 1
    )
    category_train <- category_data[category_train_index, ]
    category_test <- category_data[-category_train_index, ]
  } else {
    families <- unique(category_data$Family)
    train_indices <- c()
    for (fam in families) {
      fam_indices <- which(category_data$Family == fam)
      if (length(fam_indices) > 1) {
        n_train <- max(1, round(length(fam_indices) * train_test_split))
        train_fam_indices <- sample(fam_indices, n_train)
        train_indices <- c(train_indices, train_fam_indices)
      } else {
        train_indices <- c(train_indices, fam_indices)
      }
    }
    category_train <- category_data[train_indices, ]
    category_test <- category_data[-train_indices, ]
  }
  
  # Prepare data
  train_family_x <- as.data.frame(category_train[, feature_cols_family, drop = FALSE])
  train_family_y <- category_train$Family
  test_family_x <- as.data.frame(category_test[, feature_cols_family, drop = FALSE])
  test_family_y <- category_test$Family
  
  # Ensure factor levels match
  train_family_y <- factor(train_family_y, levels = unique(c(train_family_y, test_family_y)))
  test_family_y <- factor(test_family_y, levels = levels(train_family_y))
  
  # Train model
  cat(sprintf("  Training random forest for %s family prediction...\n", category))
  start_time <- Sys.time()
  
  rf_family <- ranger(
    x = train_family_x,
    y = train_family_y,
    num.trees = num_trees,
    mtry = mtry_family,
    min.node.size = 1,
    num.threads = num_threads,
    classification = TRUE,
    probability = TRUE,
    importance = "impurity",
    verbose = FALSE
  )
  
  end_time <- Sys.time()
  training_time <- difftime(end_time, start_time, units = "mins")
  cat(sprintf("  Training completed in %.2f minutes\n", as.numeric(training_time)))
  
  # Evaluate model
  pred_family <- predict(rf_family, test_family_x)
  pred_families <- pred_family$predictions
  pred_families_class <- colnames(pred_families)[apply(pred_families, 1, which.max)]
  pred_families_class <- factor(pred_families_class, levels = levels(test_family_y))
  
  if (caret_available) {
    cm_family <- caret::confusionMatrix(pred_families_class, test_family_y)
    cat(sprintf("  %s Family Model Accuracy: %.4f\n", category, cm_family$overall["Accuracy"]))
  } else {
    cm_table_family <- table(Predicted = pred_families_class, Actual = test_family_y)
    accuracy_family <- sum(diag(cm_table_family)) / sum(cm_table_family)
    cat(sprintf("  %s Family Model Accuracy: %.4f\n", category, accuracy_family))
  }
  
  # Save model
  category_models[[category]] <- rf_family
  model_name <- paste0("data/models/rf_family_", tolower(category), "_model.rds")
  saveRDS(rf_family, model_name)
  cat(sprintf("  Saved: %s\n", model_name))
}

cat("\n=== Model Training Complete ===\n")
cat("All models have been retrained excluding the Hash column.\n")

