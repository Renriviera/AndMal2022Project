#!/usr/bin/env Rscript
# Complete script to run all interpretability sections

# Load libraries
library(ranger)
library(dplyr)
library(caret)
library(parallel)
library(lime)
library(fastshap)
library(ggplot2)
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

# Load model to get required features, but we'll exclude Hash from them
if (file.exists("data/models/rf_category_model.rds")) {
  rf_category_temp <- readRDS("data/models/rf_category_model.rds")
  required_features <- rf_category_temp$forest$independent.variable.names
  # Remove Hash if it's in the required features
  required_features <- setdiff(required_features, "Hash")
  rm(rf_category_temp)
} else {
  # If model doesn't exist, we'll determine features from data
  all_cols <- names(andmal_after)
  metadata_cols_present <- intersect(metadata_cols, all_cols)
  required_features <- setdiff(all_cols, metadata_cols_present)
  required_features <- required_features[!str_detect(required_features, "rank|color|fam_color|rank_in_cat")]
}

# Add missing features with default values
missing_features <- setdiff(required_features, names(andmal_after))
if (length(missing_features) > 0) {
  cat(sprintf("Adding %d missing features with default values...\n", length(missing_features)))
  for (feat in missing_features) {
    # Use 0 for numeric features, FALSE for logical
    if (grepl("^has_|^dex_any$", feat)) {
      andmal_after[[feat]] <- FALSE
    } else {
      andmal_after[[feat]] <- 0
    }
  }
  cat("Missing features added\n")
}

# Use the required features (already added to dataset)
feature_cols_model1 <- required_features
feature_cols_family <- feature_cols_model1

cat(sprintf("Using %d features for model\n", length(feature_cols_model1)))
cat(sprintf("Dataset now has %d columns\n", ncol(andmal_after)))

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

# Convert to plain data.frame, completely removing tibble attributes
train_model1_x <- data.frame(train_data[, feature_cols_model1, drop = FALSE], 
                              stringsAsFactors = FALSE, check.names = FALSE)
train_model1_y <- train_data$Category
test_model1_x <- data.frame(test_data[, feature_cols_model1, drop = FALSE], 
                             stringsAsFactors = FALSE, check.names = FALSE)
test_model1_y <- test_data$Category

# Ensure they are plain data.frames
class(train_model1_x) <- "data.frame"
class(test_model1_x) <- "data.frame"
attr(train_model1_x, "row.names") <- .set_row_names(nrow(train_model1_x))
attr(test_model1_x, "row.names") <- .set_row_names(nrow(test_model1_x))

# Load category model
cat("Loading category model...\n")
rf_category <- readRDS("data/models/rf_category_model.rds")

# ============================================================================
# CATEGORY MODEL INTERPRETABILITY
# ============================================================================

cat("\n=== CATEGORY MODEL INTERPRETABILITY ===\n")

# Instance Selection
cat("\n--- Instance Selection ---\n")
set.seed(42)
target_categories <- c("Adware", "Riskware", "Trojan")
selected_instances <- list()

for (category in target_categories) {
  category_indices <- which(test_data$Category == category)
  if (length(category_indices) > 0) {
    selected_idx <- category_indices[1]
    selected_instances[[category]] <- test_data[selected_idx, ]
    cat(sprintf("Selected %s instance: row %d\n", category, selected_idx))
  }
}

selected_instances_df <- bind_rows(selected_instances)
selected_instances_x <- data.frame(selected_instances_df[, feature_cols_model1, drop = FALSE], 
                                   stringsAsFactors = FALSE, check.names = FALSE)
class(selected_instances_x) <- "data.frame"
attr(selected_instances_x, "row.names") <- .set_row_names(nrow(selected_instances_x))

if (!dir.exists("data/processed")) {
  dir.create("data/processed", recursive = TRUE)
}
saveRDS(selected_instances_df, "data/processed/selected_instances.rds")
cat("Saved selected instances\n")

# LIME Explanations
cat("\n--- LIME Explanations for Category Model ---\n")
set.seed(42)

# Ensure selected instances have the same columns in the same order as training data
selected_instances_x <- selected_instances_x[, colnames(train_model1_x), drop = FALSE]

# Ensure all required features are present in selected instances
missing_in_selected <- setdiff(colnames(train_model1_x), colnames(selected_instances_x))
if (length(missing_in_selected) > 0) {
  for (feat in missing_in_selected) {
    if (grepl("^has_|^dex_any$", feat)) {
      selected_instances_x[[feat]] <- FALSE
    } else {
      selected_instances_x[[feat]] <- 0
    }
  }
  selected_instances_x <- selected_instances_x[, colnames(train_model1_x), drop = FALSE]
}

# Prediction function for LIME
predict_function <- function(model, newdata) {
  # Ensure newdata has same columns as training data, in same order
  newdata <- as.data.frame(newdata)
  missing_cols <- setdiff(colnames(train_model1_x), colnames(newdata))
  if (length(missing_cols) > 0) {
    for (feat in missing_cols) {
      if (grepl("^has_|^dex_any$", feat)) {
        newdata[[feat]] <- FALSE
      } else {
        newdata[[feat]] <- 0
      }
    }
  }
  newdata <- newdata[, colnames(train_model1_x), drop = FALSE]
  pred <- predict(model, newdata)
  return(pred$predictions)
}

cat("Creating LIME explainer...\n")
explainer <- lime::lime(
  train_model1_x,
  model = rf_category,
  bin_continuous = TRUE,
  n_bins = 5
)

cat("Generating LIME explanations...\n")
lime_explanations <- lime::explain(
  selected_instances_x,
  explainer = explainer,
  n_features = 10,
  n_permutations = 5000,
  n_labels = 1
)

cat("Plotting LIME explanations...\n")
for (i in 1:nrow(selected_instances_x)) {
  category_name <- selected_instances_df$Category[i]
  cat(sprintf("\nLIME Explanation for %s instance:\n", category_name))
  p <- plot_features(lime_explanations[lime_explanations$case == i, ])
  print(p)
  
  # Save plot
  ggsave(sprintf("plots/lime_category_%s.png", tolower(category_name)), 
         plot = p, width = 10, height = 6, dpi = 300)
}

# SHAP Per-Instance
cat("\n--- Per-Instance SHAP Values for Category Model ---\n")
set.seed(42)

# Create prediction wrapper that handles data type conversion
# fastshap may call this with different argument formats, so we use ... to catch all
pred_wrapper <- function(object, newdata, ...) {
  # Handle different calling conventions - newdata might be in ...
  args <- list(...)
  if (missing(newdata) && "newdata" %in% names(args)) {
    newdata <- args$newdata
  } else if (missing(newdata) && length(args) > 0) {
    newdata <- args[[1]]
  }
  
  # Convert to plain data.frame, removing any tibble attributes
  if (inherits(newdata, "tbl_df") || inherits(newdata, "tbl")) {
    newdata <- as.data.frame(newdata, stringsAsFactors = FALSE)
  }
  newdata <- data.frame(newdata, stringsAsFactors = FALSE, check.names = FALSE)
  class(newdata) <- "data.frame"
  
  # Ensure all required columns are present and in correct order
  missing_cols <- setdiff(colnames(train_model1_x), colnames(newdata))
  if (length(missing_cols) > 0) {
    for (feat in missing_cols) {
      if (grepl("^has_|^dex_any$", feat)) {
        newdata[[feat]] <- FALSE
      } else {
        newdata[[feat]] <- 0
      }
    }
  }
  newdata <- newdata[, colnames(train_model1_x), drop = FALSE]
  
  pred <- predict(object, newdata)
  return(pred$predictions)
}

shap_values_list <- list()

for (i in 1:nrow(selected_instances_x)) {
  category_name <- selected_instances_df$Category[i]
  cat(sprintf("Calculating SHAP values for %s instance...\n", category_name))
  
  instance_data <- as.data.frame(selected_instances_x[i, , drop = FALSE], stringsAsFactors = FALSE)
  class(instance_data) <- "data.frame"
  # Ensure all columns are present
  missing_cols <- setdiff(colnames(train_model1_x), colnames(instance_data))
  if (length(missing_cols) > 0) {
    for (feat in missing_cols) {
      if (grepl("^has_|^dex_any$", feat)) {
        instance_data[[feat]] <- FALSE
      } else {
        instance_data[[feat]] <- 0
      }
    }
    instance_data <- as.data.frame(instance_data[, colnames(train_model1_x), drop = FALSE], stringsAsFactors = FALSE)
    class(instance_data) <- "data.frame"
  }
  
  # Use feature_names parameter to ensure compatibility
  shap_vals <- fastshap::explain(
    object = rf_category,
    feature_names = colnames(train_model1_x),
    X = train_model1_x,
    newdata = instance_data,
    pred_wrapper = pred_wrapper,
    nsim = 100
  )
  
  shap_values_list[[category_name]] <- shap_vals
  
  pred_probs <- predict(rf_category, instance_data)$predictions
  pred_class <- colnames(pred_probs)[which.max(pred_probs)]
  
  if (is.data.frame(shap_vals) || is.matrix(shap_vals)) {
    if (pred_class %in% colnames(shap_vals)) {
      shap_df <- data.frame(
        feature = rownames(shap_vals),
        shap_value = shap_vals[[pred_class]]
      )
    } else if (ncol(shap_vals) == length(feature_cols_model1)) {
      shap_df <- data.frame(
        feature = colnames(shap_vals),
        shap_value = as.numeric(shap_vals[1, ])
      )
    } else {
      if (nrow(shap_vals) == 1) {
        shap_df <- data.frame(
          feature = colnames(shap_vals),
          shap_value = as.numeric(shap_vals[1, ])
        )
      } else {
        shap_df <- data.frame(
          feature = rownames(shap_vals),
          shap_value = as.numeric(shap_vals[, 1])
        )
      }
    }
    
    shap_df <- shap_df %>%
      arrange(desc(abs(shap_value))) %>%
      head(20)
    
    cat(sprintf("Top 20 SHAP values (predicted class: %s):\n", pred_class))
    print(shap_df)
    
    # Plot
    p <- ggplot(shap_df, aes(x = reorder(feature, shap_value), y = shap_value)) +
      geom_col() +
      coord_flip() +
      labs(
        title = sprintf("SHAP Values - %s Instance", category_name),
        x = "Feature",
        y = "SHAP Value"
      ) +
      theme_minimal()
    print(p)
    ggsave(sprintf("plots/shap_category_%s_instance.png", tolower(category_name)), 
           plot = p, width = 10, height = 6, dpi = 300)
  }
}

# Mean Absolute SHAP Values section removed per user request

# ============================================================================
# FAMILY MODELS INTERPRETABILITY
# ============================================================================

cat("\n=== FAMILY MODELS INTERPRETABILITY ===\n")

# Load family models
cat("Loading family models...\n")
category_models <- list()
target_categories <- c("Adware", "Riskware", "Trojan")

for (category in target_categories) {
  model_path <- paste0("data/models/rf_family_", tolower(category), "_model.rds")
  if (file.exists(model_path)) {
    category_models[[category]] <- readRDS(model_path)
    cat(sprintf("Loaded %s model\n", category))
  }
}

# Process each family model
for (category in target_categories) {
  if (category %in% names(category_models) && category %in% selected_instances_df$Category) {
    cat(sprintf("\n=== %s Family Model Explanations ===\n", category))
    
    instance_idx <- which(selected_instances_df$Category == category)
    instance_x <- as.data.frame(selected_instances_df[instance_idx, feature_cols_family, drop = FALSE])
    
    category_train_data <- andmal_after %>% filter(Category == category)
    
    if (nrow(category_train_data) > 0) {
      # Recreate train/test split for this category
      set.seed(42)
      if (caret_available) {
        cat_train_idx <- caret::createDataPartition(
          category_train_data$Family,
          p = train_test_split,
          list = FALSE,
          times = 1
        )
        cat_train <- category_train_data[cat_train_idx, ]
      } else {
        families <- unique(category_train_data$Family)
        train_indices <- c()
        for (fam in families) {
          fam_indices <- which(category_train_data$Family == fam)
          if (length(fam_indices) > 1) {
            n_train <- max(1, round(length(fam_indices) * train_test_split))
            train_fam_indices <- sample(fam_indices, n_train)
            train_indices <- c(train_indices, train_fam_indices)
          } else {
            train_indices <- c(train_indices, fam_indices)
          }
        }
        cat_train <- category_train_data[train_indices, ]
      }
      
      train_family_x <- as.data.frame(cat_train[, feature_cols_family, drop = FALSE])
      model <- category_models[[category]]
      
      # LIME
      cat(sprintf("\n--- LIME Explanation for %s Instance ---\n", category))
      explainer_family <- lime::lime(
        train_family_x,
        model = model,
        bin_continuous = TRUE,
        n_bins = 5
      )
      
      lime_explanation <- lime::explain(
        instance_x,
        explainer = explainer_family,
        n_features = 10,
        n_permutations = 5000,
        n_labels = 1
      )
      
      p <- plot_features(lime_explanation)
      print(p)
      ggsave(sprintf("plots/lime_family_%s.png", tolower(category)), 
             plot = p, width = 10, height = 6, dpi = 300)
      
      # Per-Instance SHAP
      cat(sprintf("\n--- Per-Instance SHAP Values for %s Instance ---\n", category))
      pred_wrapper_family <- function(object, newdata) {
        pred <- predict(object, newdata)
        return(pred$predictions)
      }
      
      shap_instance <- fastshap::explain(
        model,
        X = train_family_x,
        newdata = instance_x,
        pred_wrapper = pred_wrapper_family,
        nsim = 100
      )
      
      pred_probs <- predict(model, instance_x)$predictions
      pred_class <- colnames(pred_probs)[which.max(pred_probs)]
      
      if (is.data.frame(shap_instance) || is.matrix(shap_instance)) {
        if (pred_class %in% colnames(shap_instance)) {
          shap_df <- data.frame(
            feature = rownames(shap_instance),
            shap_value = shap_instance[[pred_class]]
          )
        } else if (ncol(shap_instance) == length(feature_cols_family)) {
          shap_df <- data.frame(
            feature = colnames(shap_instance),
            shap_value = as.numeric(shap_instance[1, ])
          )
        } else {
          if (nrow(shap_instance) == 1) {
            shap_df <- data.frame(
              feature = colnames(shap_instance),
              shap_value = as.numeric(shap_instance[1, ])
            )
          } else {
            shap_df <- data.frame(
              feature = rownames(shap_instance),
              shap_value = as.numeric(shap_instance[, 1])
            )
          }
        }
        
        shap_df <- shap_df %>%
          arrange(desc(abs(shap_value))) %>%
          head(20)
        
        cat(sprintf("Top 20 SHAP values (predicted class: %s):\n", pred_class))
        print(shap_df)
        
        p <- ggplot(shap_df, aes(x = reorder(feature, shap_value), y = shap_value)) +
          geom_col() +
          coord_flip() +
          labs(
            title = sprintf("SHAP Values - %s Family Model (%s Instance)", category, category),
            x = "Feature",
            y = "SHAP Value"
          ) +
          theme_minimal()
        print(p)
        ggsave(sprintf("plots/shap_family_%s_instance.png", tolower(category)), 
               plot = p, width = 10, height = 6, dpi = 300)
      }
      
      # Mean Absolute SHAP section removed per user request
    }
  }
}

cat("\n=== Interpretability Analysis Complete ===\n")
cat("All plots have been saved to the plots/ directory\n")

