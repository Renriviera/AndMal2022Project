# Random Forest Models for Android Malware Classification
# This script trains two random forest models:
# 1. Category prediction (using features, excluding Family)
# 2. Family prediction (using features + Category, excluding Family as predictor)
#
# Optimized for 8 vCPU and 64GB RAM

# ---- Check and Set Library Paths ----
# Print current library paths
cat("Current R library paths:\n")
print(.libPaths())

# Try to add user library path if it exists
user_lib <- Sys.getenv("R_LIBS_USER")
if (user_lib != "" && dir.exists(user_lib)) {
  .libPaths(c(.libPaths(), user_lib))
  cat("Added user library path:", user_lib, "\n")
}

# ---- Load Required Libraries ----
cat("\nLoading libraries...\n")

# Try loading with better error messages
tryCatch({
  library(ranger)
  cat("  ✓ ranger loaded\n")
}, error = function(e) {
  cat("  ✗ Error loading ranger:", e$message, "\n")
  cat("  Current library paths:", paste(.libPaths(), collapse = ", "), "\n")
  stop("Please install ranger or check library paths")
})

# Try to load tidyverse, fall back to individual packages if not available
tidyverse_available <- require("tidyverse", quietly = TRUE)
if (tidyverse_available) {
  cat("  ✓ tidyverse loaded\n")
} else {
  cat("  ⚠ tidyverse not available - loading individual packages\n")
  tryCatch({
    library(dplyr)
    cat("  ✓ dplyr loaded\n")
  }, error = function(e) {
    cat("  ✗ Error loading dplyr:", e$message, "\n")
    stop("Please install dplyr or check library paths")
  })
  
  tryCatch({
    library(readr)
    cat("  ✓ readr loaded\n")
  }, error = function(e) {
    cat("  ✗ Error loading readr:", e$message, "\n")
    stop("Please install readr or check library paths")
  })
  
  library(tibble)  # Usually part of tidyverse
  library(stringr)  # Usually part of tidyverse
  library(tidyr)  # Needed for unnest function
  library(purrr)  # Needed for map function
  cat("  ✓ tidyr loaded\n")
  cat("  ✓ purrr loaded\n")
}
library(parallel)  # Built-in

# Try to load caret, but provide alternative if not available
caret_available <- require("caret", quietly = TRUE)
if (caret_available) {
  cat("  ✓ caret loaded\n")
} else {
  cat("  ⚠ caret not available - will use base R for train/test split\n")
}

cat("All required libraries loaded successfully.\n")

# ---- Configuration ----
# Parallelization settings
num_threads <- 8L  # Match vCPU count
available_cores <- parallel::detectCores()
cat(sprintf("Available CPU cores: %d\n", available_cores))
cat(sprintf("Using threads: %d\n", num_threads))

# Model parameters
num_trees <- 500L  # Number of trees in random forest (reduced from 1000 for memory efficiency)
train_test_split <- 0.8  # 80% training, 20% testing

# ---- Data Loading and Preparation ----
cat("\n=== Loading and preparing data ===\n")

# Directory with the CSVs
data_dir <- "AndMal2020-dynamic-BeforeAndAfterReboot"

# Check if directory exists
if (!dir.exists(data_dir)) {
  cat(sprintf("Warning: Data directory '%s' not found.\n", data_dir))
  cat("Attempting to find CSV files in parent directories...\n")
  # Try alternative paths
  alt_paths <- c(
    "AndMal2020-dynamic-BeforeAndAfterReboot",
    "../AndMal2020-dynamic-BeforeAndAfterReboot",
    file.path(dirname(getwd()), "AndMal2020-dynamic-BeforeAndAfterReboot")
  )
  for (alt_path in alt_paths) {
    if (dir.exists(alt_path)) {
      data_dir <- alt_path
      cat(sprintf("Found alternative path: %s\n", data_dir))
      break
    }
  }
}

files <- list.files(
  path    = data_dir,
  pattern = "_Cat\\.csv$",
  full.names = TRUE
)

if (length(files) == 0) {
  stop(sprintf("No CSV files found matching pattern '_Cat.csv' in directory '%s'. Please ensure the data directory exists and contains the required CSV files.", data_dir))
}

cat(sprintf("Found %d CSV files\n", length(files)))

# Build combined data frame
cat("Reading CSV files...\n")
andmal <- tibble(path = files) %>%
  mutate(
    file = basename(path),
    # Use a different name for the category inferred from the file name
    Category_file = str_replace(file, "_(before|after)_reboot_Cat\\.csv", ""),
    reboot_state = if_else(
      str_detect(file, "before_reboot"),
      "Before reboot",
      "After reboot"
    ),
    data = map(path, readr::read_csv, show_col_types = FALSE)
  ) %>%
  unnest(data) %>%  # no name conflict now
  mutate(
    # Category here is the column from the CSV (feature #143)
    Category      = factor(Category),
    Family        = factor(Family),
    Category_file = factor(Category_file),
    reboot_state  = factor(reboot_state,
                           levels = c("Before reboot", "After reboot"))
  )

# Split into two main dataframes
andmal_before <- andmal %>%
  filter(reboot_state == "Before reboot")

andmal_after <- andmal %>%
  filter(reboot_state == "After reboot")

# Remove combined dataframe to save memory
rm(andmal)
gc()

cat(sprintf("andmal_after: %d rows, %d columns\n", nrow(andmal_after), ncol(andmal_after)))

# ---- Feature Engineering (from proposal.qmd) ----
cat("\n=== Engineering features ===\n")

# Define API feature groups
ui_features <- c(
  "Memory_Views",
  "Memory_ViewRootImpl",
  "Memory_AppContexts",
  "Memory_Activities",
  "Memory_Assets",
  "Memory_AssetManagers",
  "Memory_LocalBinders",
  "Memory_ProxyBinders",
  "Memory_ParcelMemory",
  "Memory_ParcelCount",
  "Memory_DeathRecipients",
  "Memory_OpenSSLSockets",
  "Memory_WebViews"
)

dex_apis <- c(
  "API_DexClassLoader_dalvik.system.BaseDexClassLoader_findResource",
  "API_DexClassLoader_dalvik.system.BaseDexClassLoader_findResources",
  "API_DexClassLoader_dalvik.system.BaseDexClassLoader_findLibrary",
  "API_DexClassLoader_dalvik.system.DexFile_loadDex",
  "API_DexClassLoader_dalvik.system.DexFile_loadClass",
  "API_DexClassLoader_dalvik.system.DexClassLoader_.init"
)

webview_cols <- c(
  "API_WebView_android.webkit.WebView_loadUrl",
  "API_WebView_android.webkit.WebView_loadData",
  "API_WebView_android.webkit.WebView_loadDataWithBaseURL",
  "API_WebView_android.webkit.WebView_addJavascriptInterface",
  "API_WebView_android.webkit.WebView_evaluateJavascript",
  "API_WebView_android.webkit.WebView_postUrl",
  "API_WebView_android.webkit.WebView_postWebMessage",
  "API_WebView_android.webkit.WebView_savePassword",
  "API_WebView_android.webkit.WebView_setHttpAuthUsernamePassword",
  "API_WebView_android.webkit.WebView_getHttpAuthUsernamePassword",
  "API_WebView_android.webkit.WebView_setWebContentsDebuggingEnabled"
)

fileio_apis <- c(
  "API_FileIO_libcore.io.IoBridge_open",
  "API_FileIO_android.content.ContextWrapper_openFileInput",
  "API_FileIO_android.content.ContextWrapper_openFileOutput",
  "API_FileIO_android.content.ContextWrapper_deleteFile"
)

db_apis <- c(
  "API_Database_android.content.ContextWrapper_openOrCreateDatabase",
  "API_Database_android.content.ContextWrapper_databaseList",
  "API_Database_android.content.ContextWrapper_deleteDatabase",
  "API_Database_android.database.sqlite.SQLiteDatabase_execSQL",
  "API_Database_android.database.sqlite.SQLiteDatabase_deleteDatabase",
  "API_Database_android.database.sqlite.SQLiteDatabase_getPath",
  "API_Database_android.database.sqlite.SQLiteDatabase_insert",
  "API_Database_android.database.sqlite.SQLiteDatabase_insertOrThrow",
  "API_Database_android.database.sqlite.SQLiteDatabase_insertWithOnConflict",
  "API_Database_android.database.sqlite.SQLiteDatabase_openDatabase",
  "API_Database_android.database.sqlite.SQLiteDatabase_openOrCreateDatabase",
  "API_Database_android.database.sqlite.SQLiteDatabase_query",
  "API_Database_android.database.sqlite.SQLiteDatabase_queryWithFactory",
  "API_Database_android.database.sqlite.SQLiteDatabase_rawQuery",
  "API_Database_android.database.sqlite.SQLiteDatabase_rawQueryWithFactory",
  "API_Database_android.database.sqlite.SQLiteDatabase_update",
  "API_Database_android.database.sqlite.SQLiteDatabase_updateWithOnConflict",
  "API_Database_android.database.sqlite.SQLiteDatabase_compileStatement",
  "API_Database_android.database.sqlite.SQLiteDatabase_create"
)

db_read_apis <- c(
  "API_Database_android.database.sqlite.SQLiteDatabase_query",
  "API_Database_android.database.sqlite.SQLiteDatabase_queryWithFactory",
  "API_Database_android.database.sqlite.SQLiteDatabase_rawQuery",
  "API_Database_android.database.sqlite.SQLiteDatabase_rawQueryWithFactory"
)

db_write_apis <- c(
  "API_Database_android.database.sqlite.SQLiteDatabase_execSQL",
  "API_Database_android.database.sqlite.SQLiteDatabase_deleteDatabase",
  "API_Database_android.database.sqlite.SQLiteDatabase_insert",
  "API_Database_android.database.sqlite.SQLiteDatabase_insertOrThrow",
  "API_Database_android.database.sqlite.SQLiteDatabase_insertWithOnConflict",
  "API_Database_android.database.sqlite.SQLiteDatabase_update",
  "API_Database_android.database.sqlite.SQLiteDatabase_updateWithOnConflict",
  "API_Database_android.database.sqlite.SQLiteDatabase_create"
)

crypto_apis <- c(
  "API_Crypto_javax.crypto.spec.SecretKeySpec_.init",
  "API_Crypto_javax.crypto.Cipher_doFinal",
  "API_Crypto.Hash_java.security.MessageDigest_digest",
  "API_Crypto.Hash_java.security.MessageDigest_update"
)

base64_apis <- c(
  "API_Base64_android.util.Base64_decode",
  "API_Base64_android.util.Base64_encode",
  "API_Base64_android.util.Base64_encodeToString"
)

ipc_apis <- c(
  "API_IPC_android.content.ContextWrapper_sendBroadcast",
  "API_IPC_android.content.ContextWrapper_sendStickyBroadcast",
  "API_IPC_android.content.ContextWrapper_startActivity",
  "API_IPC_android.content.ContextWrapper_startService",
  "API_IPC_android.content.ContextWrapper_stopService",
  "API_IPC_android.content.ContextWrapper_registerReceiver"
)

id_apis <- c(
  "API_DeviceInfo_android.telephony.TelephonyManager_getDeviceId",
  "API_DeviceInfo_android.telephony.TelephonyManager_getSubscriberId",
  "API_DeviceInfo_android.telephony.TelephonyManager_getLine1Number",
  "API_DeviceInfo_android.telephony.TelephonyManager_getDeviceSoftwareVersion",
  "API_DeviceInfo_android.telephony.TelephonyManager_getSimSerialNumber"
)

wifi_apis <- c(
  "API_DeviceInfo_android.net.wifi.WifiInfo_getMacAddress",
  "API_DeviceInfo_android.net.wifi.WifiInfo_getBSSID",
  "API_DeviceInfo_android.net.wifi.WifiInfo_getIpAddress",
  "API_DeviceInfo_android.net.wifi.WifiInfo_getNetworkId"
)

accounts_apis <- c(
  "API_DeviceData_android.accounts.AccountManager_getAccountsByType",
  "API_DeviceData_android.accounts.AccountManager_getAccounts"
)

content_apis <- c(
  "API_DeviceData_android.content.ContentResolver_query",
  "API_DeviceData_android.content.ContentResolver_registerContentObserver",
  "API_DeviceData_android.content.ContentResolver_insert",
  "API_DeviceData_android.content.ContentResolver_delete"
)

location_apis <- c(
  "API_DeviceData_android.location.Location_getLatitude",
  "API_DeviceData_android.location.Location_getLongitude"
)

mic_apis <- c(
  "API_DeviceData_android.media.AudioRecord_startRecording",
  "API_DeviceData_android.media.MediaRecorder_start"
)

env_apis <- c(
  "API_DeviceInfo_android.content.pm.PackageManager_getInstallerPackageName",
  "API_DeviceInfo_android.content.pm.PackageManager_getInstalledApplications",
  "API_DeviceInfo_android.content.pm.PackageManager_getInstalledModules",
  "API_DeviceInfo_android.content.pm.PackageManager_getInstalledPackages",
  "API_DeviceData_android.app.ApplicationPackageManager_getInstalledPackages",
  "API_DeviceInfo_android.telephony.TelephonyManager_getNetworkOperator",
  "API_DeviceInfo_android.telephony.TelephonyManager_getNetworkOperatorName",
  "API_DeviceInfo_android.telephony.TelephonyManager_getSimOperatorName",
  "API_DeviceData_android.os.SystemProperties_get"
)

binder_apis <- c(
  "API_Binder_android.app.ContextImpl_registerReceiver",
  "API_Binder_android.app.ActivityThread_handleReceiver",
  "API_Binder_android.app.Activity_startActivity"
)

ipc_all <- c(ipc_apis, binder_apis)

# Helper function for safe rowSums (optimized for memory)
safe_rowSums <- function(data, cols) {
  cols <- intersect(cols, names(data))
  if (length(cols) == 0) {
    return(rep(0L, nrow(data)))
  } else {
    # Use as.matrix for better memory efficiency with rowSums
    return(rowSums(as.matrix(data[, cols, drop = FALSE]), na.rm = TRUE))
  }
}

# Apply feature engineering to andmal_after
cat("Adding derived features...\n")

# Memory features
cat("  Memory features...\n")
andmal_after <- andmal_after %>%
  mutate(
    heap_util = if_else(
      Memory_HeapSize > 0,
      Memory_HeapAlloc / Memory_HeapSize,
      NA_real_
    ),
    dirty_ratio = if_else(
      Memory_PssTotal > 0,
      (Memory_PrivateDirty + Memory_SharedDirty) / Memory_PssTotal,
      NA_real_
    ),
    shared_frac = if_else(
      (Memory_SharedClean + Memory_PrivateClean) > 0,
      Memory_SharedClean / (Memory_SharedClean + Memory_PrivateClean),
      NA_real_
    ),
    log_PssTotal      = log10(Memory_PssTotal + 1),
    log_Process_total = log10(Process_total + 1),
    log_API_sessions  = log10(API__sessions + 1)
  )
gc()  # Clean up after memory-intensive operations

# Webview features
cat("  Webview features...\n")
webview_cols_present <- webview_cols[webview_cols %in% names(andmal_after)]
andmal_after <- andmal_after %>%
  mutate(
    webview_calls = if (length(webview_cols_present) > 0) {
      rowSums(as.matrix(.[, webview_cols_present, drop = FALSE]), na.rm = TRUE)
    } else {
      0
    },
    log_WebView_calls = log10(webview_calls + 1),
    log_NetTxBytes    = log10(Network_TotalTransmittedBytes + 1),
    log_NetRxBytes    = log10(Network_TotalReceivedBytes + 1)
  )
gc()

# Database features
cat("  Database features...\n")
db_read_apis_present <- db_read_apis[db_read_apis %in% names(andmal_after)]
db_write_apis_present <- db_write_apis[db_write_apis %in% names(andmal_after)]
andmal_after <- andmal_after %>%
  mutate(
    total_DB_read_calls = if (length(db_read_apis_present) > 0) {
      rowSums(as.matrix(.[, db_read_apis_present, drop = FALSE]), na.rm = TRUE)
    } else {
      0
    },
    total_DB_write_calls = if (length(db_write_apis_present) > 0) {
      rowSums(as.matrix(.[, db_write_apis_present, drop = FALSE]), na.rm = TRUE)
    } else {
      0
    },
    log_DB_reads  = log10(total_DB_read_calls  + 1),
    log_DB_writes = log10(total_DB_write_calls + 1)
  )
gc()

# Dex features
cat("  Dex features...\n")
dex_apis_present <- dex_apis[dex_apis %in% names(andmal_after)]

andmal_after <- andmal_after %>%
  mutate(
    dex_any = if (length(dex_apis_present) == 0) {
      FALSE
    } else {
      rowSums(as.matrix(.[, dex_apis_present, drop = FALSE]), na.rm = TRUE) > 0
    }
  )
gc()

# Crypto features
cat("  Crypto features...\n")
crypto_apis_present  <- crypto_apis[crypto_apis %in% names(andmal_after)]
base64_apis_present  <- base64_apis[base64_apis %in% names(andmal_after)]

andmal_after <- andmal_after %>%
  mutate(
    crypto_calls = if (length(crypto_apis_present) == 0) {
      0
    } else {
      rowSums(as.matrix(.[, crypto_apis_present, drop = FALSE]), na.rm = TRUE)
    },
    base64_calls = if (length(base64_apis_present) == 0) {
      0
    } else {
      rowSums(as.matrix(.[, base64_apis_present, drop = FALSE]), na.rm = TRUE)
    },
    log_crypto  = log10(crypto_calls  + 1),
    log_base64  = log10(base64_calls  + 1)
  )
gc()

# IPC features
ipc_present <- ipc_all[ipc_all %in% names(andmal_after)]

andmal_after <- andmal_after %>%
  mutate(
    has_broadcast = safe_rowSums(
      .,
      c(
        "API_IPC_android.content.ContextWrapper_sendBroadcast",
        "API_IPC_android.content.ContextWrapper_sendStickyBroadcast"
      )
    ) > 0,
    has_service = safe_rowSums(
      .,
      c(
        "API_IPC_android.content.ContextWrapper_startService",
        "API_IPC_android.content.ContextWrapper_stopService"
      )
    ) > 0,
    has_receiver = safe_rowSums(
      .,
      c(
        "API_IPC_android.content.ContextWrapper_registerReceiver",
        "API_Binder_android.app.ContextImpl_registerReceiver",
        "API_Binder_android.app.ActivityThread_handleReceiver"
      )
    ) > 0,
    has_activity = safe_rowSums(
      .,
      c(
        "API_IPC_android.content.ContextWrapper_startActivity",
        "API_Binder_android.app.Activity_startActivity"
      )
    ) > 0
  )

# Privacy features
cat("  Privacy features...\n")
id_wifi_combined <- c(id_apis, wifi_apis)
accounts_content_combined <- c(accounts_apis, content_apis)
andmal_after <- andmal_after %>%
  mutate(
    id_calls       = safe_rowSums(andmal_after, id_wifi_combined),
    accounts_calls = safe_rowSums(andmal_after, accounts_content_combined),
    location_calls = safe_rowSums(andmal_after, location_apis),
    mic_calls      = safe_rowSums(andmal_after, mic_apis),
    env_calls      = safe_rowSums(andmal_after, env_apis),
    PII_access_count   = id_calls + accounts_calls + location_calls + mic_calls,
    env_probe_count    = env_calls,
    log_PII_access  = log10(PII_access_count + 1),
    log_env_probe   = log10(env_probe_count + 1),
    has_ids       = id_calls       > 0,
    has_accounts  = accounts_calls > 0,
    has_location  = location_calls > 0,
    has_mic       = mic_calls      > 0
  )
gc()

# Log features
cat("  Log features...\n")
andmal_after <- andmal_after %>%
  mutate(
    log_Logcat_total  = log10(Logcat_total  + 1)
  )

cat("Feature engineering complete.\n")
gc()  # Final cleanup before proceeding
cat(sprintf("Final dataset: %d rows, %d columns\n", nrow(andmal_after), ncol(andmal_after)))

# ---- Identify Feature Columns ----
cat("\n=== Identifying feature columns ===\n")

# Metadata columns to exclude from features
metadata_cols <- c("Category", "Family", "Category_file", "reboot_state", "path", "file")

# Get all column names
all_cols <- names(andmal_after)

# Identify metadata columns that actually exist
metadata_cols_present <- intersect(metadata_cols, all_cols)

# Feature columns for Model 1 (exclude Family and Category, plus other metadata)
feature_cols_model1 <- setdiff(all_cols, metadata_cols_present)

# Additional exclusion: any rank/color columns that might have been added
feature_cols_model1 <- feature_cols_model1[!str_detect(feature_cols_model1, "rank|color|fam_color|rank_in_cat")]

cat(sprintf("Model 1 features (Category prediction): %d features\n", length(feature_cols_model1)))
cat(sprintf("Excluded metadata columns: %s\n", paste(metadata_cols_present, collapse = ", ")))

# Feature columns for category-specific family models (same as Model 1, exclude Family)
# Note: We don't include Category as a feature since we filter by category first
feature_cols_family <- setdiff(feature_cols_model1, "Family")

cat(sprintf("Family prediction features (category-specific): %d features\n", length(feature_cols_family)))

# ---- Train/Test Split ----
cat("\n=== Creating train/test split ===\n")

# Check class distribution
cat("Category distribution:\n")
print(table(andmal_after$Category))
cat("\nFamily distribution:\n")
print(table(andmal_after$Family))

# Create stratified split for Category
set.seed(42)
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
  # Use base R stratified sampling
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

cat(sprintf("Training set: %d rows (%.1f%%)\n", nrow(train_data), 100 * nrow(train_data) / nrow(andmal_after)))
cat(sprintf("Test set: %d rows (%.1f%%)\n", nrow(test_data), 100 * nrow(test_data) / nrow(andmal_after)))

# ---- Model 1: Category Prediction ----
cat("\n=== Training Model 1: Category Prediction ===\n")

# Prepare data for Model 1
train_model1_x <- train_data[, feature_cols_model1, drop = FALSE]
train_model1_y <- train_data$Category
test_model1_x <- test_data[, feature_cols_model1, drop = FALSE]
test_model1_y <- test_data$Category

# Calculate mtry (default: sqrt of number of features)
mtry_model1 <- floor(sqrt(length(feature_cols_model1)))
cat(sprintf("Model 1 parameters:\n"))
cat(sprintf("  Number of trees: %d\n", num_trees))
cat(sprintf("  mtry: %d (sqrt of %d features)\n", mtry_model1, length(feature_cols_model1)))
cat(sprintf("  Number of threads: %d\n", num_threads))

# Train Model 1
cat("Training random forest for Category prediction...\n")
start_time <- Sys.time()

rf_model1 <- ranger(
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
training_time_model1 <- difftime(end_time, start_time, units = "mins")
cat(sprintf("Model 1 training completed in %.2f minutes\n", as.numeric(training_time_model1)))

# Evaluate Model 1
cat("\nEvaluating Model 1...\n")
pred_model1 <- predict(rf_model1, test_model1_x)
pred_categories <- pred_model1$predictions

# Get predicted class (highest probability)
pred_categories_class <- colnames(pred_categories)[apply(pred_categories, 1, which.max)]
pred_categories_class <- factor(pred_categories_class, levels = levels(test_model1_y))

# Confusion matrix
if (caret_available) {
  cm_model1 <- caret::confusionMatrix(pred_categories_class, test_model1_y)
  cat("\nModel 1 - Category Prediction - Confusion Matrix:\n")
  print(cm_model1)
  
  # Calculate per-class metrics
  metrics_model1 <- cm_model1$overall
  per_class_model1 <- cm_model1$byClass
  
  cat("\nModel 1 - Overall Accuracy:", metrics_model1["Accuracy"], "\n")
} else {
  # Base R confusion matrix and metrics
  cm_table1 <- table(Predicted = pred_categories_class, Actual = test_model1$Category)
  cat("\nModel 1 - Category Prediction - Confusion Matrix:\n")
  print(cm_table1)
  
  accuracy1 <- sum(diag(cm_table1)) / sum(cm_table1)
  cat("\nModel 1 - Overall Accuracy:", accuracy1, "\n")
  
  # Store for saving
  metrics_model1 <- list(Accuracy = accuracy1)
  per_class_model1 <- NULL
  cm_model1 <- list(table = cm_table1, overall = metrics_model1)
}

# ---- Models 2a, 2b, 2c: Category-Specific Family Prediction ----
cat("\n=== Training Category-Specific Family Prediction Models ===\n")

# Categories to model
target_categories <- c("Adware", "Riskware", "Trojan")

# Storage for models and results
category_models <- list()
category_metrics <- list()
category_training_times <- list()
category_cm <- list()

# Calculate mtry for family models
mtry_family <- floor(sqrt(length(feature_cols_family)))

cat(sprintf("Family prediction parameters:\n"))
cat(sprintf("  Number of trees: %d\n", num_trees))
cat(sprintf("  mtry: %d (sqrt of %d features)\n", mtry_family, length(feature_cols_family)))
cat(sprintf("  Number of threads: %d\n", num_threads))

# Train a model for each category
for (category in target_categories) {
  cat(sprintf("\n--- Training Model 2%s: %s Family Prediction ---\n", 
              letters[which(target_categories == category)], category))
  
  # Filter data for this category
  category_data <- andmal_after %>%
    filter(Category == category)
  
  if (nrow(category_data) == 0) {
    cat(sprintf("  WARNING: No samples found for category '%s'. Skipping.\n", category))
    next
  }
  
  # Check family distribution
  family_dist <- table(category_data$Family)
  cat(sprintf("  Samples in %s category: %d\n", category, nrow(category_data)))
  cat(sprintf("  Number of families: %d\n", length(unique(category_data$Family))))
  cat(sprintf("  Families with samples: %s\n", 
              paste(names(family_dist[family_dist > 0]), collapse = ", ")))
  
  # Skip if too few families or samples
  if (length(unique(category_data$Family)) < 2) {
    cat(sprintf("  WARNING: Category '%s' has fewer than 2 families. Skipping.\n", category))
    next
  }
  
  if (nrow(category_data) < 50) {
    cat(sprintf("  WARNING: Category '%s' has fewer than 50 samples. Skipping.\n", category))
    next
  }
  
  # Create train/test split stratified by Family
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
    # Base R stratified sampling by Family
    families <- unique(category_data$Family)
    train_indices <- c()
    for (fam in families) {
      fam_indices <- which(category_data$Family == fam)
      if (length(fam_indices) > 1) {
        n_train <- max(1, round(length(fam_indices) * train_test_split))
        train_fam_indices <- sample(fam_indices, n_train)
        train_indices <- c(train_indices, train_fam_indices)
      } else {
        # If only one sample, put it in training
        train_indices <- c(train_indices, fam_indices)
      }
    }
    category_train <- category_data[train_indices, ]
    category_test <- category_data[-train_indices, ]
  }
  
  cat(sprintf("  Training set: %d samples\n", nrow(category_train)))
  cat(sprintf("  Test set: %d samples\n", nrow(category_test)))
  
  # Prepare data for training
  train_family_x <- category_train[, feature_cols_family, drop = FALSE]
  train_family_y <- category_train$Family
  test_family_x <- category_test[, feature_cols_family, drop = FALSE]
  test_family_y <- category_test$Family
  
  # Ensure Family factor levels match
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
    verbose = FALSE  # Less verbose for multiple models
  )
  
  end_time <- Sys.time()
  training_time <- difftime(end_time, start_time, units = "mins")
  category_training_times[[category]] <- training_time
  cat(sprintf("  Training completed in %.2f minutes\n", as.numeric(training_time)))
  
  # Evaluate model
  cat(sprintf("  Evaluating %s family prediction model...\n", category))
  pred_family <- predict(rf_family, test_family_x)
  pred_families <- pred_family$predictions
  
  # Get predicted class
  pred_families_class <- colnames(pred_families)[apply(pred_families, 1, which.max)]
  pred_families_class <- factor(pred_families_class, levels = levels(test_family_y))
  
  # Confusion matrix and metrics
  if (caret_available) {
    cm_family <- caret::confusionMatrix(pred_families_class, test_family_y)
    cat(sprintf("  %s Family Prediction - Accuracy: %.4f\n", 
                category, cm_family$overall["Accuracy"]))
    
    metrics_family <- cm_family$overall
    per_class_family <- cm_family$byClass
    category_cm[[category]] <- cm_family
  } else {
    # Base R confusion matrix
    cm_table_family <- table(Predicted = pred_families_class, Actual = test_family_y)
    accuracy_family <- sum(diag(cm_table_family)) / sum(cm_table_family)
    cat(sprintf("  %s Family Prediction - Accuracy: %.4f\n", category, accuracy_family))
    
    metrics_family <- list(Accuracy = accuracy_family)
    per_class_family <- NULL
    category_cm[[category]] <- list(table = cm_table_family, overall = metrics_family)
  }
  
  # Store model and metrics
  category_models[[category]] <- rf_family
  category_metrics[[category]] <- list(
    overall = metrics_family,
    per_class = per_class_family
  )
  
  # Cleanup intermediate variables
  rm(category_data, category_train, category_test, train_family_x, train_family_y, 
     test_family_x, test_family_y, pred_family, pred_families, pred_families_class)
  gc()
}

# ---- Save Models and Results ----
cat("\n=== Saving models and results ===\n")

# Save models
saveRDS(rf_model1, "rf_category_model.rds")
cat("Saved: rf_category_model.rds\n")

# Save category-specific family models
for (category in names(category_models)) {
  model_name <- paste0("rf_family_", tolower(category), "_model.rds")
  saveRDS(category_models[[category]], model_name)
  cat(sprintf("Saved: %s\n", model_name))
}

# Save evaluation results to text file
sink("model_evaluation_results.txt")
cat("Random Forest Models - Evaluation Results\n")
cat("==========================================\n\n")

cat("MODEL 1: Category Prediction\n")
cat("----------------------------\n")
cat("Training time:", as.numeric(training_time_model1), "minutes\n")
cat("\nOverall Metrics:\n")
if (caret_available) {
  print(metrics_model1)
  cat("\nPer-Class Metrics:\n")
  print(per_class_model1)
} else {
  cat("Accuracy:", metrics_model1$Accuracy, "\n")
}
cat("\nConfusion Matrix:\n")
print(cm_model1$table)

cat("\n\nMODELS 2a-2c: Category-Specific Family Prediction\n")
cat("==================================================\n")

for (category in names(category_models)) {
  cat(sprintf("\nModel 2%s: %s Family Prediction\n", 
              letters[which(target_categories == category)], category))
  cat("----------------------------------------\n")
  cat("Training time:", as.numeric(category_training_times[[category]]), "minutes\n")
  
  metrics_fam <- category_metrics[[category]]$overall
  per_class_fam <- category_metrics[[category]]$per_class
  
  cat("\nOverall Metrics:\n")
  if (caret_available && !is.null(metrics_fam)) {
    if (is.list(metrics_fam)) {
      print(metrics_fam)
    } else {
      cat("Accuracy:", metrics_fam["Accuracy"], "\n")
    }
    if (!is.null(per_class_fam)) {
      cat("\nPer-Class Metrics:\n")
      print(per_class_fam)
    }
  } else {
    cat("Accuracy:", metrics_fam$Accuracy, "\n")
  }
  
  cat("\nConfusion Matrix:\n")
  if (caret_available && !is.null(category_cm[[category]]$table)) {
    print(category_cm[[category]]$table)
  } else {
    print(category_cm[[category]]$table)
  }
}

cat("\n\nData Summary\n")
cat("------------\n")
cat("Total samples:", nrow(andmal_after), "\n")
cat("Training samples:", nrow(train_data), "\n")
cat("Test samples:", nrow(test_data), "\n")
cat("Model 1 features:", length(feature_cols_model1), "\n")
cat("Family prediction features:", length(feature_cols_family), "\n")
cat("Number of trees per model:", num_trees, "\n")
cat("Threads used:", num_threads, "\n")
cat("\nCategory-specific family models created:\n")
for (category in names(category_models)) {
  cat(sprintf("  - %s: %d families predicted\n", category, 
              length(unique(category_models[[category]]$forest$levels))))
}

sink()

cat("Saved: model_evaluation_results.txt\n")

# ---- Cleanup ----
cat("\n=== Cleaning up ===\n")
gc()

cat("\n=== Script completed successfully! ===\n")
cat("Models saved:\n")
cat("  - rf_category_model.rds\n")
for (category in names(category_models)) {
  model_name <- paste0("rf_family_", tolower(category), "_model.rds")
  cat(sprintf("  - %s\n", model_name))
}
cat("Results saved:\n")
cat("  - model_evaluation_results.txt\n")

