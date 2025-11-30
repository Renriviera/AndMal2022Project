### Preliminary Analysis:
# ---- Install (first run only) ----

pkgs <- c("duckdb","DBI","arrow","janitor","Matrix","qs")
install.packages(pkgs, type = "binary")




library(DBI)
library(duckdb)
library(arrow)
library(janitor)
library(Matrix)
library(qs)

# ---------- Paths & knobs ----------
csv_dir      <- "CCCS-CIC-Benign-CSVs"
csv_files    <- file.path(csv_dir, sprintf("Ben%d.csv", 0:4))
parquet_dir  <- file.path(csv_dir, "parquet")   # output directory for Parquet
dir.create(parquet_dir, showWarnings = FALSE, recursive = TRUE)

# DuckDB / Arrow threads
duckdb_threads <- max(2L, parallel::detectCores() - 1L)
Sys.setenv(ARROW_NUM_THREADS = duckdb_threads)

# Chunk size for sparse build (rows per fetch from DuckDB)
chunk_nrows  <- 2000   # adjust up/down based on RAM (wide data => keep moderate)

# Optionally drop tiny magnitudes to zero for extra sparsity (set to 0 to disable)
zero_eps     <- 0

# ---------- Stage A: CSV -> Parquet (1 Parquet per CSV) ----------
con <- dbConnect(duckdb())
dbExecute(con, sprintf("PRAGMA threads=%d;", duckdb_threads))

for (csv in csv_files) {
  stopifnot(file.exists(csv))
  pq_out <- file.path(parquet_dir, paste0(tools::file_path_sans_ext(basename(csv)), ".parquet"))
  message("Converting: ", basename(csv), "  -->  ", basename(pq_out))
  # Use DuckDB's fast CSV reader & Parquet writer (full-scan inference)
  dbExecute(con, sprintf("
    COPY (
      SELECT * FROM read_csv_auto('%s', sample_size=-1)
    ) TO '%s' (FORMAT 'parquet');
  ", normalizePath(csv, winslash = "/"), normalizePath(pq_out, winslash = "/")))
}

dbDisconnect(con, shutdown = TRUE)