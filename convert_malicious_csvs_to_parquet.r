# convert_malicious_csvs_to_parquet_robust.R
# CSV -> Parquet for all CSVs in CCCS-CIC-Malicious-CSVs/, with explicit dialect.
# Handles: no header, delim=",", no quotes, trailing commas (pads with NULLs).

# ---- Packages ----
pkgs <- c("duckdb","DBI")
mis <- setdiff(pkgs, rownames(installed.packages()))
if (length(mis)) install.packages(mis, type = "binary")
library(DBI)
library(duckdb)

# ---- Config ----
csv_dir <- "CCCS-CIC-Malicious-CSVs"
# Convert every *.csv in the folder (safer than a hardcoded list)
csv_files <- list.files(csv_dir, pattern = "\\.csv$", full.names = TRUE)
stopifnot(length(csv_files) > 0)

parquet_dir <- file.path(csv_dir, "parquet")
dir.create(parquet_dir, showWarnings = FALSE, recursive = TRUE)

threads <- max(2L, parallel::detectCores() - 1L)

# ---- Connect ----
con <- dbConnect(duckdb())
dbExecute(con, sprintf("PRAGMA threads=%d;", threads))

# Helper: run a single conversion with explicit CSV options
convert_one <- function(in_csv, out_parquet) {
  sql <- sprintf("
    COPY (
      SELECT * FROM read_csv_auto(
        '%s',
        sample_size = -1,
        -- Explicit dialect to match CIC MalDroid CSVs:
        header = false,
        delim = ',',
        quote = '',           -- no quotes
        escape = '',          -- no escapes
        null_padding = true,  -- pad trailing empty fields (due to trailing comma)
        strict_mode = false,  -- don't choke on minor irregularities
        ignore_errors = true, -- skip malformed rows if encountered
        all_varchar = true,   -- read everything as text; downstream can cast
        max_line_size = 10000000
      )
    ) TO '%s' (FORMAT 'parquet');
  ",
                 normalizePath(in_csv, winslash = "/", mustWork = TRUE),
                 normalizePath(out_parquet, winslash = "/", mustWork = FALSE)
  )
  dbExecute(con, sql)
}

# ---- Convert all ----
for (in_csv in csv_files) {
  out_parquet <- file.path(
    parquet_dir,
    sub("\\.csv$", ".parquet", basename(in_csv), ignore.case = TRUE)
  )
  message(sprintf("Converting %s  -->  %s",
                  basename(in_csv), basename(out_parquet)))
  tryCatch(
    convert_one(in_csv, out_parquet),
    error = function(e) {
      message("  Primary read failed; trying a very forgiving fallback...")
      # Fallback: same settings but let DuckDB parse lines even more leniently.
      sql_fallback <- sprintf("
        COPY (
          SELECT * FROM read_csv_auto(
            '%s',
            sample_size = -1,
            header = false,
            delim = ',',
            quote = '',
            escape = '',
            null_padding = true,
            strict_mode = false,
            ignore_errors = true,
            all_varchar = true,
            max_line_size = 10000000
          )
        ) TO '%s' (FORMAT 'parquet');
      ",
                              normalizePath(in_csv, winslash = "/", mustWork = TRUE),
                              normalizePath(out_parquet, winslash = "/", mustWork = FALSE)
      )
      dbExecute(con, sql_fallback)
    }
  )
}

# ---- Optional: quick verification (row counts) ----
message("Verifying Parquet row counts...")
for (pq in list.files(parquet_dir, pattern="\\.parquet$", full.names = TRUE)) {
  n <- dbGetQuery(con, sprintf(
    "SELECT COUNT(*) AS n FROM read_parquet('%s');",
    normalizePath(pq, winslash = "/", mustWork = TRUE)
  ))$n
  message(sprintf("%-20s rows: %s", basename(pq), format(n, big.mark=",")))
}

dbDisconnect(con, shutdown = TRUE)
message("Done. Parquet files written to: ", normalizePath(parquet_dir))
