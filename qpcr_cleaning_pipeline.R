# ============================================================
#  qPCR Data Cleaning Pipeline
#  MIQE-aligned workflow
#
#  PURPOSE:
#    Reads one or more qPCR plate CSV files, applies a
#    standardised cleaning and flagging workflow, and writes:
#      - <stem>_all_samples.csv    : every row, with QC columns appended
#      - <stem>_review_samples.csv : only rows that need manual review
#      - audit/pcr_decisions.csv   : append-log of every QC decision made
#      - audit/pcr_variables.csv   : append-log of parameters used per run
#
#  USAGE:
#    1. Edit Section 1 (Configuration) to match your paths and thresholds.
#    2. Edit Section 2 (LOD Definitions) to match your targets.
#    3. Run the whole script (source() or Ctrl+Shift+Enter in RStudio).
#
#  DEPENDENCIES:
#    install.packages("tidyverse")   # tidyverse >= 2.0 required
# ============================================================


# ============================================================
# SECTION 1: Configuration
# ============================================================
# All user-facing settings live here. You should rarely need
# to edit anything outside Sections 1 and 2.

# --- Input ---
INPUT_DIR    <- "data_raw"   # Folder containing plate CSV files.
FILE_PATTERN <- "\\.csv$"    # Regex: which files in INPUT_DIR to include.
FILES        <- NULL         # Optional: explicit character vector of file paths.
# If set, INPUT_DIR and FILE_PATTERN are ignored.
# e.g. FILES <- c("data_raw/plate1.csv", "data_raw/plate2.csv")

# --- Output ---
OUTPUT_DIR   <- "outputs"                  # Folder for cleaned CSVs.
DEC_LOG_PATH <- "audit/pcr_decisions.csv"  # Audit log: one row per QC decision.
VAR_LOG_PATH <- "audit/pcr_variables.csv"  # Audit log: parameters used per run.

# --- QC Thresholds ---
DELTA_CQ_THRESHOLD <- 1.0  # Max acceptable |Cq replicate difference|.
# Pairs exceeding this are flagged for review.

# --- Optional: Remove rows whose sample/content values match a pattern ---
# Any row where a value in RM_COLUMNS matches any pattern in RM_PATTERNS
# will be removed. Matching is case-insensitive.
RM_PATTERNS <- c("OM", "Std", "NTC", "Neg")  # Regex patterns to match.
RM_COLUMNS  <- c("sample")                    # Which columns to search in.

# Set to TRUE to print a table of matched rows before removing them.
ENABLE_PREVIEW <- FALSE

# Set to TRUE to skip the actual removal step (log only — useful for testing).
DRY_RUN <- FALSE

# Set to TRUE to print intermediate data frames to the console (for debugging).
DEBUG_PRINT <- FALSE


# ============================================================
# SECTION 2: LOD (Limit of Detection) Definitions
# ============================================================
# Define upper (Hi) and lower (Lo) SQ limits for each target.
# Keys must match the target names in your CSV files (case-insensitive).
#
# If all targets share the same limits you can simplify by setting
# LOD_Lo and LOD_Hi as single scalars in process_plate() directly,
# but the list approach here allows per-target flexibility.

TARGET_LOD <- list(
  LOD_Hi = list(
    fucp      = 2000,
    hpd3      = 2000,
    lyta      = 2000,
    copb      = 2000,
    speb      = 2000,
    nuc       = 2000,
    gyrb      = 2000,
    uni       = 200,
    universal = 200
  ),
  LOD_Lo = list(
    fucp      = 0.012,
    hpd3      = 0.012,
    lyta      = 0.012,
    copb      = 0.012,
    speb      = 0.012,
    nuc       = 0.012,
    gyrb      = 0.012,
    uni       = 0.0012,
    universal = 0.0012
  )
)


# ============================================================
# SECTION 3: Library Import
# ============================================================
# tidyverse covers: dplyr, tidyr, stringr, purrr, readr, lubridate (>= 2.0)

library(tidyverse)
options(dplyr.show_progress = FALSE)

# ============================================================
# SECTION 4: Internal Utility Helpers
# ============================================================
# Small replacements for single-use external package functions,
# keeping the dependency list to just tidyverse.

# Replacement for tools::file_path_sans_ext()
# Returns the filename without its extension.
file_stem <- function(path) {
  sub("\\.[^.]*$", "", basename(path))
}

# Replacement for janitor::clean_names()
# Lowercases column names and replaces runs of non-alphanumeric characters
# with underscores, trimming leading/trailing underscores.
clean_col_names <- function(df) {
  names(df) <- names(df) |>
    tolower() |>
    trimws() |>
    (\(x) gsub("[^a-z0-9]+", "_", x))() |>
    (\(x) gsub("^_+|_+$",    "",  x))()
  df
}

# Wrapper around write_csv() with automatic retry on failure.
#
# Network-attached storage (e.g. mapped drives, SMB shares) can
# intermittently refuse a write due to locking or connectivity
# blips. This wrapper catches those errors, waits a short interval,
# and retries up to `max_tries` times before giving up.
#
# Args:
#   x         : data frame to write
#   file      : destination path
#   append    : passed through to write_csv (default FALSE)
#   max_tries : maximum number of attempts (default 5)
#   wait_secs : seconds to wait between attempts (default 3)
#   ...       : any other arguments passed through to write_csv
write_csv_retry <- function(x, file, append = FALSE,
                            max_tries = 5, wait_secs = 3, ...) {
  attempt <- 0
  repeat {
    attempt <- attempt + 1
    result  <- tryCatch(
      {
        write_csv(x, file, append = append, ...)
        "ok"
      },
      error = function(e) e
    )

    if (identical(result, "ok")) break

    if (attempt >= max_tries) {
      stop(sprintf(
        "write_csv_retry: failed after %d attempt(s) writing to:\n  %s\nLast error: %s",
        attempt, file, conditionMessage(result)
      ))
    }

    cat(sprintf(
      "  [write retry %d/%d] Could not write to '%s'. Waiting %ds...\n",
      attempt, max_tries, basename(file), wait_secs
    ))
    Sys.sleep(wait_secs)
  }
  invisible(x)
}


# ============================================================
# SECTION 5: Progress Reporting Helpers
# ============================================================
# Provides a simple, consistent progress display in the console.
# Each plate prints a header and then one line per pipeline step.
#
# Example output:
#
#   [1/3] plate_A.csv ════════════════════════════════════════
#     Import          │ found header at row 18 │ 220 data rows
#     LOD resolution  │ targets: fucp, hpd3, lyta
#   ▶ Step 0 │ Removing unnamed samples ...
#     Step 0 │ done │ 2 removed
#   ▶ Step 1 │ SQ adjustment ...
#     Step 1 │ done │ 14 adjusted
#     ...
#   ──────────────────────────────────────────────────────────
#     Output │ 204 rows out │ 6 for review
#

.pg_width <- 60  # Console ruler width for plate header lines

# Print the plate header banner.
pg_plate <- function(i, n_total, filename) {
  label  <- sprintf("[%d/%d] %s ", i, n_total, basename(filename))
  dashes <- strrep("\u2550", max(0, .pg_width - nchar(label)))
  cat("\n", label, dashes, "\n", sep = "")
}

# Print a one-line informational message (e.g. after import or LOD check).
pg_info <- function(label, detail) {
  cat(sprintf("  %-16s\u2502 %s\n", label, detail))
}

# Print the "starting step X" line.
pg_step_start <- function(step_num, step_name) {
  cat(sprintf("> Step %d \u2502 %s ...\n", step_num, step_name))
}

# Print the "step X done" line with an optional result summary.
pg_step_done <- function(step_num, detail = NULL) {
  if (is.null(detail)) {
    cat(sprintf("  Step %d \u2502 done\n", step_num))
  } else {
    cat(sprintf("  Step %d \u2502 done \u2502 %s\n", step_num, detail))
  }
}

# Print the final summary line at the end of a plate.
pg_summary <- function(n_out, n_review) {
  cat(strrep("\u2500", .pg_width), "\n", sep = "")
  cat(sprintf("  Output  \u2502 %d rows out \u2502 %d for review\n", n_out, n_review))
}


# ============================================================
# SECTION 6: Logging Helpers
# ============================================================
# Two append-only audit logs:
#   - Decision log : one row per QC action taken on a sample/target
#   - Variable log : one row per parameter in effect for each run
#
# Both are created with a typed header on first use and appended
# to on subsequent runs, giving a full cross-session history.

# --- Internal: resolve the current input file name ---
# process_plate() sets .current_input_file in the global env so that
# all logging calls within a plate run record the correct source file.
.resolve_input_file <- function(default_file) {
  val <- get0(".current_input_file", envir = .GlobalEnv, inherits = FALSE)
  if (is.null(val)) basename(default_file) else basename(val)
}

# --- Decision log ---

# Creates the decision log CSV with typed headers if it does not exist.
ensure_dec_log <- function(path) {
  if (!dir.exists(dirname(path))) dir.create(dirname(path), recursive = TRUE)
  if (!file.exists(path)) {
    write_csv_retry(
      tibble(
        timestamp  = as_datetime(character()),
        user       = character(),
        run_id     = character(),
        input_file = character(),
        sample_id  = character(),
        target     = character(),
        rule_id    = character(),
        outcome    = character(),
        evidence   = character(),
        source     = character()
      ),
      path
    )
  }
  invisible(path)
}

# Appends a single decision row to the log.
#
# Args:
#   sample_id    : sample identifier (NA for plate-level decisions)
#   target       : target/assay name (NA for plate-level decisions)
#   rule_id      : short code for the QC rule, e.g. "RV_DELTA_CQ"
#   outcome      : "applied" | "skipped" | "pass" | "preview" | "dry_run"
#   evidence     : human-readable string explaining why the rule fired
#   run_id       : unique ID for this plate run
#   dec_log_path : path to the decision log CSV
#   default_file : fallback file name if .current_input_file is not set
log_decision <- function(sample_id, target, rule_id, outcome, evidence,
                         run_id, dec_log_path, default_file) {
  ensure_dec_log(dec_log_path)
  entry <- tibble(
    timestamp  = now(tzone = "UTC"),
    user       = Sys.info()[["user"]],
    run_id     = run_id,
    input_file = .resolve_input_file(default_file),
    sample_id  = as.character(sample_id %||% NA_character_),
    target     = as.character(target     %||% NA_character_),
    rule_id    = rule_id,
    outcome    = outcome,
    evidence   = evidence,
    source     = "R_script",
    version    = "0.1.2"
  )
  write_csv_retry(entry, dec_log_path, append = TRUE)
  invisible(entry)
}

# --- Variable log ---

# Creates the variable log CSV with typed headers if it does not exist.
ensure_var_log <- function(path) {
  if (!dir.exists(dirname(path))) dir.create(dirname(path), recursive = TRUE)
  if (!file.exists(path)) {
    write_csv_retry(
      tibble(
        timestamp  = as_datetime(character()),
        user       = character(),
        run_id     = character(),
        input_file = character(),
        sample_id  = character(),
        target     = character(),
        var_name   = character(),
        var_value  = character(),
        var_class  = character(),
        source     = character()
      ),
      path
    )
  }
  invisible(path)
}

# Converts any R object to a compact string for storage in the variable log.
# Uses jsonlite if available (recommended), falling back to base R otherwise.
.serialize_value <- function(x, max_chars = 4000) {
  cls <- paste(class(x), collapse = "/")

  if (requireNamespace("jsonlite", quietly = TRUE)) {
    val <- tryCatch(
      as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "string")),
      error = function(e) NA_character_
    )
    if (!is.na(val)) {
      if (nchar(val) > max_chars) val <- paste0(substr(val, 1, max_chars), "... <truncated>")
      return(list(value = val, class = cls))
    }
  }

  # Base-R fallback
  val_chr <- tryCatch({
    if (is.atomic(x)) {
      paste(sprintf("%s", x), collapse = ", ")
    } else if (is.data.frame(x)) {
      paste(utils::capture.output(utils::head(x, 5)), collapse = "\n")
    } else if (is.list(x)) {
      nm    <- names(x) %||% seq_along(x)
      items <- paste0(nm, "=", vapply(x, function(e) {
        paste(utils::head(
          tryCatch(as.character(e), error = function(e) "<non-coercible>"), 3),
          collapse = "|")
      }, character(1)))
      paste(items, collapse = ", ")
    } else {
      paste(utils::capture.output(utils::str(x, max.level = 1)), collapse = "\n")
    }
  }, error = function(e) "<unserializable>")

  if (nchar(val_chr) > max_chars) val_chr <- paste0(substr(val_chr, 1, max_chars), "... <truncated>")
  list(value = val_chr, class = cls)
}

# Appends one row per variable in `vars` to the variable log.
#
# Args:
#   vars         : named list, e.g. list(threshold = 1.0, targets = c("fucp"))
#   run_id       : unique ID for this plate run
#   var_log_path : path to the variable log CSV
#   default_file : fallback file name if .current_input_file is not set
#   sample_id    : optional — links the entry to a specific sample
#   target       : optional — links the entry to a specific target
log_variables <- function(vars, run_id, var_log_path, default_file,
                          sample_id = NULL, target = NULL) {
  if (is.null(vars) || length(vars) == 0) return(invisible(tibble()))
  if (is.null(names(vars)) || any(is.na(names(vars)) | names(vars) == "")) {
    stop("`vars` must be a named list or named vector with non-empty names.")
  }

  ensure_var_log(var_log_path)

  ser        <- lapply(vars, .serialize_value)
  var_values <- vapply(ser, function(s) s$value, character(1))
  var_class  <- vapply(ser, function(s) s$class,  character(1))

  entry <- tibble(
    timestamp  = now(tzone = "UTC"),
    user       = Sys.info()[["user"]],
    run_id     = run_id,
    input_file = .resolve_input_file(default_file),
    sample_id  = as.character(sample_id %||% NA_character_),
    target     = as.character(target     %||% NA_character_),
    var_name   = names(vars),
    var_value  = var_values,
    var_class  = var_class,
    source     = "R_script",
    version    = "0.1.0"
  )

  write_csv_retry(entry, var_log_path, append = TRUE)
  invisible(entry)
}

# [UTILITY — not called in main pipeline, available for future use]
# Convenience wrapper: logs selected columns from a single-row data frame.
log_variables_from_df <- function(df, cols, run_id, var_log_path,
                                  default_file, sample_id = NULL, target = NULL) {
  stopifnot(is.data.frame(df), all(cols %in% names(df)), nrow(df) == 1)
  vars <- as.list(df[1, cols, drop = FALSE])
  names(vars) <- cols
  log_variables(
    vars         = vars,
    run_id       = run_id,
    var_log_path = var_log_path,
    default_file = default_file,
    sample_id    = sample_id,
    target       = target
  )
}


# ============================================================
# SECTION 7: Core Processing Function — process_plate()
# ============================================================
# Runs the full cleaning workflow on a single plate CSV file.
#
# Workflow:
#   Import  — Locate the "Well" header row dynamically and read from there
#   LOD     — Resolve per-target or scalar LOD bounds
#   Step 0  — Remove rows with no sample name
#   Step 1  — Adjust SQ values below LOD_Lo to LOD_Lo / 2
#   Step 2  — Optionally remove rows matching regex patterns
#   Step 3  — Flag sample-name mismatches within (Fluor, Target, Content)
#   Step 4  — Compute per-replicate-group stats (delta Cq, average SQ, flags)
#   Step 5  — Assemble review flags and write output CSVs
#
# Args:
#   file           : path to the plate CSV
#   LOD_List       : named list with $LOD_Hi and $LOD_Lo (per-target, preferred)
#   LOD_Lo / Hi    : scalar fallbacks used only when LOD_List is NULL
#   dCq_thr        : delta-Cq threshold above which a replicate pair is flagged
#   rm_patterns    : character vector of regex patterns for row removal
#   rm_cols_req    : column names to apply rm_patterns against
#   plate_index    : integer position of this plate in the run (for progress display)
#   n_plates       : total number of plates in the run (for progress display)
#   enable_preview, dry_run, debug_print : see Section 1
#   output_dir, dec_log_path, var_log_path : see Section 1
#
# Returns: a named list summarising the run (paths, row counts, flags)

process_plate <- function(file,
                          LOD_Lo = NULL, LOD_Hi = NULL, LOD_List = NULL,
                          dCq_thr,
                          rm_patterns, rm_cols_req,
                          plate_index, n_plates,
                          enable_preview, dry_run, debug_print,
                          output_dir, dec_log_path, var_log_path) {

  # Register the current file globally so logging helpers can pick it up
  assign(".current_input_file", file, envir = .GlobalEnv)

  stem   <- file_stem(file)
  run_id <- paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_", stem)

  pg_plate(plate_index, n_plates, file)

  # ----------------------------------------------------------
  # Import
  # Rather than hardcoding a skip row, we read the whole file
  # and locate the first row where column 1 contains "Well".
  # Everything from that row onward becomes the data table.
  # ----------------------------------------------------------
  pg_step_start(0, "Import — locating header row")

  raw_full  <- read_csv(file, show_col_types = FALSE, col_names = FALSE)
  well_row  <- which(raw_full[[1]] == "Well")

  if (length(well_row) == 0) {
    stop("Could not find a row with 'Well' in column 1 of: ", file,
         "\nCheck that the file is a valid CFX Manager export.")
  }

  well_row       <- well_row[1]
  col_headers    <- as.character(raw_full[well_row, ])
  raw            <- raw_full[(well_row + 1):nrow(raw_full), ]
  colnames(raw)  <- col_headers
  raw            <- clean_col_names(raw)   # normalise: lowercase, underscores

  pg_info("Import", sprintf("header at row %d | %d data rows", well_row, nrow(raw)))

  if (debug_print) { message("Table: raw"); print(head(raw)) }

  # Validate expected columns are present after name normalisation
  required_cols <- c("well", "fluor", "target", "content", "sample",
                     "cq", "starting_quantity_sq")
  missing_req   <- setdiff(required_cols, names(raw))
  if (length(missing_req) > 0) {
    stop("Missing required columns in: ", file,
         "\nExpected but not found: ", paste(missing_req, collapse = ", "),
         "\nColumns present: ",         paste(names(raw),  collapse = ", "))
  }

  # Parse Cq and SQ to numeric (non-numeric values → NA, warning suppressed)
  dat <- raw |>
    mutate(
      cq_num = suppressWarnings(as.numeric(cq)),
      sq_raw = suppressWarnings(as.numeric(starting_quantity_sq))
    )

  # ----------------------------------------------------------
  # LOD resolution
  # Preferred: per-target list (TARGET_LOD).
  # Fallback:  scalar LOD_Lo and LOD_Hi passed directly.
  # ----------------------------------------------------------
  targets <- NULL  # initialise here so it is always in scope for variable logging

  if (!is.null(LOD_List)) {

    targets <- dat |>
      pull(target) |>
      unique() |>
      as.character() |>
      trimws() |>
      tolower()

    if (anyNA(targets)) {
      stop("NA present in `target` column of ", file, ". Please correct raw data.")
    }

    missing_hi <- setdiff(targets, names(LOD_List$LOD_Hi))
    missing_lo <- setdiff(targets, names(LOD_List$LOD_Lo))
    if (length(missing_hi)) stop("Missing targets in LOD_Hi: ", paste(missing_hi, collapse = ", "))
    if (length(missing_lo)) stop("Missing targets in LOD_Lo: ", paste(missing_lo, collapse = ", "))

    hi_vals <- as.numeric(unlist(LOD_List$LOD_Hi[targets]))
    lo_vals <- as.numeric(unlist(LOD_List$LOD_Lo[targets]))

    tol <- 1e-9

    # If targets have different Hi or Lo values a single-pass pipeline cannot
    # apply one threshold to all rows. Subset your data first, or extend the
    # pipeline to support per-row LOD lookups.
    if ((max(hi_vals) - min(hi_vals)) > tol) {
      print(tibble(target = targets, LOD_Hi = hi_vals, LOD_Lo = lo_vals) |> arrange(LOD_Hi))
      stop("Raw data subsetting required. Different Hi values found: ",
           paste(unique(hi_vals), collapse = ", "))
    }

    if ((max(lo_vals) - min(lo_vals)) > tol) {
      print(tibble(target = targets, LOD_Hi = hi_vals, LOD_Lo = lo_vals) |> arrange(LOD_Lo))
      stop("Raw data subsetting required. Different Lo values found: ",
           paste(unique(lo_vals), collapse = ", "))
    }

    LOD_Hi <- unique(hi_vals)
    LOD_Lo <- unique(lo_vals)

  } else if (is.null(LOD_Lo) || is.null(LOD_Hi)) {
    stop("Provide either LOD_List (per-target) or both scalar LOD_Lo and LOD_Hi.")
  }

  pg_info("LOD", sprintf(
    "Hi=%-8g Lo=%-8g | targets: %s",
    LOD_Hi, LOD_Lo,
    if (is.null(targets)) "(scalar)" else paste(targets, collapse = ", ")
  ))

  # ----------------------------------------------------------
  # Step 0: Remove rows with no sample name
  # ----------------------------------------------------------
  pg_step_start(0, "Removing unnamed samples")

  dat <- dat |>
    mutate(
      sample_reason = case_when(
        is.na(sample) ~ "RM_Sample_removed_due_to_absent_name",
        TRUE          ~ NA_character_
      )
    )

  n_unnamed <- sum(!is.na(dat$sample_reason))

  dat |>
    filter(!is.na(sample_reason)) |>
    rowwise() |>
    do({
      log_decision(.$sample, .$target, .$sample_reason, "applied",
                   "Removed: absent sample name", run_id, dec_log_path, file)
      tibble(.)
    }) |>
    invisible()

  dat <- dat |> filter(is.na(sample_reason))
  pg_step_done(0, sprintf("%d removed", n_unnamed))

  # ----------------------------------------------------------
  # Step 1: Adjust SQ values that are NA or below LOD_Lo
  # Values with no detectable signal are set to LOD_Lo / 2,
  # a standard convention for below-detection-limit observations.
  # ----------------------------------------------------------
  pg_step_start(1, "SQ adjustment (below LOD_Lo → LOD_Lo / 2)")

  dat1 <- dat |>
    mutate(
      sq_adj = case_when(
        is.na(sq_raw) | is.nan(sq_raw) ~ LOD_Lo / 2,
        sq_raw < LOD_Lo                ~ LOD_Lo / 2,
        TRUE                           ~ sq_raw
      ),
      sq_adj_reason = case_when(
        is.na(sq_raw) | is.nan(sq_raw) ~ "ADJ2:SQ_NA_to_half_LOD_Lo",
        sq_raw < LOD_Lo                ~ "ADJ1:SQ_below_LOD_Lo_to_half",
        TRUE                           ~ NA_character_
      )
    )

  n_adj <- sum(!is.na(dat1$sq_adj_reason))

  dat1 |>
    filter(!is.na(sq_adj_reason)) |>
    rowwise() |>
    do({
      log_decision(.$sample, .$target, .$sq_adj_reason, "applied",
                   paste0("sq_raw=", .$sq_raw, "; LOD_Lo=", LOD_Lo),
                   run_id, dec_log_path, file)
      tibble(.)
    }) |>
    invisible()

  pg_step_done(1, sprintf("%d values adjusted", n_adj))
  if (debug_print) { message("Table: dat1 after Step 1"); print(head(dat1)) }

  # ----------------------------------------------------------
  # Step 2: Optional regex-based row removal
  # Rows where any value in rm_cols_req matches any pattern in
  # rm_patterns are removed (or logged only in dry_run mode).
  # ----------------------------------------------------------
  pg_step_start(2, "Regex-based row removal")

  if (!is.null(rm_patterns) && length(rm_patterns) > 0) {
    rm_cols <- intersect(tolower(rm_cols_req), names(dat1))

    if (length(rm_cols) > 0) {
      combined_pat <- paste(rm_patterns, collapse = "|")
      re           <- regex(combined_pat, ignore_case = TRUE)

      dat1 <- dat1 |>
        mutate(rm_hit = if_any(all_of(rm_cols),
                               ~ str_detect(as.character(.x), re)))
      n_hits <- sum(dat1$rm_hit, na.rm = TRUE)

      if (isTRUE(enable_preview)) {
        n_total <- nrow(dat1)
        cat(sprintf("\n  -- Preview for %s --\n", stem))
        cat(sprintf("  Total rows: %d | Matched: %d (%.1f%%)\n",
                    n_total, n_hits, 100 * n_hits / n_total))
        print(dat1 |> filter(rm_hit) |>
                select(well, fluor, target, content, sample, cq, starting_quantity_sq) |>
                head(20))
        log_decision(NA, NA, "RM_PATTERN", "preview",
                     paste0("patterns=", combined_pat, "; cols=", paste(rm_cols, collapse = ","),
                            "; n_matches=", n_hits),
                     run_id, dec_log_path, file)
      }

      if (isTRUE(dry_run)) {
        log_decision(NA, NA, "RM_PATTERN", "dry_run",
                     paste0("patterns=", combined_pat, "; cols=", paste(rm_cols, collapse = ","),
                            "; n_matches=", n_hits),
                     run_id, dec_log_path, file)
        dat1 <- dat1 |> select(-rm_hit)
        pg_step_done(2, sprintf("dry run | %d would be removed", n_hits))
      } else {
        dat1 |>
          filter(rm_hit) |>
          rowwise() |>
          do({
            log_decision(.$sample, .$target, "RM_PATTERN", "applied",
                         paste0("cols=", paste(rm_cols, collapse = ","),
                                "; pattern=", combined_pat),
                         run_id, dec_log_path, file)
            tibble(.)
          }) |>
          invisible()
        dat1 <- dat1 |> filter(!rm_hit) |> select(-rm_hit)
        pg_step_done(2, sprintf("%d rows removed (pattern: %s)", n_hits, combined_pat))
      }
    } else {
      pg_step_done(2, "skipped — none of rm_cols_req found in data")
    }
  } else {
    pg_step_done(2, "skipped — no rm_patterns configured")
  }

  if (debug_print) { message("Table: dat1 after Step 2"); print(head(dat1)) }

  # ----------------------------------------------------------
  # Step 3: Flag sample-name inconsistencies
  # Within each (Fluor, Target, Content) group there should be
  # exactly one sample name. More than one suggests a labelling
  # error and is flagged for review.
  # ----------------------------------------------------------
  pg_step_start(3, "Sample-name consistency check")

  name_checks <- dat1 |>
    group_by(fluor, target, content) |>
    summarise(
      n_samples = n_distinct(sample),
      samples   = paste(sort(unique(sample)), collapse = "|"),
      .groups   = "drop"
    ) |>
    mutate(flag_name_mismatch = n_samples > 1)

  n_mismatch <- sum(name_checks$flag_name_mismatch)

  name_checks |>
    filter(flag_name_mismatch) |>
    rowwise() |>
    do({
      log_decision(NA, .$target, "QC_SN_MISMATCH", "applied",
                   paste0("content=", .$content, "; samples=", .$samples),
                   run_id, dec_log_path, file)
      tibble(.)
    }) |>
    invisible()

  dat2 <- dat1 |>
    left_join(name_checks |> select(fluor, target, content, flag_name_mismatch),
              by = c("fluor", "target", "content")) |>
    mutate(flag_name_mismatch = replace_na(flag_name_mismatch, FALSE))

  pg_step_done(3, sprintf("%d group(s) with mismatched names", n_mismatch))
  if (debug_print) { message("Table: dat2 after Step 3"); print(head(dat2)) }

  # ----------------------------------------------------------
  # Step 4: Per-replicate-group statistics
  # Groups by (Fluor, Target, Sample) and computes:
  #   - delta_cq : |max(Cq) - min(Cq)| within the group
  #   - avg_sq   : mean of adjusted SQ values
  # Then sets review flags based on the computed stats.
  # ----------------------------------------------------------
  pg_step_start(4, "Replicate statistics and review flags")

  rep_key <- c("fluor", "target", "sample")

  rep_summary <- dat2 |>
    group_by(across(all_of(rep_key))) |>
    summarise(
      n_rows   = n(),
      n_num_cq = sum(!is.na(cq_num) & !is.nan(cq_num)),
      cq_vals  = list(cq_num[!is.na(cq_num) & !is.nan(cq_num)]),
      # delta_cq is only meaningful when exactly 2 valid Cq values exist
      delta_cq = if (length(cq_vals[[1]]) == 2) abs(diff(range(cq_vals[[1]]))) else NA_real_,
      avg_sq   = mean(sq_adj, na.rm = TRUE),
      .groups  = "drop"
    ) |>
    mutate(
      both_cq_na  = (n_num_cq == 0),               # Both replicates NA → true negative
      one_cq_na   = (n_num_cq == 1 & n_rows >= 2),  # Only one replicate amplified
      single_rep  = (n_rows == 1),                   # No replication at all

      # Review flags — any TRUE causes the group to appear in the review CSV
      rv_single_rep   = single_rep,
      rv_delta_cq     = !is.na(delta_cq) & (delta_cq > dCq_thr),
      rv_mixed_na_num = one_cq_na,
      rv_high_sq      = avg_sq > LOD_Hi,
      pass_negative   = both_cq_na   # Clean negative; not a failure
    )

  n_flagged <- rep_summary |>
    summarise(n = sum(rv_single_rep | rv_delta_cq | rv_mixed_na_num | rv_high_sq,
                      na.rm = TRUE)) |>
    pull(n)

  # Log every flag outcome for every replicate group
  rep_summary |>
    rowwise() |>
    do({
      log_decision(.$sample, .$target, "RV_SINGLE_REP",
                   if (.$rv_single_rep) "applied" else "skipped",
                   paste0("n_rows=", .$n_rows), run_id, dec_log_path, file)

      log_decision(.$sample, .$target, "RV_DELTA_CQ",
                   if (!is.na(.$delta_cq) && .$delta_cq > dCq_thr) "applied" else "skipped",
                   paste0("|DeltaCq|=", .$delta_cq, "; thr=", dCq_thr),
                   run_id, dec_log_path, file)

      log_decision(.$sample, .$target, "RV_MIXED_CQ",
                   if (.$rv_mixed_na_num) "applied" else "skipped",
                   paste0("n_num_cq=", .$n_num_cq, "; n_rows=", .$n_rows),
                   run_id, dec_log_path, file)

      if (.$pass_negative)
        log_decision(.$sample, .$target, "PASS_NEGATIVE", "pass",
                     "Both Cq values are NA/NaN (true negative)",
                     run_id, dec_log_path, file)

      log_decision(.$sample, .$target, "RV_HIGH_SQ",
                   if (.$rv_high_sq) "applied" else "skipped",
                   paste0("avg_sq=", .$avg_sq, "; LOD_Hi=", LOD_Hi),
                   run_id, dec_log_path, file)

      tibble(.)
    }) |>
    invisible()

  pg_step_done(4, sprintf("%d replicate group(s) flagged for review", n_flagged))
  if (debug_print) { message("Table: rep_summary"); print(head(rep_summary)) }

  # ----------------------------------------------------------
  # Step 5: Assemble final output
  # Join flags back to row-level data. Summary statistics
  # (delta_cq, avg_sq) and review_reason are placed only on
  # the first row of each replicate group to avoid duplication.
  # ----------------------------------------------------------
  pg_step_start(5, "Assembling output tables")

  dat3 <- dat2 |>
    left_join(
      rep_summary |> select(all_of(rep_key), delta_cq, avg_sq, both_cq_na,
                            rv_single_rep, rv_delta_cq, rv_mixed_na_num, rv_high_sq),
      by = rep_key
    ) |>
    mutate(flag_name_mismatch = coalesce(flag_name_mismatch, FALSE))

  # Human-readable descriptions mapped to each flag column
  reason_map <- tribble(
    ~flag,                ~reason,
    "flag_name_mismatch", "Sample name mismatch within (Fluor, Target, Content)",
    "rv_single_rep",      "Only one replicate present",
    "rv_delta_cq",        "|DeltaCq| exceeds threshold",
    "rv_mixed_na_num",    "One Cq is NA/NaN and the other is numeric",
    "rv_high_sq",         "AverageSQ > LOD_Hi"
  )

  review_cols <- c("flag_name_mismatch", "rv_single_rep", "rv_delta_cq",
                   "rv_mixed_na_num", "rv_high_sq")

  dat3 <- dat3 |>
    mutate(
      needs_review  = if_any(all_of(review_cols), ~ .x),
      review_reason = pmap_chr(
        pick(all_of(review_cols)),
        function(...) {
          flags_logical <- c(...)
          flags         <- review_cols[flags_logical]
          if (length(flags) == 0) return(NA_character_)
          paste(reason_map$reason[match(flags, reason_map$flag)], collapse = " ; ")
        }
      )
    )

  # Add replicate index within each group (places summary stats on row 1 only)
  dat4 <- dat3 |>
    group_by(across(all_of(rep_key))) |>
    arrange(well, .by_group = TRUE) |>
    mutate(rep_idx = row_number()) |>
    ungroup()

  # Group-level review flag: TRUE if any row in the group needs review
  needs_review_grp <- dat4 |>
    group_by(across(all_of(rep_key))) |>
    summarise(needs_review_grp = any(needs_review), .groups = "drop")

  dat_out <- dat4 |>
    mutate(
      out_delta_cq   = if_else(rep_idx == 1, delta_cq,      NA_real_),
      out_average_sq = if_else(rep_idx == 1, avg_sq,        NA_real_),
      out_reason     = if_else(rep_idx == 1, review_reason, NA_character_)
    ) |>
    left_join(needs_review_grp, by = rep_key)

  if (debug_print) { message("Table: dat_out"); print(head(dat_out)) }

  # ----------------------------------------------------------
  # Log parameters used for this run
  # ----------------------------------------------------------
  log_variables(
    vars = list(
      file           = file,
      dCq_thr        = dCq_thr,
      targets        = targets,   # NULL when scalar LODs were used
      lod_hi         = LOD_Hi,
      lod_lo         = LOD_Lo,
      rm_patterns    = rm_patterns,
      rm_cols_req    = rm_cols_req,
      enable_preview = enable_preview,
      dry_run        = dry_run,
      debug_print    = debug_print,
      output_dir     = output_dir,
      dec_log_path   = dec_log_path,
      var_log_path   = var_log_path
    ),
    run_id       = run_id,
    var_log_path = var_log_path,
    default_file = file
  )

  # ----------------------------------------------------------
  # Write output CSVs
  # ----------------------------------------------------------
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  all_path    <- file.path(output_dir, paste0(stem, "_all_samples.csv"))
  review_path <- file.path(output_dir, paste0(stem, "_review_samples.csv"))

  all_samples <- dat_out |>
    transmute(
      Well                     = well,
      Fluor                    = fluor,
      Target                   = target,
      Content                  = content,
      Sample                   = sample,
      Cq                       = cq,
      `Starting Quantity (SQ)` = sq_adj,
      DeltaCq                  = out_delta_cq,
      AverageSQ                = out_average_sq,
      review_reason            = out_reason
    )

  review_samples <- all_samples |>
    left_join(
      dat_out |> select(well, fluor, target, sample, needs_review_grp),
      by = c("Well" = "well", "Fluor" = "fluor", "Target" = "target", "Sample" = "sample")
    ) |>
    filter(needs_review_grp) |>
    select(-needs_review_grp)

  write_csv_retry(all_samples,    all_path,    na = "")
  write_csv_retry(review_samples, review_path, na = "")

  pg_step_done(5)
  pg_summary(nrow(all_samples), nrow(review_samples))

  invisible(list(
    stem            = stem,
    all_path        = all_path,
    review_path     = review_path,
    n_in            = nrow(raw),
    n_out           = nrow(all_samples),
    n_review        = nrow(review_samples),
    preview_enabled = isTRUE(enable_preview),
    dry_run         = isTRUE(dry_run)
  ))
}


# ============================================================
# SECTION 8: Discover and Validate Plate Files
# ============================================================

plate_files <- if (!is.null(FILES)) {
  as.character(FILES)
} else {
  list.files(INPUT_DIR, pattern = FILE_PATTERN, full.names = TRUE)
}

if (length(plate_files) == 0) {
  stop("No input files found. Check INPUT_DIR ('", INPUT_DIR,
       "') and FILE_PATTERN ('", FILE_PATTERN, "').")
}

missing_files <- plate_files[!file.exists(plate_files)]
if (length(missing_files) > 0) {
  stop("The following files do not exist:\n",
       paste(" -", missing_files, collapse = "\n"))
}

cat(sprintf("Found %d file(s) to process:\n", length(plate_files)))
cat(paste(" -", plate_files, collapse = "\n"), "\n")


# ============================================================
# SECTION 9: Run Pipeline Across All Plates
# ============================================================

results <- lapply(seq_along(plate_files), function(i) {
  process_plate(
    file           = plate_files[[i]],
    LOD_List       = TARGET_LOD,           # Remove this line and set LOD_Lo / LOD_Hi
    dCq_thr        = DELTA_CQ_THRESHOLD,   # directly if not using per-target LODs
    rm_patterns    = RM_PATTERNS,
    rm_cols_req    = RM_COLUMNS,
    plate_index    = i,
    n_plates       = length(plate_files),
    enable_preview = ENABLE_PREVIEW,
    dry_run        = DRY_RUN,
    debug_print    = DEBUG_PRINT,
    output_dir     = OUTPUT_DIR,
    dec_log_path   = DEC_LOG_PATH,
    var_log_path   = VAR_LOG_PATH
  )
})


# ============================================================
# SECTION 10: Run Summary
# ============================================================

cat("\n", strrep("=", .pg_width), "\n", sep = "")
cat(" Run complete\n")
cat(strrep("=", .pg_width), "\n", sep = "")

summary_tbl <- bind_rows(lapply(results, function(r) {
  tibble(
    plate    = r$stem,
    rows_in  = r$n_in,
    rows_out = r$n_out,
    review   = r$n_review,
    dry_run  = r$dry_run
  )
}))

print(summary_tbl)


# ============================================================
# SECTION 11: Audit Log Preview
# ============================================================
# Prints the 30 most recent audit decisions and a count table
# showing how many times each rule fired per file.

if (file.exists(DEC_LOG_PATH)) {
  audit <- read_csv(DEC_LOG_PATH, show_col_types = FALSE)

  cat("\n--- 30 most recent audit decisions ---\n")
  print(audit |> arrange(desc(timestamp)) |> head(30))

  cat("\n--- Decision counts by file, rule, and outcome ---\n")
  print(audit |> count(input_file, rule_id, outcome, sort = TRUE))

  rm(audit)
} else {
  cat("No audit log found at:", DEC_LOG_PATH, "\n")
}
