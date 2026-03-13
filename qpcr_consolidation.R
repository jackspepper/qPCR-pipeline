# ============================================================
#  qPCR Results Consolidation
#  Complementary to qpcr_cleaning_pipeline.R
#
#  PURPOSE:
#    Reads all _all_samples.csv and _review_samples.csv files
#    produced by the cleaning pipeline from INPUT_DIR, adds
#    metadata columns (File Name, Date, Plate#), then writes
#    two Excel workbooks — one per output type — with each
#    target as a separate sheet:
#      - <ALL_OUT_PATH>    : all cleaned rows, one sheet per target
#      - <REVIEW_OUT_PATH> : review-flagged rows, one sheet per target
#
#  USAGE:
#    1. Set INPUT_DIR to match OUTPUT_DIR from the cleaning pipeline.
#    2. Set ALL_OUT_PATH and REVIEW_OUT_PATH for your desired locations.
#    3. Run the whole script (source() or Ctrl+Shift+Enter in RStudio).
#
#  If either output workbook already exists you will be prompted to:
#    O = Overwrite   — discard existing workbook and rebuild from scratch
#    A = Append      — merge new data into the existing workbook
#    N = New file    — keep existing workbook and save under a new name
#
#  DEPENDENCIES:
#    install.packages(c("tidyverse", "openxlsx"))
# ============================================================

version_script <- 0.1.1

# ============================================================
# SECTION 1: Configuration
# ============================================================

# --- Input ---
INPUT_DIR <- "outputs/"   # OUTPUT_DIR from the cleaning pipeline.

# Regex patterns used to identify the two CSV types in INPUT_DIR.
# Change these only if the cleaning pipeline output filenames differ.
ALL_PATTERN    <- "_all_samples\\.csv$"
REVIEW_PATTERN <- "_review_samples\\.csv$"

# --- Output ---
# Folder where consolidated workbooks are written (created if missing).
CONSOLIDATION_DIR <- "consolidated"

ALL_OUT_PATH    <- file.path(CONSOLIDATION_DIR, "qpcr_all_samples.xlsx")
REVIEW_OUT_PATH <- file.path(CONSOLIDATION_DIR, "qpcr_review_samples.xlsx")

# --- Excel formatting ---
TABLE_STYLE <- "TableStyleMedium2"  # Excel built-in table style (header + alt rows).
FONT_NAME   <- "Arial"              # Font applied to all data cells.
FONT_SIZE   <- 10                   # Font size (pt).

# Minimum column width (characters).  "auto" sizes to content but can be very
# narrow for sparse columns; this floor keeps them readable.
COL_WIDTH_MIN <- 10

# --- Write retry (for network / server paths) ---
MAX_WRITE_TRIES <- 5   # Maximum attempts before giving up.
WAIT_SECS       <- 3   # Seconds to wait between retry attempts.

# --- Target alias mapping ---
# Defines groups of target names that should be treated as the same target
# and consolidated onto a single sheet.
#
# Structure: each entry is a named character vector where:
#   - The NAME  is the canonical (final) sheet name that will appear in Excel
#   - The VALUE is a character vector of ALL aliases for that target,
#     including the canonical name itself
#
# Matching is case-insensitive (targets are already lowercased before
# alias resolution runs).
#
# To add a new alias group, add another entry following the same pattern.
# To disable alias mapping entirely, set TARGET_ALIASES <- list()
TARGET_ALIASES <- list(
  uni = c("uni", "universal")
  # Add further groups as needed, e.g.:
  # speb = c("speb", "spec_b", "specb")
)

# Controls what appears in the Target column of the OUTPUT DATA ROWS
# after alias resolution:
#
#   TRUE  — update Target to the canonical name (all "universal" rows
#           become "uni" in the sheet data)
#   FALSE — preserve the original value from the source file (rows
#           keep "universal" or "uni" as written, but both land on the
#           same sheet)
UPDATE_TARGET_TO_CANONICAL <- TRUE

# --- Run log ---
# Plain-text transcript of all console output for this run.
# Set to NULL to disable.
CONSOLIDATION_LOG_PATH <- "audit/pcr_consolidation_log.txt"


# ============================================================
# SECTION 2: Library Import
# ============================================================

library(tidyverse)
library(openxlsx)

options(dplyr.show_progress = FALSE)
options(readr.show_progress = FALSE)
options(vroom.show_progress = FALSE)
options(warn = 2)


# ============================================================
# SECTION 3: Utility Helpers
# ============================================================

# --- Console ruler width (matches cleaning pipeline) ---
.pg_width <- 60


# ----------------------------------------------------------
# Decision / event reporter
#
# All notable actions are printed with a clearly visible
# prefix so they stand out when reviewing the console log.
#
# Types:
#   "+"  success / normal info
#   "!"  warning (processed but something to note)
#   "~"  a change was made (append / rename / coerce)
#   "x"  skipped / error (file not processed)
# ----------------------------------------------------------
report <- function(type, msg) {
  symbol <- switch(type,
                   "+" = "[+]",
                   "!" = "[!]",
                   "~" = "[~]",
                   "x" = "[x]",
                   "[?]"
  )
  cat(sprintf("  %s %s\n", symbol, msg))
}


# ----------------------------------------------------------
# Progress bar
#
# Renders a single updating line to the console while files
# are being read or sheets are being written.  Clears the
# line cleanly on completion.
#
# Usage:
#   pb <- pb_init(total = 77, label = "Reading")
#   for (i in seq_len(77)) {
#     pb_tick(pb, i, detail = basename(files[[i]]))
#   }
#   pb_done(pb)
# ----------------------------------------------------------
pb_init <- function(total, label = "Working") {
  list(total = total, label = label, start = Sys.time(), width = 36)
}

pb_tick <- function(pb, current, detail = "") {
  pct     <- current / pb$total
  filled  <- round(pct * pb$width)
  elapsed <- as.numeric(difftime(Sys.time(), pb$start, units = "secs"))
  eta     <- if (pct > 0 && pct < 1)
    sprintf("ETA: %ds", round(elapsed / pct * (1 - pct)))
  else if (pct >= 1) "done    " else "ETA: --"

  bar     <- sprintf("[%s%s]",
                     strrep("=", filled),
                     strrep(" ", pb$width - filled))

  detail_str <- if (nzchar(detail))
    sprintf("  %s", substr(basename(detail), 1, 28))
  else ""

  cat(sprintf("\r  %-10s %s %d/%d (%.0f%%)  %s%s",
              pb$label, bar, current, pb$total,
              pct * 100, eta, detail_str))
  flush.console()
  invisible(pb)
}

pb_done <- function(pb) {
  elapsed <- as.numeric(difftime(Sys.time(), pb$start, units = "secs"))
  cat(sprintf("\r  %-10s %s %d/%d (100%%)  %.1fs elapsed%s\n",
              pb$label,
              sprintf("[%s]", strrep("=", pb$width)),
              pb$total, pb$total, elapsed,
              strrep(" ", 20)))
  flush.console()
  invisible(pb)
}


# ----------------------------------------------------------
# save_workbook_retry
#
# Wraps openxlsx::saveWorkbook() with automatic retry on
# failure. Mirrors write_csv_retry() in the cleaning pipeline
# to handle intermittent network / server write errors.
#
# Args:
#   wb         : openxlsx Workbook object
#   path       : destination .xlsx path
#   overwrite  : passed to saveWorkbook (default TRUE)
#   max_tries  : maximum attempts (default MAX_WRITE_TRIES)
#   wait_secs  : seconds between attempts (default WAIT_SECS)
# ----------------------------------------------------------
save_workbook_retry <- function(wb, path,
                                overwrite  = TRUE,
                                max_tries  = MAX_WRITE_TRIES,
                                wait_secs  = WAIT_SECS) {
  attempt <- 0
  repeat {
    attempt <- attempt + 1
    result  <- tryCatch({
      saveWorkbook(wb, file = path, overwrite = overwrite)
      "ok"
    }, error = function(e) e)

    if (identical(result, "ok")) break

    if (attempt >= max_tries) {
      stop(sprintf(
        "save_workbook_retry: failed after %d attempt(s) writing to:\n  %s\nLast error: %s",
        attempt, path, conditionMessage(result)
      ))
    }
    cat(sprintf(
      "  [write retry %d/%d] Could not write to '%s'. Waiting %ds...\n",
      attempt, max_tries, basename(path), wait_secs
    ))
    Sys.sleep(wait_secs)
  }
  invisible(wb)
}


# ----------------------------------------------------------
# parse_filename
#
# Extracts Date and Plate# from a CSV filename using the
# conventions seen in CFX Manager exports:
#   Date   — a leading YYMMDD or YYYYMMDD block
#   Plate# — the integer following the word "plate"
#            (case-insensitive, e.g. "plate9" → 9)
#
# Returns a named list: $date (character "YYYY-MM-DD" or NA)
#                       $plate_num (integer or NA)
# ----------------------------------------------------------
parse_filename <- function(fname) {
  stem <- sub("\\.csv$", "", basename(fname), ignore.case = TRUE)

  # --- Date ---
  date_raw <- regmatches(stem, regexpr("^(\\d{8}|\\d{6})", stem))
  parsed_date <- if (length(date_raw) == 1L && nchar(date_raw) > 0L) {
    fmt <- if (nchar(date_raw) == 6L) "%y%m%d" else "%Y%m%d"
    tryCatch(
      format(as.Date(date_raw, format = fmt), "%Y-%m-%d"),
      error = function(e) NA_character_
    )
  } else {
    NA_character_
  }

  # --- Plate number ---
  plate_raw <- regmatches(
    stem,
    regexpr("(?i)plate(\\d+)", stem, perl = TRUE)
  )
  plate_num <- if (length(plate_raw) == 1L && nchar(plate_raw) > 0L) {
    suppressWarnings(as.integer(
      sub("(?i)^plate", "", plate_raw, perl = TRUE)
    ))
  } else {
    NA_integer_
  }

  list(date = parsed_date, plate_num = plate_num)
}


# ----------------------------------------------------------
# fmt_size
# Formats a byte count as a human-readable string.
# ----------------------------------------------------------
fmt_size <- function(bytes) {
  if (is.na(bytes) || bytes < 0) return("unknown")
  units <- c("B", "KB", "MB", "GB")
  i <- 1L
  while (bytes >= 1024 && i < length(units)) {
    bytes <- bytes / 1024
    i <- i + 1L
  }
  sprintf("%.1f %s", bytes, units[[i]])
}


# ----------------------------------------------------------
# wb_stats
# Returns a summary string for an existing xlsx workbook.
# ----------------------------------------------------------
wb_stats <- function(path) {
  info       <- file.info(path)
  sheet_nms  <- tryCatch(getSheetNames(path), error = function(e) character(0))
  n_sheets   <- length(sheet_nms)
  sheets_str <- if (n_sheets == 0) "(none)"
  else paste(sheet_nms, collapse = ", ")

  sprintf(
    "  Modified : %s\n  Size     : %s\n  Sheets   : %s (%d sheet%s)",
    format(info$mtime, "%Y-%m-%d %H:%M:%S"),
    fmt_size(info$size),
    sheets_str,
    n_sheets,
    if (n_sheets == 1L) "" else "s"
  )
}


# ----------------------------------------------------------
# prompt_file_action
#
# If the output path already exists, prints stats on the
# existing file and asks the user how to proceed.
#
# Returns a named list:
#   $action    : "overwrite" | "append" | "new"
#   $out_path  : the path to actually write to (may differ
#                from `path` if action == "new")
# ----------------------------------------------------------
prompt_file_action <- function(path, label) {

  if (!file.exists(path)) {
    return(list(action = "new_file", out_path = path))
  }

  cat(sprintf("\n%s\n", strrep("-", .pg_width)))
  cat(sprintf(" %s already exists:\n", label))
  cat(wb_stats(path), "\n")
  cat(strrep("-", .pg_width), "\n")
  cat(" Options:\n")
  cat("   O = Overwrite  — discard existing workbook and rebuild\n")
  cat("   A = Append     — merge new data into existing workbook\n")
  cat("   N = New file   — save under a different name\n")

  if (!interactive()) {
    cat("\n  [non-interactive] Defaulting to O (overwrite).\n")
    return(list(action = "overwrite", out_path = path))
  }

  cat(" Enter O, A, or N: ")
  response <- toupper(trimws(readline()))
  while (!response %in% c("O", "A", "N")) {
    cat("  Please enter O, A, or N: ")
    response <- toupper(trimws(readline()))
  }

  if (response == "O") {
    report("+", sprintf("Overwrite selected for: %s", basename(path)))
    return(list(action = "overwrite", out_path = path))
  }

  if (response == "A") {
    report("~", sprintf("Append selected for: %s", basename(path)))
    return(list(action = "append", out_path = path))
  }

  # --- New file ---
  dir_part  <- dirname(path)
  base_stem <- sub("\\.xlsx$", "", basename(path), ignore.case = TRUE)
  default_suffix <- format(Sys.time(), "%Y%m%d_%H%M%S")

  cat(sprintf(
    "\n  Enter a suffix for the new filename.\n  Press Enter to use timestamp [default: _%s]:\n  > ",
    default_suffix
  ))
  raw_suffix <- trimws(readline())
  suffix     <- if (nzchar(raw_suffix)) raw_suffix else default_suffix
  new_path   <- file.path(dir_part, sprintf("%s_%s.xlsx", base_stem, suffix))

  report("+", sprintf("New file: %s", basename(new_path)))
  list(action = "new_file", out_path = new_path)
}


# ============================================================
# SECTION 4: Open Run Log Sink
# ============================================================

if (!is.null(CONSOLIDATION_LOG_PATH)) {
  if (!dir.exists(dirname(CONSOLIDATION_LOG_PATH)))
    dir.create(dirname(CONSOLIDATION_LOG_PATH), recursive = TRUE,
               showWarnings = FALSE)
  .log_con   <- file(CONSOLIDATION_LOG_PATH, open = "wt")
  .log_start <- Sys.time()
  sink(.log_con, split = TRUE, type = "output")

  cat(strrep("=", .pg_width), "\n", sep = "")
  cat(" qPCR Consolidation Run Log\n")
  cat(strrep("-", .pg_width), "\n", sep = "")
  cat(sprintf(" Started  : %s\n", format(.log_start, "%Y-%m-%d %H:%M:%S")))
  cat(sprintf(" User     : %s\n", Sys.info()[["user"]]))
  cat(sprintf(" Input    : %s\n", INPUT_DIR))
  cat(sprintf(" All out  : %s\n", ALL_OUT_PATH))
  cat(sprintf(" Review   : %s\n", REVIEW_OUT_PATH))
  cat(sprintf(" Version  : %s\n", version_script))
  cat(strrep("=", .pg_width), "\n\n", sep = "")
}


# ============================================================
# SECTION 5: Discover Input Files
# ============================================================

cat(strrep("-", .pg_width), "\n", sep = "")
cat(" File discovery\n")
cat(strrep("-", .pg_width), "\n", sep = "")

if (!dir.exists(INPUT_DIR)) {
  stop("INPUT_DIR does not exist: ", INPUT_DIR,
       "\nCheck that the cleaning pipeline has been run and OUTPUT_DIR matches.",
       call. = FALSE)
}

all_files    <- sort(list.files(INPUT_DIR, pattern = ALL_PATTERN,
                                full.names = TRUE, recursive = TRUE))
review_files <- sort(list.files(INPUT_DIR, pattern = REVIEW_PATTERN,
                                full.names = TRUE, recursive = TRUE))

if (length(all_files) == 0 && length(review_files) == 0) {
  stop(
    "No matching files found in: ", INPUT_DIR,
    "\n  ALL_PATTERN    : ", ALL_PATTERN,
    "\n  REVIEW_PATTERN : ", REVIEW_PATTERN,
    "\nCheck that the cleaning pipeline has been run successfully.",
    call. = FALSE
  )
}

cat(sprintf("  Found %d _all_samples file(s)\n",    length(all_files)))
cat(sprintf("  Found %d _review_samples file(s)\n", length(review_files)))


# ============================================================
# SECTION 6: Load, Parse, and Annotate Source Files
# ============================================================
# Reads each CSV, adds File Name / Date / Plate# columns,
# renames columns to match the output spec, and returns a
# single combined data frame ready for sheet-splitting.
#
# Expected source columns (from cleaning pipeline):
#   Well, Fluor, Target, Content, Sample, Cq,
#   Starting Quantity (SQ), DeltaCq, AverageSQ, review_reason
#
# Output columns (in display order):
#   File Name, Date, Plate#, Well, Fluor, Target, Content,
#   Sample, Cq, Starting Quantity (SQ), DeltaCq, AverageSQ,
#   Review Reason
# ============================================================

# Canonical output column names and their source equivalents.
# Source columns are as written by the cleaning pipeline.
.OUT_COLS <- c(
  "File Name", "Date", "Plate#",           # added by this script
  "Well", "Fluor", "Target", "Content",
  "Sample", "Cq", "Starting Quantity (SQ)",
  "DeltaCq", "AverageSQ", "Review Reason"  # "review_reason" renamed
)

# Columns expected from the cleaning pipeline CSV.
# If any are absent a warning is emitted and the column is filled with NA.
.EXPECTED_SRC_COLS <- c(
  "Well", "Fluor", "Target", "Content", "Sample",
  "Cq", "Starting Quantity (SQ)", "DeltaCq", "AverageSQ", "review_reason"
)


read_and_annotate <- function(files, label) {

  if (length(files) == 0) {
    cat(sprintf("\n  No %s files to read.\n", label))
    return(tibble())
  }

  cat(sprintf("\n%s\n", strrep("-", .pg_width)))
  cat(sprintf(" Reading %s files\n", label))
  cat(strrep("-", .pg_width), "\n", sep = "")

  pb      <- pb_init(length(files), label = "Reading")
  results <- vector("list", length(files))

  for (i in seq_along(files)) {
    fp   <- files[[i]]
    stem <- sub("\\.csv$", "", basename(fp), ignore.case = TRUE)

    pb_tick(pb, i, detail = basename(fp))

    # --- Parse metadata from filename ---
    meta <- parse_filename(fp)

    if (is.na(meta$date)) {
      report("!", sprintf("No date found in filename: %s", basename(fp)))
    }
    if (is.na(meta$plate_num)) {
      report("!", sprintf("No plate number found in filename: %s", basename(fp)))
    }

    # --- Read CSV ---
    dat <- tryCatch(
      read_csv(fp, show_col_types = FALSE, progress = FALSE,
               col_types = cols(.default = col_character())),
      error = function(e) {
        report("x", sprintf("Could not read '%s': %s", basename(fp),
                            conditionMessage(e)))
        NULL
      }
    )

    if (is.null(dat)) next

    # --- Check for missing expected columns ---
    missing_cols <- setdiff(.EXPECTED_SRC_COLS, names(dat))
    if (length(missing_cols) > 0) {
      report("!", sprintf(
        "%s — missing column(s) filled with NA: %s",
        basename(fp), paste(missing_cols, collapse = ", ")
      ))
      for (mc in missing_cols) dat[[mc]] <- NA_character_
    }

    # --- Rename review_reason → Review Reason ---
    dat <- dat |>
      rename(`Review Reason` = review_reason)

    # --- Coerce numeric columns (stored as character after read with col_character) ---
    numeric_cols <- c("Cq", "Starting Quantity (SQ)", "DeltaCq", "AverageSQ")
    for (nc in intersect(numeric_cols, names(dat))) {
      dat[[nc]] <- suppressWarnings(as.numeric(dat[[nc]]))
    }

    # --- Add metadata columns ---
    dat <- dat |>
      mutate(
        `File Name` = basename(fp),
        `Date`      = meta$date,
        `Plate#`    = meta$plate_num,
        .before     = 1
      )

    # --- Select and order output columns (any_of tolerates extras gracefully) ---
    dat <- dat |> select(any_of(.OUT_COLS))

    # --- Add any .OUT_COLS still missing after selection ---
    truly_missing <- setdiff(.OUT_COLS, names(dat))
    if (length(truly_missing) > 0) {
      for (tm in truly_missing) dat[[tm]] <- NA_character_
    }

    dat <- dat |> select(all_of(.OUT_COLS))

    results[[i]] <- dat
  }

  pb_done(pb)

  combined <- bind_rows(results)
  n_rows   <- nrow(combined)
  n_files  <- sum(!vapply(results, is.null, logical(1)))

  report("+", sprintf(
    "%s: %d row(s) loaded from %d of %d file(s)",
    label, n_rows, n_files, length(files)
  ))

  # --- Normalise Target to lowercase ---
  # Excel sheet names are case-insensitive, so mixed-case variants like
  # "gyrB" and "GyrB" would collide when used as sheet names.  All Target
  # values are lowercased here; any that required a change are reported
  # so the user can see exactly what was standardised.
  if ("Target" %in% names(combined) && nrow(combined) > 0) {
    original_targets <- unique(combined$Target[!is.na(combined$Target)])
    normalised       <- tolower(original_targets)
    changed          <- original_targets[original_targets != normalised]

    if (length(changed) > 0) {
      report("~", sprintf(
        "%s: %d Target value(s) normalised to lowercase: %s",
        label,
        length(changed),
        paste(sprintf("'%s'->'%s'", changed, tolower(changed)), collapse = ", ")
      ))
    }

    combined <- combined |>
      mutate(Target = tolower(Target))
  }

  # --- Apply target alias mapping ---
  # Aliases are matched against the already-lowercased Target column.
  # For each alias group, rows whose Target matches any alias are
  # routed to the canonical sheet; the Target column value is updated
  # or preserved according to UPDATE_TARGET_TO_CANONICAL.
  if (length(TARGET_ALIASES) > 0 &&
      "Target" %in% names(combined) &&
      nrow(combined) > 0) {

    for (canonical in names(TARGET_ALIASES)) {
      aliases     <- tolower(TARGET_ALIASES[[canonical]])
      # Aliases other than the canonical name itself
      non_canonical <- aliases[aliases != tolower(canonical)]
      # Rows that match any non-canonical alias
      hits <- combined$Target %in% non_canonical & !is.na(combined$Target)
      n_hits <- sum(hits)

      if (n_hits > 0) {
        hit_vals <- sort(unique(combined$Target[hits]))
        if (isTRUE(UPDATE_TARGET_TO_CANONICAL)) {
          combined$Target[hits] <- canonical
          report("~", sprintf(
            "%s: %d row(s) re-labelled to canonical target '%s' (from: %s)  [UPDATE_TARGET_TO_CANONICAL = TRUE]",
            label, n_hits, canonical,
            paste(sprintf("'%s'", hit_vals), collapse = ", ")
          ))
        } else {
          report("~", sprintf(
            "%s: %d row(s) with target(s) %s will share sheet '%s' — original labels preserved  [UPDATE_TARGET_TO_CANONICAL = FALSE]",
            label, n_hits,
            paste(sprintf("'%s'", hit_vals), collapse = ", "),
            canonical
          ))
        }
      }
    }

    # When UPDATE_TARGET_TO_CANONICAL is FALSE the Target column still holds
    # original values, so build_workbook needs to know which sheet to route
    # each row to.  Add a hidden routing column that maps every alias to its
    # canonical name; build_workbook uses this if present and drops it before
    # writing to Excel.
    if (!isTRUE(UPDATE_TARGET_TO_CANONICAL)) {
      alias_lookup <- unlist(lapply(names(TARGET_ALIASES), function(cn) {
        setNames(rep(cn, length(TARGET_ALIASES[[cn]])),
                 tolower(TARGET_ALIASES[[cn]]))
      }))
      combined <- combined |>
        mutate(.sheet_target = dplyr::coalesce(
          alias_lookup[Target],
          Target
        ))
    }
  }

  # If UPDATE_TARGET_TO_CANONICAL is TRUE (or no aliases apply) ensure
  # .sheet_target mirrors Target for consistent downstream handling.
  if (!".sheet_target" %in% names(combined)) {
    combined <- combined |> mutate(.sheet_target = Target)
  }

  combined
}


all_data    <- read_and_annotate(all_files,    "all_samples")
review_data <- read_and_annotate(review_files, "review_samples")


# ============================================================
# SECTION 7: Handle Existing Output Files
# ============================================================
# Check whether each output workbook already exists and prompt
# the user for the desired action (overwrite / append / new).
# For append mode, load the existing workbook data so it can
# be merged with the freshly read data.
# ============================================================

dir.create(CONSOLIDATION_DIR, showWarnings = FALSE, recursive = TRUE)

# Prompt for each workbook independently
all_action    <- prompt_file_action(ALL_OUT_PATH,    "All-samples workbook")
review_action <- prompt_file_action(REVIEW_OUT_PATH, "Review-samples workbook")


# ----------------------------------------------------------
# load_existing_sheets
# For append mode: read all sheets from an existing workbook
# back into a list of data frames, then combine into one
# data frame with a Target column inferred from sheet name.
# ----------------------------------------------------------
load_existing_sheets <- function(path) {

  sheet_names <- tryCatch(
    getSheetNames(path),
    error = function(e) {
      report("x", sprintf("Could not read sheets from '%s': %s",
                          basename(path), conditionMessage(e)))
      character(0)
    }
  )

  if (length(sheet_names) == 0) return(tibble())

  cat(sprintf("  Loading %d existing sheet(s) from %s...\n",
              length(sheet_names), basename(path)))

  sheets <- lapply(sheet_names, function(sn) {
    df <- tryCatch(
      read.xlsx(path, sheet = sn, colNames = TRUE, detectDates = FALSE),
      error = function(e) {
        report("x", sprintf("Could not read sheet '%s' from '%s': %s",
                            sn, basename(path), conditionMessage(e)))
        NULL
      }
    )
    if (!is.null(df)) {
      # Ensure Target column matches sheet name (it should, but be defensive)
      if (!"Target" %in% names(df)) df$Target <- sn
      # Coerce numerics back from character (xlsx read may return character)
      for (nc in intersect(c("Cq", "Starting Quantity (SQ)", "DeltaCq", "AverageSQ"), names(df))) {
        df[[nc]] <- suppressWarnings(as.numeric(df[[nc]]))
      }
      if ("Plate#" %in% names(df))
        df[["Plate#"]] <- suppressWarnings(as.integer(df[["Plate#"]]))
    }
    df
  })

  bind_rows(sheets)
}


# If appending, merge existing data with newly read data
if (all_action$action == "append" && file.exists(all_action$out_path)) {
  existing_all <- load_existing_sheets(all_action$out_path)
  n_existing   <- nrow(existing_all)
  all_data     <- bind_rows(existing_all, all_data)
  report("~", sprintf(
    "All-samples append: %d existing row(s) + %d new row(s) = %d total",
    n_existing, nrow(all_data) - n_existing, nrow(all_data)
  ))
}

if (review_action$action == "append" && file.exists(review_action$out_path)) {
  existing_review <- load_existing_sheets(review_action$out_path)
  n_existing      <- nrow(existing_review)
  review_data     <- bind_rows(existing_review, review_data)
  report("~", sprintf(
    "Review-samples append: %d existing row(s) + %d new row(s) = %d total",
    n_existing, nrow(review_data) - n_existing, nrow(review_data)
  ))
}


# ============================================================
# SECTION 8: Build and Write Workbooks
# ============================================================

# ----------------------------------------------------------
# build_workbook
#
# Splits `data` by .sheet_target (the canonical sheet name,
# set by alias resolution in read_and_annotate), writes one
# sheet per target into a new openxlsx Workbook, and returns
# it ready for saving.  The internal .sheet_target column is
# dropped before any data is written to Excel.
#
# Sheet names are truncated to 31 characters (Excel limit).
#
# Columns are formatted with:
#   - Excel table (auto-filter on every header)
#   - Frozen first row
#   - Auto-fitted column widths (minimum COL_WIDTH_MIN chars)
#   - Arial font, consistent size
# ----------------------------------------------------------
build_workbook <- function(data, wb_label) {

  wb <- createWorkbook()

  # Use .sheet_target (canonical name) for sheet splitting.
  # This column is always present: either set by alias resolution
  # or mirroring Target when no aliases apply.
  sheet_col <- if (".sheet_target" %in% names(data)) ".sheet_target" else "Target"
  targets   <- sort(unique(data[[sheet_col]]))
  targets   <- targets[!is.na(targets) & nzchar(targets)]

  if (length(targets) == 0) {
    report("!", sprintf("%s: no target values found — empty workbook will be written", wb_label))
    return(wb)
  }

  cat(sprintf("\n%s\n", strrep("-", .pg_width)))
  cat(sprintf(" Building %s workbook — %d target sheet(s)\n",
              wb_label, length(targets)))
  cat(strrep("-", .pg_width), "\n", sep = "")

  # Style applied to every data cell (not the table header row, which
  # is controlled by the table style itself)
  data_style <- createStyle(
    fontName = FONT_NAME,
    fontSize = FONT_SIZE
  )

  pb <- pb_init(length(targets), label = "Sheets")

  for (i in seq_along(targets)) {
    tgt <- targets[[i]]

    # --- Sheet name (Excel limit: 31 chars; warn if truncated) ---
    sn <- substr(tgt, 1L, 31L)
    if (nchar(tgt) > 31L) {
      report("!", sprintf(
        "Target '%s' truncated to '%s' for Excel sheet name (31-char limit)",
        tgt, sn
      ))
    }

    # --- Filter by canonical sheet name; drop internal routing column ---
    tgt_data <- data |>
      filter(.data[[sheet_col]] == tgt) |>
      select(any_of(.OUT_COLS))   # .sheet_target is not in .OUT_COLS so drops automatically

    # Ensure all output columns are present (fill missing with NA)
    for (col in setdiff(.OUT_COLS, names(tgt_data))) {
      tgt_data[[col]] <- NA_character_
    }
    tgt_data <- tgt_data |> select(all_of(.OUT_COLS))

    n_rows <- nrow(tgt_data)

    # --- Add and populate sheet ---
    addWorksheet(wb, sheetName = sn)

    writeDataTable(
      wb,
      sheet      = sn,
      x          = tgt_data,
      tableStyle = TABLE_STYLE,
      withFilter = TRUE,
      keepNA     = FALSE,   # write NA as blank cell (not the string "NA")
      na.string  = ""
    )

    # Apply font to data rows (row 1 is the header, managed by table style)
    if (n_rows > 0) {
      addStyle(wb, sheet = sn, style = data_style,
               rows = 2:(n_rows + 1),
               cols = seq_len(ncol(tgt_data)),
               gridExpand = TRUE)
    }

    # Freeze the header row
    freezePane(wb, sheet = sn, firstRow = TRUE)

    # Auto-fit column widths with a minimum floor.
    # Guard against all-NA columns: nchar(NA) -> NA, so max(..., na.rm=TRUE)
    # on a fully-NA vector returns -Inf and warns.  Replace that with 0 so
    # the column falls through to COL_WIDTH_MIN cleanly.
    col_widths <- pmax(
      COL_WIDTH_MIN,
      vapply(names(tgt_data), function(cn) {
        vals     <- nchar(as.character(head(tgt_data[[cn]], 100)))
        content_w <- suppressWarnings(max(vals, na.rm = TRUE))
        if (!is.finite(content_w)) content_w <- 0
        max(nchar(cn), content_w)
      }, numeric(1))
    )
    setColWidths(wb, sheet = sn,
                 cols   = seq_len(ncol(tgt_data)),
                 widths = col_widths)

    pb_tick(pb, i, detail = sn)
    report("+", sprintf("Sheet '%-20s' — %d row(s)", sn, n_rows))
  }

  pb_done(pb)
  wb
}


# ----------------------------------------------------------
# Build workbooks for all-samples and review-samples
# ----------------------------------------------------------
cat("\n")

wb_all    <- build_workbook(all_data,    "all-samples")
wb_review <- build_workbook(review_data, "review-samples")


# ----------------------------------------------------------
# Write workbooks with retry
# ----------------------------------------------------------
cat(sprintf("\n%s\n", strrep("-", .pg_width)))
cat(" Writing workbooks\n")
cat(strrep("-", .pg_width), "\n", sep = "")

cat(sprintf("  Writing: %s\n", all_action$out_path))
save_workbook_retry(wb_all, path = all_action$out_path)
report("+", sprintf("Saved: %s  (%s)",
                    basename(all_action$out_path),
                    fmt_size(file.info(all_action$out_path)$size)))

cat(sprintf("  Writing: %s\n", review_action$out_path))
save_workbook_retry(wb_review, path = review_action$out_path)
report("+", sprintf("Saved: %s  (%s)",
                    basename(review_action$out_path),
                    fmt_size(file.info(review_action$out_path)$size)))


# ============================================================
# SECTION 9: Run Summary
# ============================================================

cat(sprintf("\n%s\n", strrep("=", .pg_width)))
cat(" Consolidation complete\n")
cat(strrep("=", .pg_width), "\n", sep = "")

# Per-target row count summary (printed for both workbooks)
summarise_workbook <- function(data, label) {
  if (nrow(data) == 0) {
    cat(sprintf("\n  %s: no data\n", label))
    return(invisible(NULL))
  }
  tbl <- data |>
    count(Target, name = "rows") |>
    arrange(Target)
  cat(sprintf("\n  %s — %d row(s) across %d target(s):\n",
              label, nrow(data), nrow(tbl)))
  print(tbl, n = Inf)
}

summarise_workbook(all_data,    "All-samples")
summarise_workbook(review_data, "Review-samples")

cat(sprintf("\n  Output files:\n"))
cat(sprintf("    %s\n", all_action$out_path))
cat(sprintf("    %s\n", review_action$out_path))


# ============================================================
# SECTION 10: Close Run Log
# ============================================================

if (!is.null(CONSOLIDATION_LOG_PATH) && sink.number() > 0) {
  .log_end <- Sys.time()
  .log_dur <- as.numeric(difftime(.log_end, .log_start, units = "secs"))

  cat(sprintf("\n%s\n", strrep("=", .pg_width)))
  cat(" End of consolidation log\n")
  cat(strrep("-", .pg_width), "\n", sep = "")
  cat(sprintf(" Finished : %s\n", format(.log_end, "%Y-%m-%d %H:%M:%S")))
  cat(sprintf(" Duration : %.1fs\n", .log_dur))
  cat(sprintf(" Log file : %s\n", CONSOLIDATION_LOG_PATH))
  cat(strrep("=", .pg_width), "\n", sep = "")

  sink(type = "output")
  close(.log_con)
  cat(sprintf("\n  Consolidation log saved to: %s\n", CONSOLIDATION_LOG_PATH))
}