# ============================================================
#  qPCR Results Consolidation - R Package Version
# ============================================================

# Version and config parameters are now handled by package function arguments.

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
# build_decision_summary
#
# Reads the cleaning pipeline's decision log from AUDIT_DIR,
# filters to the most recent run_id per plate, then pivots to
# a wide table with plate stems as rows and rule_ids as columns
# (counts as values, 0 where a rule did not fire).
#
# Returns NULL (with a console warning) if the log cannot be
# found or read.
# ----------------------------------------------------------
build_decision_summary <- function(audit_dir) {
  if (is.null(audit_dir)) return(NULL)

  dec_files <- list.files(audit_dir, pattern = "pcr_decisions.*\\.csv$",
                          full.names = TRUE)
  if (length(dec_files) == 0) {
    report("!", "No decision log found in AUDIT_DIR — decision breakdown omitted from run_summary")
    return(NULL)
  }
  # If multiple files exist, use the most recently modified
  dec_file <- dec_files[which.max(file.info(dec_files)$mtime)]
  report("+", sprintf("Decision log : %s", basename(dec_file)))

  dec <- tryCatch(
    read_csv(dec_file, show_col_types = FALSE, progress = FALSE,
             col_types = cols(.default = col_character())),
    error = function(e) {
      report("x", sprintf("Could not read decision log: %s", conditionMessage(e)))
      NULL
    }
  )
  if (is.null(dec) || nrow(dec) == 0) return(NULL)

  # Most recent run_id per input_file (run_id is YYYYMMDD_HHMMSS_<stem>,
  # so lexicographic max is chronologically most recent)
  dec_recent <- dec |>
    group_by(input_file) |>
    filter(run_id == max(run_id)) |>
    ungroup()

  dec_recent |>
    mutate(plate = sub("\\.csv$", "", input_file, ignore.case = TRUE)) |>
    count(plate, rule_id) |>
    pivot_wider(names_from = rule_id, values_from = n, values_fill = 0L) |>
    arrange(plate)
}


# ----------------------------------------------------------
# build_standards_summary
#
# Reads the cleaning pipeline's standards log from AUDIT_DIR,
# filters to the most recent run_id per plate, and returns a
# tidy table showing each plate's action (pass/skipped/forced),
# any missing standards, LOD overrides (for forced plates),
# and notes.
#
# Returns NULL if the log cannot be found or read.
# ----------------------------------------------------------
build_standards_summary <- function(audit_dir) {
  if (is.null(audit_dir)) return(NULL)

  std_files <- list.files(audit_dir, pattern = "pcr_standards.*\\.csv$",
                          full.names = TRUE)
  if (length(std_files) == 0) {
    report("!", "No standards log found in AUDIT_DIR — standards summary omitted from run_summary")
    return(NULL)
  }
  std_file <- std_files[which.max(file.info(std_files)$mtime)]
  report("+", sprintf("Standards log: %s", basename(std_file)))

  std <- tryCatch(
    read_csv(std_file, show_col_types = FALSE, progress = FALSE,
             col_types = cols(.default = col_character())),
    error = function(e) {
      report("x", sprintf("Could not read standards log: %s", conditionMessage(e)))
      NULL
    }
  )
  if (is.null(std) || nrow(std) == 0) return(NULL)

  std |>
    group_by(input_file) |>
    filter(run_id == max(run_id)) |>
    ungroup() |>
    mutate(plate = sub("\\.csv$", "", input_file, ignore.case = TRUE)) |>
    select(plate, action, missing_standards, lod_override, notes) |>
    arrange(plate)
}


# ----------------------------------------------------------
# build_count_pivot
#
# Builds a plate x target count table from a data frame that
# is already in memory (all_data or review_data).  Uses
# .sheet_target (canonical name after alias resolution) as the
# column axis so counts align with the sheet tabs.
#
# Plate# is kept numeric for sorting, then converted to
# character; NA plate numbers are shown as "Unknown".
# Zero-fills missing plate/target combinations.
#
# Returns NULL if `data` has zero rows.
# ----------------------------------------------------------
build_count_pivot <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(NULL)

  sheet_col <- if (".sheet_target" %in% names(data)) ".sheet_target" else "Target"

  data |>
    mutate(
      plate_sort = `Plate#`,   # retain numeric for sorting before stringify
      `Plate#`   = if_else(is.na(`Plate#`), "Unknown", as.character(`Plate#`))
    ) |>
    count(`Plate#`, plate_sort, .data[[sheet_col]]) |>
    rename(Target = all_of(sheet_col)) |>
    pivot_wider(names_from = Target, values_from = n, values_fill = 0L) |>
    arrange(plate_sort, `Plate#`) |>
    select(-plate_sort)
}


# ----------------------------------------------------------
# add_summary_sheet
#
# Inserts a "run_summary" sheet as the FIRST sheet of the
# workbook containing four stacked labelled tables:
#
#   1. Decision Breakdown      — plate x rule_id counts
#   2. Standards Check Results — plate-level action + LOD info
#   3. Final Sample Counts     — plate x target (all_data)
#   4. Review Sample Counts    — plate x target (review_data)
#
# Tables with no data are replaced by a single "(no data)"
# notice row.  A 2-row gap and bold section-header row
# separate each table.
#
# Args:
#   wb            : openxlsx Workbook (target sheets already added)
#   decision_wide : output of build_decision_summary(), or NULL
#   standards_tbl : output of build_standards_summary(), or NULL
#   all_counts    : output of build_count_pivot(all_data), or NULL
#   review_counts : output of build_count_pivot(review_data), or NULL
# ----------------------------------------------------------
add_summary_sheet <- function(wb, decision_wide, standards_tbl,
                              all_counts, review_counts,
                              font_name, font_size, col_width_min) {

  sn <- "run_summary"
  addWorksheet(wb, sheetName = sn)

  # ---- Styles ----
  section_style <- createStyle(
    fontName       = font_name,
    fontSize       = font_size + 1L,
    textDecoration = "bold",
    fgFill         = "#B8CCE4",          # light steel blue
    border         = "Bottom",
    borderColour   = "#2E5F8A"
  )
  header_style <- createStyle(
    fontName       = font_name,
    fontSize       = font_size,
    textDecoration = "bold",
    fgFill         = "#4472C4",          # medium blue
    fontColour     = "#FFFFFF"
  )
  data_style <- createStyle(
    fontName = font_name,
    fontSize = font_size
  )
  int_style <- createStyle(
    fontName = font_name,
    fontSize = font_size,
    numFmt   = "0"
  )
  notice_style <- createStyle(
    fontName      = font_name,
    fontSize      = font_size,
    fontColour    = "#7F7F7F",
    textDecoration = "italic"
  )

  current_row <- 1L

  # ----------------------------------------------------------
  # write_block: writes one titled table starting at current_row.
  # Returns the next available row (after a 2-row gap).
  # ----------------------------------------------------------
  write_block <- function(title, tbl, numeric_cols = character(0)) {

    # Section header spanning full width
    writeData(wb, sn, x = title,
              startRow = current_row, startCol = 1L, colNames = FALSE)
    addStyle(wb, sn, style = section_style,
             rows = current_row, cols = 1L, stack = FALSE)

    data_start <- current_row + 1L

    if (is.null(tbl) || nrow(tbl) == 0) {
      writeData(wb, sn, x = "(no data available)",
                startRow = data_start, startCol = 1L, colNames = FALSE)
      addStyle(wb, sn, style = notice_style,
               rows = data_start, cols = 1L, stack = FALSE)
      return(data_start + 3L)   # title + notice + 2-row gap
    }

    # Header row
    writeData(wb, sn, x = tbl,
              startRow = data_start, startCol = 1L,
              colNames = TRUE, keepNA = FALSE, na.string = "")
    addStyle(wb, sn, style = header_style,
             rows = data_start,
             cols = seq_len(ncol(tbl)),
             stack = FALSE)

    # Data rows
    n_data <- nrow(tbl)
    data_rows <- (data_start + 1L):(data_start + n_data)
    addStyle(wb, sn, style = data_style,
             rows = data_rows,
             cols = seq_len(ncol(tbl)),
             gridExpand = TRUE, stack = FALSE)

    # Integer formatting for specified columns
    for (nc in intersect(numeric_cols, names(tbl))) {
      col_idx <- which(names(tbl) == nc)
      addStyle(wb, sn, style = int_style,
               rows = data_rows, cols = col_idx,
               gridExpand = TRUE, stack = TRUE)
    }

    data_start + n_data + 3L   # header + data + 2-row gap
  }

  # Derive integer column names for count tables (everything except Plate#)
  count_int_cols <- function(tbl) {
    if (is.null(tbl)) return(character(0))
    setdiff(names(tbl), "Plate#")
  }

  # Derive integer column names for decision table (everything except plate)
  dec_int_cols <- function(tbl) {
    if (is.null(tbl)) return(character(0))
    setdiff(names(tbl), "plate")
  }

  current_row <- write_block(
    "1. Decision Breakdown  (most recent run per plate — counts by rule)",
    decision_wide,
    numeric_cols = dec_int_cols(decision_wide)
  )
  current_row <- write_block(
    "2. Standards Check Results  (most recent run per plate)",
    standards_tbl
  )
  current_row <- write_block(
    "3. Final Sample Counts  (all_samples — by Plate# and Target)",
    all_counts,
    numeric_cols = count_int_cols(all_counts)
  )
  current_row <- write_block(
    "4. Review Sample Counts  (review_samples — by Plate# and Target)",
    review_counts,
    numeric_cols = count_int_cols(review_counts)
  )

  # ---- Column widths ----
  # All four tables start at column 1, so widths are set once for the widest
  # table. For each column position we take the maximum header-name length
  # across all tables that reach that position, then apply the minimum floor.
  # This replaces the old approach (unique header names in arbitrary order)
  # which assigned widths to positions that had no relationship to their content.
  all_tbls  <- Filter(Negate(is.null), list(decision_wide, standards_tbl,
                                             all_counts, review_counts))
  max_cols  <- if (length(all_tbls) > 0L)
    max(vapply(all_tbls, ncol, integer(1L)))
  else 1L

  col_widths <- vapply(seq_len(max_cols), function(j) {
    # Collect header names at position j from every table that is wide enough
    hdrs <- vapply(all_tbls, function(tbl) {
      if (ncol(tbl) >= j) nchar(names(tbl)[[j]]) else 0L
    }, integer(1L))
    max(col_width_min, max(hdrs))
  }, numeric(1L))

  setColWidths(wb, sn,
               cols   = seq_len(max_cols),
               widths = col_widths)

  # ---- Move run_summary to sheet position 1 ----
  n_sheets <- length(wb$worksheets)
  worksheetOrder(wb) <- c(n_sheets, seq_len(n_sheets - 1L))

  invisible(wb)
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
      # Suppress the transient openxlsx unzip notice that fires on temp-file
      # cleanup — it is advisory-only and carries no data-loss risk.
      # All other warnings are left unaffected (not promoted to errors).
      withCallingHandlers(
        saveWorkbook(wb, file = path, overwrite = overwrite),
        warning = function(w) {
          if (grepl("unzip|cannot unzip|temp", conditionMessage(w),
                    ignore.case = TRUE, perl = TRUE))
            invokeRestart("muffleWarning")
          # Any other warning propagates normally
        }
      )
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
  # Matches YYYYMMDD (8 digits), YYMMDD (6 digits), or separator-delimited
  # variants at the start of the filename stem followed by an underscore.
  m <- regexec(
    "^((?:\\d{8}|\\d{6}|\\d{4}[-/]\\d{2}[-/]\\d{2}|\\d{2}[-/]\\d{2}[-/]\\d{4}))_",
    stem
  )
  res <- regmatches(stem, m)[[1]]

  date_raw <- if (length(res) >= 2) res[2] else NA_character_

  # Try all known formats.
  # Format list is chosen based on the width of date_raw so that a 6-digit
  # string (YYMMDD) is always tried against %y%m%d before %Y%m%d.
  # Without this guard, as.Date("241225", "%Y%m%d") succeeds and returns
  # the year 0024 rather than 2024, because R does not enforce field widths.
  parsed_date <- if (!is.na(date_raw) && nzchar(date_raw)) {
    fmts <- if (nchar(date_raw) == 6L)
      c("%y%m%d", "%d%m%y")
    else
      c("%Y%m%d", "%Y-%m-%d", "%Y/%m/%d", "%d-%m-%Y", "%d/%m/%Y")
    parsed <- NA_character_   # character NA so parsed_date is always character type

    for (fmt in fmts) {
      try({
        tmp <- as.Date(date_raw, format = fmt)
        if (!is.na(tmp)) {
          parsed <- format(tmp, "%Y-%m-%d")
          break
        }
      }, silent = TRUE)
    }

    parsed
  } else {
    NA_character_
  }

  # --- Plate number (bulletproof) ---
  plate_pattern <- "(?ix)
  plate              # literal word 'plate' (case-insensitive)
  \\s*               # optional whitespace
  (?:                # optional separators or qualifiers
      [#:_-]?        # optional punctuation (#, :, _, -, or nothing)
      \\s*           # optional whitespace
      (?:no|num|number)?  # optional 'no' / 'num' / 'number'
      \\.?           # optional dot after 'no.'
      \\s*           # optional whitespace
  )?
  (\\d{1,3})         # capture the plate number (1–3 digits)
"

  plate_match <- regexec(plate_pattern, stem, perl = TRUE)
  pm <- regmatches(stem, plate_match)[[1]]

  plate_num <- if (length(pm) >= 2) {
    as.integer(pm[2])
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
# Wrapped in tryCatch so a locked or momentarily unreachable
# network path returns a graceful notice rather than an error.
# ----------------------------------------------------------
wb_stats <- function(path) {
  tryCatch({
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
  }, error = function(e) {
    sprintf("  (could not read file info: %s)", conditionMessage(e))
  })
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


#' Run the qPCR Consolidation Pipeline
#'
#' Gathers cleaned CSV output files into structured Excel workbooks.
#'
#' @param input_dir Folder containing cleaned CSVs (defaults to "outputs").
#' @param all_pattern Regex pattern used to identify "all_samples" CSV files.
#' @param review_pattern Regex pattern used to identify "review_samples" CSV files.
#' @param consolidation_dir Folder where consolidated workbooks are written.
#' @param all_out_path Output path for all-samples consolidated Excel workbook.
#' @param review_out_path Output path for review-flagged consolidated Excel workbook.
#' @param table_style Excel built-in table style applied to data tables.
#' @param font_name Font name applied to Excel cells.
#' @param font_size Font size in points.
#' @param col_width_min Minimum column width in Excel.
#' @param max_write_tries Maximum save attempts before giving up.
#' @param wait_secs Seconds to wait between file-save retries.
#' @param target_aliases List of target name mapping lists.
#' @param update_target_to_canonical Set to TRUE to update Target to the canonical name.
#' @param audit_dir Location of the audit folder written by the cleaning pipeline.
#' @param consolidation_log_path Plain-text transcript log path.
#' @return Invisibly returns the `consolidation_dir` path, allowing for piping.
#' @export
run_consolidation_pipeline <- function(
  input_dir = "outputs/",
  all_pattern = "_all_samples\\.csv$",
  review_pattern = "_review_samples\\.csv$",
  consolidation_dir = "consolidated",
  all_out_path = NULL,
  review_out_path = NULL,
  table_style = "TableStyleMedium2",
  font_name = "Arial",
  font_size = 10,
  col_width_min = 10,
  max_write_tries = 5,
  wait_secs = 3,
  target_aliases = list(uni = c("uni", "universal", "univ")),
  update_target_to_canonical = TRUE,
  audit_dir = "audit/",
  consolidation_log_path = "audit/pcr_consolidation_log.txt"
) {
  # Close any stale sinks from a previous pipeline stage
  while (sink.number() > 0) sink(type = "output")

  # Assign lowercase function arguments to uppercase local variables
  # to match the existing script naming conventions.
  input_dir <- input_dir
  ALL_PATTERN <- all_pattern
  REVIEW_PATTERN <- review_pattern
  CONSOLIDATION_DIR <- consolidation_dir
  TABLE_STYLE <- table_style
  FONT_NAME <- font_name
  FONT_SIZE <- font_size
  COL_WIDTH_MIN <- col_width_min
  MAX_WRITE_TRIES <- max_write_tries
  WAIT_SECS <- wait_secs
  TARGET_ALIASES <- target_aliases
  UPDATE_TARGET_TO_CANONICAL <- update_target_to_canonical
  AUDIT_DIR <- audit_dir
  CONSOLIDATION_LOG_PATH <- consolidation_log_path

  # Resolve paths
  ALL_OUT_PATH <- if (is.null(all_out_path)) {
    file.path(CONSOLIDATION_DIR, "qpcr_all_samples.xlsx")
  } else {
    all_out_path
  }
  REVIEW_OUT_PATH <- if (is.null(review_out_path)) {
    file.path(CONSOLIDATION_DIR, "qpcr_review_samples.xlsx")
  } else {
    review_out_path
  }


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
  cat(sprintf(" Input    : %s\n", input_dir))
  cat(sprintf(" All out  : %s\n", ALL_OUT_PATH))
  cat(sprintf(" Review   : %s\n", REVIEW_OUT_PATH))
  cat(sprintf(" Version  : %s\n", packageVersion("qpcrpipeline")))
  cat(strrep("=", .pg_width), "\n\n", sep = "")
}


# ============================================================
# SECTION 5: Discover Input Files
# ============================================================

cat(strrep("-", .pg_width), "\n", sep = "")
cat(" File discovery\n")
cat(strrep("-", .pg_width), "\n", sep = "")

if (!dir.exists(input_dir)) {
  stop("input_dir does not exist: ", input_dir,
       "\nCheck that the cleaning pipeline has been run and output_dir matches.",
       call. = FALSE)
}

all_files    <- sort(list.files(input_dir, pattern = ALL_PATTERN,
                                full.names = TRUE, recursive = TRUE))
review_files <- sort(list.files(input_dir, pattern = REVIEW_PATTERN,
                                full.names = TRUE, recursive = TRUE))

if (length(all_files) == 0 && length(review_files) == 0) {
  stop(
    "No matching files found in: ", input_dir,
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
  "File Name", "Date", "Plate#", "Repeat#",           # added by this script
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

    # --- Extract repeat number from filename ---
    # perl = TRUE is required so that the inline (?i) case-insensitivity flag
    # is honoured — R's default TRE engine silently ignores PCRE inline flags.
    repeat_pattern <- "(?i)(rpt|rep|repeat)\\s*([0-9]*)"
    rp             <- regexec(repeat_pattern, stem, perl = TRUE)
    repeat_match   <- regmatches(stem, rp)[[1]]   # renamed from 'rm' to avoid shadowing base rm()

    repeat_num <- NA_integer_

    if (length(repeat_match) >= 2) {
      # repeat_match[2] = rpt/rep/repeat keyword
      # repeat_match[3] = the trailing number (if present)
      if (nzchar(repeat_match[3])) {
        repeat_num <- as.integer(repeat_match[3])
      } else {
        # Found keyword but no number — treat as repeat 1
        repeat_num <- 1L
      }
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
        `Repeat#`   = repeat_num,
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

  # Exclude the run_summary sheet — it contains audit tables, not
  # sample data, and cannot be bound with the target data sheets.
  sheet_names <- sheet_names[sheet_names != "run_summary"]

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

      # RISK-06: .sheet_target is dropped before writing to Excel, so it must
      # be rebuilt here so build_workbook() can route rows to the right sheet.
      # Re-apply alias resolution using the same TARGET_ALIASES as the current
      # run (assumes aliases are stable between runs — documented in Section 1).
      if (!".sheet_target" %in% names(df)) {
        if (length(TARGET_ALIASES) > 0 && "Target" %in% names(df)) {
          alias_lookup <- unlist(lapply(names(TARGET_ALIASES), function(cn) {
            setNames(rep(cn, length(TARGET_ALIASES[[cn]])),
                     tolower(TARGET_ALIASES[[cn]]))
          }))
          tgt_lower <- tolower(df$Target)
          df$.sheet_target <- dplyr::coalesce(alias_lookup[tgt_lower], tgt_lower)
        } else {
          df$.sheet_target <- tolower(df$Target)
        }
      }
    }
    df
  })

  bind_rows(sheets)
}


# ----------------------------------------------------------
# Helper: deduplicate a combined data frame after append.
# Key: File Name + Well + Fluor + Target + Sample.
# Keeps the first occurrence so existing rows take precedence
# over re-reads of the same plate from the new data.
# ----------------------------------------------------------
.dedup_append <- function(combined, label) {
  key_cols <- c("File Name", "Well", "Fluor", "Target", "Sample")
  present  <- intersect(key_cols, names(combined))

  if (length(present) < length(key_cols)) {
    report("!", sprintf(
      "%s: deduplication skipped — key column(s) missing: %s",
      label, paste(setdiff(key_cols, present), collapse = ", ")
    ))
    return(combined)
  }

  n_before  <- nrow(combined)
  combined  <- combined |> distinct(across(all_of(key_cols)), .keep_all = TRUE)
  n_removed <- n_before - nrow(combined)

  if (n_removed > 0)
    report("~", sprintf(
      "%s: removed %d duplicate row(s) after append (key: %s)",
      label, n_removed, paste(key_cols, collapse = " + ")
    ))

  combined
}


# If appending, merge existing data with newly read data then deduplicate
if (all_action$action == "append" && file.exists(all_action$out_path)) {
  existing_all <- load_existing_sheets(all_action$out_path)
  n_existing   <- nrow(existing_all)
  all_data     <- .dedup_append(bind_rows(existing_all, all_data), "all_samples")
  report("~", sprintf(
    "All-samples append: %d existing row(s) + %d new row(s) = %d total (after dedup)",
    n_existing, nrow(all_data) - n_existing, nrow(all_data)
  ))
}

if (review_action$action == "append" && file.exists(review_action$out_path)) {
  existing_review <- load_existing_sheets(review_action$out_path)
  n_existing      <- nrow(existing_review)
  review_data     <- .dedup_append(bind_rows(existing_review, review_data), "review_samples")
  report("~", sprintf(
    "Review-samples append: %d existing row(s) + %d new row(s) = %d total (after dedup)",
    n_existing, nrow(review_data) - n_existing, nrow(review_data)
  ))
}


# ============================================================
# SECTION 7.5: Load Audit Data for run_summary Sheet
# ============================================================
# Reads the cleaning pipeline's decision log and standards log
# from AUDIT_DIR.  Both are filtered to the most recent run_id
# per plate so the summary reflects the same data that is in
# the output CSVs (i.e. the last time each plate was processed).
#
# Count pivots for the sample/review tables are built directly
# from all_data and review_data (already in memory).
#
# All four objects are passed to add_summary_sheet() later.
# If AUDIT_DIR is NULL or a log is missing, the corresponding
# table is omitted gracefully with a [!] console notice.
# ============================================================

cat(sprintf("\n%s\n", strrep("-", .pg_width)))
cat(" Loading audit data for run_summary sheet\n")
cat(strrep("-", .pg_width), "\n", sep = "")

if (!is.null(AUDIT_DIR) && !dir.exists(AUDIT_DIR)) {
  report("!", sprintf("AUDIT_DIR not found: '%s' — audit tables will be empty", AUDIT_DIR))
  AUDIT_DIR <- NULL
}

.summary_decision  <- build_decision_summary(AUDIT_DIR)
.summary_standards <- build_standards_summary(AUDIT_DIR)
.summary_all_counts    <- build_count_pivot(all_data)
.summary_review_counts <- build_count_pivot(review_data)

report("+", sprintf("Decision breakdown : %s",
                    if (is.null(.summary_decision)) "no data"
                    else sprintf("%d plate(s) x %d rule(s)",
                                 nrow(.summary_decision), ncol(.summary_decision) - 1L)))
report("+", sprintf("Standards summary  : %s",
                    if (is.null(.summary_standards)) "no data"
                    else sprintf("%d plate(s)", nrow(.summary_standards))))
report("+", sprintf("Final counts pivot : %s",
                    if (is.null(.summary_all_counts)) "no data"
                    else sprintf("%d plate(s) x %d target(s)",
                                 nrow(.summary_all_counts), ncol(.summary_all_counts) - 1L)))
report("+", sprintf("Review counts pivot: %s",
                    if (is.null(.summary_review_counts)) "no data"
                    else sprintf("%d plate(s) x %d target(s)",
                                 nrow(.summary_review_counts), ncol(.summary_review_counts) - 1L)))


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
    # Strip NAs from the character-length vector before calling max() so
    # that fully-NA columns (e.g. Review Reason on a clean plate) never
    # produce a "no non-missing arguments to max" warning.
    col_widths <- pmax(
      COL_WIDTH_MIN,
      vapply(names(tgt_data), function(cn) {
        vals      <- nchar(as.character(head(tgt_data[[cn]], 100)))
        vals      <- vals[!is.na(vals)]
        content_w <- if (length(vals) > 0) max(vals) else 0L
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
# Prepend run_summary sheet to both workbooks
# ----------------------------------------------------------
cat(sprintf("\n%s\n", strrep("-", .pg_width)))
cat(" Building run_summary sheet\n")
cat(strrep("-", .pg_width), "\n", sep = "")

add_summary_sheet(wb_all,
                  decision_wide  = .summary_decision,
                  standards_tbl  = .summary_standards,
                  all_counts     = .summary_all_counts,
                  review_counts  = .summary_review_counts,
                  font_name      = font_name, 
                  font_size      = font_size, 
                  col_width_min  = col_width_min)
report("+", "run_summary sheet added to all-samples workbook")

add_summary_sheet(wb_review,
                  decision_wide  = .summary_decision,
                  standards_tbl  = .summary_standards,
                  all_counts     = .summary_all_counts,
                  review_counts  = .summary_review_counts,
                  font_name      = font_name, 
                  font_size      = font_size, 
                  col_width_min  = col_width_min)
report("+", "run_summary sheet added to review-samples workbook")


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

# Per-target row count summary (printed for both workbooks).
# Uses .sheet_target (canonical sheet name) so the counts shown here
# match the sheet tabs in the workbook, even when UPDATE_TARGET_TO_CANONICAL
# is FALSE and the Target column still holds the original alias values.
summarise_workbook <- function(data, label) {
  if (nrow(data) == 0) {
    cat(sprintf("\n  %s: no data\n", label))
    return(invisible(NULL))
  }
  display_col <- if (".sheet_target" %in% names(data)) ".sheet_target" else "Target"
  tbl <- data |>
    mutate(.display = .data[[display_col]]) |>
    count(.display, name = "rows") |>
    rename(Sheet = .display) |>
    arrange(Sheet)
  cat(sprintf("\n  %s — %d row(s) across %d sheet(s):\n",
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
  invisible(CONSOLIDATION_DIR)
}