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
INPUT_DIR    <- "data_raw/"   # Folder containing plate CSV files.
FILE_PATTERN <- "\\.csv$"    # Regex: which files to include (matched against filename).
FILES        <- NULL         # Optional: explicit character vector of file paths.
# If set, INPUT_DIR, FILE_PATTERN, and SEARCH_DEPTH
# are all ignored.
# e.g. FILES <- c("data_raw/plate1.csv", "data_raw/plate2.csv")

SEARCH_DEPTH <- 0            # How many subfolder levels to search within INPUT_DIR.
#   0 = only files directly inside INPUT_DIR
#   1 = INPUT_DIR + one level of subfolders
#   2 = INPUT_DIR + two levels of subfolders, etc.
# Safe to set higher than your actual folder depth —
# the script will just use whatever levels exist.

# --- File tree ---
# A tree of all matching files found is produced before processing begins.
# It shows the folder structure and which files will be processed.
TREE_OUTPUT  <- "both"                 # Where to send the tree: "console", "file", or "both".
TREE_PATH    <- "audit/file_tree.txt"  # Destination when TREE_OUTPUT is "file" or "both".

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

# --- Resume / skip completed plates ---
# Set to TRUE to check whether output CSVs already exist for each plate before
# processing. Plates where both <stem>_all_samples.csv AND
# <stem>_review_samples.csv already exist in OUTPUT_DIR will be listed and you
# will be asked whether to skip them (Y) or reprocess them (N).
# If the script is run non-interactively (e.g. scheduled via Rscript), it
# defaults to N — reprocess everything — and logs that no prompt was possible.
SKIP_COMPLETED <- TRUE

# --- Standards check ---
# Before any data is removed, checks that every expected standard is present
# in the Content column of each plate.  Both canonical labelling (Std-001,
# Std-002 …) and abbreviated labelling (Std-01, Std-02 …) are accepted; all
# found labels are normalised to Std-NNN internally for comparison.
# Plates whose standards use the abbreviated (01/02) style will produce a
# console warning and an audit-log note, but will still be processed provided
# no standards are actually missing.
# Plates with missing standards are gathered and you will be asked whether
# to skip them or force them through with manually specified LOD overrides.
STD_CHECK_ENABLED <- TRUE   # Set to FALSE to bypass the check entirely.
N_STANDARDS       <- 6      # How many standards to expect (Std-001 … Std-006).

# Per-target LOD overrides for plates forced through the standards check.
# Keys must be lowercase target names matching your CSV files.
# Required in non-interactive sessions; also offered as defaults interactively.
# Set to NULL to be prompted to enter every value manually each time.
STD_FORCE_LOD <- NULL
# Example:
# STD_FORCE_LOD <- list(
#   LOD_Hi = list(nuc = 2000, fucp = 2000),
#   LOD_Lo = list(nuc = 0.012, fucp = 0.012)
# )

# Separate audit log that records the outcome of every standards check.
STD_LOG_PATH <- "audit/pcr_standards.csv"


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


# Depth-aware file discovery.
#
# Lists all files matching `pattern` within `dir`, up to `depth` subfolder
# levels deep. Gracefully handles a requested depth that exceeds the actual
# folder structure — it simply returns whatever exists without erroring.
#
# Args:
#   dir     : root directory to search
#   pattern : regex matched against filenames (same as FILE_PATTERN)
#   depth   : max subfolder depth (0 = root only, 1 = root + 1 level, etc.)
#
# Returns: character vector of full file paths, sorted
list_files_depth <- function(dir, pattern, depth) {
  if (!dir.exists(dir)) stop("INPUT_DIR does not exist: ", dir)

  if (depth == 0) {
    return(sort(list.files(dir, pattern = pattern,
                           full.names = TRUE, recursive = FALSE)))
  }

  all_files <- list.files(dir, pattern = pattern,
                          full.names = TRUE, recursive = TRUE)
  if (length(all_files) == 0) return(character(0))

  norm      <- function(p) gsub("\\\\", "/", p)
  root      <- norm(dir)
  if (!endsWith(root, "/")) root <- paste0(root, "/")
  root_esc  <- gsub("([.+*?|(){}\\[\\]^$])", "\\\\\\1", root)

  rel_paths  <- sub(paste0("^", root_esc), "", norm(all_files))
  file_depth <- nchar(gsub("[^/]", "", rel_paths))

  sort(all_files[file_depth <= depth])
}


# Build an ASCII file tree of the discovered files.
#
# Args:
#   files           : character vector of full file paths to display
#   root_dir        : INPUT_DIR (used as the tree root label)
#   depth_requested : SEARCH_DEPTH value (shown in the header for reference)
#
# Returns: character vector — one element per line of the rendered tree
build_file_tree <- function(files, root_dir, depth_requested) {

  norm     <- function(p) gsub("\\\\", "/", p)
  root     <- norm(root_dir)
  if (!endsWith(root, "/")) root <- paste0(root, "/")
  root_esc <- gsub("([.+*?|(){}\\[\\]^$])", "\\\\\\1", root)

  rel          <- sort(sub(paste0("^", root_esc), "", norm(files)))
  depth_actual <- if (length(rel) > 0) max(nchar(gsub("[^/]", "", rel))) else 0

  header <- sprintf(
    "%s  [depth requested: %d | deepest file: %d level(s) | %d file(s) to process]",
    basename(root_dir), depth_requested, depth_actual, length(files)
  )

  if (length(files) == 0) {
    return(c(header, "  (no matching files found)"))
  }

  render_node <- function(paths, prefix) {
    if (length(paths) == 0) return(character(0))

    heads   <- sub("/.*$", "", paths)
    is_file <- !grepl("/", paths, fixed = TRUE)
    tails   <- ifelse(is_file, NA_character_, sub("^[^/]*/", "", paths))

    unique_heads <- unique(heads)
    lines        <- character(0)

    for (i in seq_along(unique_heads)) {
      h        <- unique_heads[[i]]
      is_last  <- (i == length(unique_heads))
      conn     <- if (is_last) "\u2514\u2500\u2500 " else "\u251c\u2500\u2500 "
      child_px <- if (is_last) paste0(prefix, "    ") else paste0(prefix, "\u2502   ")
      idx      <- which(heads == h)

      if (all(is_file[idx])) {
        lines <- c(lines, paste0(prefix, conn, h))
      } else {
        children <- tails[idx]
        children <- children[!is.na(children)]
        lines    <- c(lines, paste0(prefix, conn, h, "/"))
        lines    <- c(lines, render_node(children, child_px))
      }
    }
    lines
  }

  c(header, render_node(rel, ""))
}


# Print and/or save the file tree.
#
# Args:
#   tree_lines  : character vector returned by build_file_tree()
#   output_mode : "console", "file", or "both"
#   tree_path   : destination .txt path (used when output_mode is "file" or "both")
emit_file_tree <- function(tree_lines, output_mode, tree_path) {
  valid_modes <- c("console", "file", "both")
  if (!output_mode %in% valid_modes) {
    stop('TREE_OUTPUT must be one of: "console", "file", or "both". Got: "', output_mode, '"')
  }

  if (output_mode %in% c("console", "both")) {
    cat(paste(tree_lines, collapse = "\n"), "\n\n")
  }

  if (output_mode %in% c("file", "both")) {
    if (!dir.exists(dirname(tree_path))) {
      dir.create(dirname(tree_path), recursive = TRUE, showWarnings = FALSE)
    }
    writeLines(
      c(paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), "", tree_lines),
      tree_path
    )
    cat("  File tree saved to:", tree_path, "\n\n")
  }
}


# -------------------------------------------------------
# Standards check helpers
# -------------------------------------------------------

# Reads a plate file and verifies that all expected Std-001 … Std-NNN entries
# are present in the Content column.  Uses the same dynamic header detection
# as process_plate() so results are consistent with what the pipeline will see.
#
# Args:
#   file       : path to the plate CSV
#   n_expected : integer; N_STANDARDS from Section 1
#
# Returns a named list:
#   $passed       : TRUE if all standards found (after normalisation)
#   $expected     : character vector of expected standard names (canonical Std-NNN form)
#   $found        : character vector of standard names exactly as they appear in the file
#   $found_norm   : character vector of found names normalised to canonical Std-NNN form
#   $missing      : character vector of standard names absent (canonical form)
#   $targets      : character vector of unique (lowercased) target names in the plate
#   $label_style  : "standard" (all Std-001), "short" (all Std-01), "mixed", or "unknown"
#   $label_warning: NULL when style is canonical; descriptive string otherwise
#   $error        : NULL on success; error message string if the file could not be read
check_plate_standards <- function(file, n_expected) {
  tryCatch({
    raw_full <- read_csv(file, show_col_types = FALSE, col_names = FALSE,
                         col_types = cols(.default = col_character()))
    well_row <- which(raw_full[[1]] == "Well")

    if (length(well_row) == 0)
      return(list(passed = FALSE, expected = character(), found = character(),
                  found_norm = character(), missing = character(),
                  targets = character(), label_style = "unknown",
                  label_warning = NULL,
                  error = "Could not find 'Well' header row"))

    well_row      <- well_row[1]
    col_headers   <- as.character(raw_full[well_row, ])
    raw           <- raw_full[(well_row + 1):nrow(raw_full), ]
    colnames(raw) <- col_headers
    raw           <- clean_col_names(raw)

    if (!"content" %in% names(raw))
      return(list(passed = FALSE, expected = character(), found = character(),
                  found_norm = character(), missing = character(),
                  targets = character(), label_style = "unknown",
                  label_warning = NULL,
                  error = "No 'content' column found after header detection"))

    std_vals <- as.character(raw$content)

    # Detect standards in either Std-001 (3-digit) or Std-01 (2-digit) format.
    std_present_raw <- sort(unique(std_vals[str_detect(std_vals,
                                                       regex("^Std-0{0,2}[1-9]\\d?$",
                                                             ignore_case = TRUE))]))

    # Normalise all found labels to canonical 3-digit form (Std-001) so that
    # both "Std-01" and "Std-001" resolve identically during the missing check.
    normalize_std_label <- function(s) {
      num <- suppressWarnings(
        as.integer(sub("(?i)^std-0*", "", s, perl = TRUE))
      )
      sprintf("Std-%03d", num)
    }
    std_present_norm <- if (length(std_present_raw) > 0)
      vapply(std_present_raw, normalize_std_label, character(1), USE.NAMES = FALSE)
    else
      character(0)

    std_expected <- sprintf("Std-%03d", seq_len(n_expected))
    std_missing  <- std_expected[!tolower(std_expected) %in% tolower(std_present_norm)]

    # Classify label style: "standard" = all 3-digit, "short" = all ≤2-digit, "mixed".
    is_short_fmt <- grepl("^Std-\\d{1,2}$", std_present_raw, ignore.case = TRUE)
    is_long_fmt  <- grepl("^Std-\\d{3,}$",  std_present_raw, ignore.case = TRUE)
    label_style  <- if      (length(std_present_raw) == 0) "unknown"
    else if (all(is_long_fmt))              "standard"
    else if (all(is_short_fmt))             "short"
    else                                    "mixed"

    # Compose a warning message for any non-canonical labelling.
    label_warning <- if (label_style %in% c("short", "mixed")) {
      sprintf(
        "Standards use abbreviated labelling ('%s' style, e.g. '%s') instead of the canonical 3-digit format (e.g. '%s'). Found: %s",
        label_style,
        std_present_raw[1],
        normalize_std_label(std_present_raw[1]),
        paste(std_present_raw, collapse = ", ")
      )
    } else {
      NULL
    }

    targets <- character(0)
    if ("target" %in% names(raw)) {
      targets <- sort(unique(tolower(trimws(as.character(raw$target)))))
      targets <- targets[!is.na(targets) & nzchar(targets)]
    }

    list(passed        = length(std_missing) == 0,
         expected      = std_expected,
         found         = std_present_raw,   # labels exactly as they appear in the file
         found_norm    = std_present_norm,  # normalised to Std-NNN for audit/reference
         missing       = std_missing,
         targets       = targets,
         label_style   = label_style,
         label_warning = label_warning,
         error         = NULL)

  }, error = function(e) {
    list(passed = FALSE, expected = character(), found = character(),
         found_norm = character(), missing = character(), targets = character(),
         label_style = "unknown", label_warning = NULL,
         error = conditionMessage(e))
  })
}


# Creates the standards audit log CSV with typed headers if it does not exist.
ensure_std_log <- function(path) {
  if (!dir.exists(dirname(path))) dir.create(dirname(path), recursive = TRUE)
  if (!file.exists(path)) {
    write_csv_retry(
      tibble(
        timestamp          = as_datetime(character()),
        user               = character(),
        run_id             = character(),
        input_file         = character(),
        n_expected         = integer(),
        expected_standards = character(),
        found_standards    = character(),
        missing_standards  = character(),
        action             = character(),  # "pass" | "skipped" | "forced" | "error"
        lod_override       = character(),  # serialised per-target LOD, or NA
        notes              = character(),
        source             = character(),
        version            = character()
      ),
      path
    )
  }
  invisible(path)
}

# Appends one row to the standards audit log.
#
# Args:
#   file         : plate file path
#   n_expected   : N_STANDARDS value used for this check
#   check_result : list returned by check_plate_standards()
#   action       : "pass" | "skipped" | "forced" | "error"
#   lod_override : per-target LOD list used for a forced plate, or NULL
#   notes        : free-text string, or NULL
#   run_id       : session run ID
#   std_log_path : STD_LOG_PATH
log_standard_check <- function(file, n_expected, check_result, action,
                               lod_override = NULL, notes = NULL,
                               run_id, std_log_path) {
  ensure_std_log(std_log_path)
  lod_str <- if (!is.null(lod_override)) .serialize_value(lod_override)$value else NA_character_
  entry <- tibble(
    timestamp          = now(tzone = "UTC"),
    user               = Sys.info()[["user"]],
    run_id             = run_id,
    input_file         = basename(file),
    n_expected         = as.integer(n_expected),
    expected_standards = paste(check_result$expected, collapse = "|"),
    found_standards    = paste(check_result$found,    collapse = "|"),
    missing_standards  = paste(check_result$missing,  collapse = "|"),
    action             = action,
    lod_override       = lod_str,
    notes              = as.character(notes %||% NA_character_),
    source             = "R_script",
    version            = "0.1.8"
  )
  write_csv_retry(entry, std_log_path, append = TRUE)
  invisible(entry)
}


# Shows a combined console summary of all plates that failed the standards check,
# then prompts the user to either skip them all or force them through.
#
# Interactive — force path: collects per-target LOD_Hi and LOD_Lo for each plate.
#   STD_FORCE_LOD values are offered as defaults (press Enter to accept).
#
# Non-interactive — always forces through using STD_FORCE_LOD.
#   Errors if STD_FORCE_LOD is NULL, since no values can be entered.
#
# Args:
#   failing_info  : list of named lists, one per failing plate:
#                   $file, $found, $missing, $targets, $error
#   n_expected    : N_STANDARDS
#   std_force_lod : STD_FORCE_LOD from Section 1
#
# Returns a named list:
#   $skip_files : character vector of file paths to remove from processing
#   $force_lods : named list of per-file LOD overrides (keyed by file path)
ask_standards_action <- function(failing_info, n_expected, std_force_lod) {

  n_fail <- length(failing_info)

  # ---- Print combined failure summary ----
  cat(sprintf("\n%s\n", strrep("-", .pg_width)))
  cat(sprintf(
    " Standards check: %d plate(s) failed (expected %d standards: %s)\n",
    n_fail, n_expected,
    paste(sprintf("Std-%03d", seq_len(n_expected)), collapse = ", ")
  ))
  cat(strrep("-", .pg_width), "\n")

  for (fi in failing_info) {
    if (!is.null(fi$error)) {
      cat(sprintf("  [ERROR] %s\n    Could not read file: %s\n\n",
                  file_stem(fi$file), fi$error))
    } else {
      cat(sprintf(
        "  %s\n    Found   : %s\n    Missing : %s\n\n",
        file_stem(fi$file),
        if (length(fi$found)   == 0) "(none)" else paste(fi$found,   collapse = ", "),
        if (length(fi$missing) == 0) "(none)" else paste(fi$missing, collapse = ", ")
      ))
    }
  }
  cat(strrep("-", .pg_width), "\n")

  # ---- Non-interactive path ----
  if (!interactive()) {
    if (is.null(std_force_lod)) {
      stop(
        "Standards check failed for ", n_fail, " plate(s) in a non-interactive session,\n",
        "but STD_FORCE_LOD is NULL.\n",
        "Set STD_FORCE_LOD in Section 1 to provide fallback LOD values, or resolve\n",
        "the missing standards in the raw data before re-running.",
        call. = FALSE
      )
    }
    cat(sprintf(
      "\n  [non-interactive] Auto-forcing %d plate(s) through with STD_FORCE_LOD.\n\n",
      n_fail
    ))
    force_lods <- setNames(
      lapply(failing_info, function(fi) std_force_lod),
      vapply(failing_info, function(fi) fi$file, character(1))
    )
    return(list(skip_files = character(0), force_lods = force_lods))
  }

  # ---- Interactive path ----
  cat(" Options:\n")
  cat("   S = Skip all failing plates (excluded from this run)\n")
  cat("   F = Force all failing plates through (you will supply LOD overrides)\n")
  cat(" Enter S or F: ")

  response <- toupper(trimws(readline()))
  while (!response %in% c("S", "F")) {
    cat("  Please enter S or F: ")
    response <- toupper(trimws(readline()))
  }

  if (response == "S") {
    skip_files <- vapply(failing_info, function(fi) fi$file, character(1))
    cat(sprintf("  Skipping %d plate(s).\n\n", length(skip_files)))
    return(list(skip_files = skip_files, force_lods = list()))
  }

  # ---- Force: collect per-target LOD for each plate ----
  cat(sprintf(
    "\n  You will now enter LOD overrides for each plate.\n  (STD_FORCE_LOD %s — shown as [default] where applicable)\n\n",
    if (is.null(std_force_lod)) "is NOT defined" else "is defined"
  ))

  force_lods <- list()

  for (fi in failing_info) {
    stem_label <- file_stem(fi$file)
    cat(sprintf("  --- %s ---\n", stem_label))

    if (length(fi$targets) == 0) {
      cat("  Warning: no targets found in this plate.\n")
      if (is.null(std_force_lod))
        stop("Cannot force '", stem_label, "': no targets found and STD_FORCE_LOD is NULL.",
             call. = FALSE)
      cat("  Using STD_FORCE_LOD directly.\n\n")
      force_lods[[fi$file]] <- std_force_lod
      next
    }

    lod_hi_out <- list()
    lod_lo_out <- list()

    for (tgt in fi$targets) {
      def_hi <- tryCatch(std_force_lod$LOD_Hi[[tgt]], error = function(e) NULL)
      def_lo <- tryCatch(std_force_lod$LOD_Lo[[tgt]], error = function(e) NULL)

      # LOD_Hi
      cat(sprintf("    Target '%s'  LOD_Hi%s: ", tgt,
                  if (!is.null(def_hi)) sprintf(" [default: %g]", def_hi) else ""))
      raw_in <- trimws(readline())
      if (raw_in == "" && !is.null(def_hi)) {
        hi_val <- def_hi
        cat(sprintf("    → Accepted default: %g\n", hi_val))
      } else {
        hi_val <- suppressWarnings(as.numeric(raw_in))
        while (is.na(hi_val) || hi_val <= 0) {
          cat(sprintf("    Please enter a positive number for LOD_Hi ('%s'): ", tgt))
          hi_val <- suppressWarnings(as.numeric(trimws(readline())))
        }
      }

      # LOD_Lo
      cat(sprintf("    Target '%s'  LOD_Lo%s: ", tgt,
                  if (!is.null(def_lo)) sprintf(" [default: %g]", def_lo) else ""))
      raw_in <- trimws(readline())
      if (raw_in == "" && !is.null(def_lo)) {
        lo_val <- def_lo
        cat(sprintf("    → Accepted default: %g\n", lo_val))
      } else {
        lo_val <- suppressWarnings(as.numeric(raw_in))
        while (is.na(lo_val) || lo_val <= 0) {
          cat(sprintf("    Please enter a positive number for LOD_Lo ('%s'): ", tgt))
          lo_val <- suppressWarnings(as.numeric(trimws(readline())))
        }
      }

      lod_hi_out[[tgt]] <- hi_val
      lod_lo_out[[tgt]] <- lo_val
    }

    force_lods[[fi$file]] <- list(LOD_Hi = lod_hi_out, LOD_Lo = lod_lo_out)
    cat(sprintf("  LOD override recorded for %s.\n\n", stem_label))
  }

  list(skip_files = character(0), force_lods = force_lods)
}


# Checks whether a plate has already been fully processed.
# A plate is considered complete when BOTH output CSVs exist:
#   <stem>_all_samples.csv  and  <stem>_review_samples.csv
#
# Args:
#   stem       : filename stem (no extension) of the plate, e.g. "plate1"
#   output_dir : OUTPUT_DIR value (where processed CSVs are written)
#
# Returns: TRUE if both files exist, FALSE otherwise
check_plate_complete <- function(stem, output_dir) {
  all_path    <- file.path(output_dir, paste0(stem, "_all_samples.csv"))
  review_path <- file.path(output_dir, paste0(stem, "_review_samples.csv"))
  file.exists(all_path) && file.exists(review_path)
}


# Prompts the user interactively to confirm whether to skip completed plates.
# Prints a summary table of which plates would be skipped, then waits for Y/N.
#
# Non-interactive fallback: if stdin is not a terminal (e.g. Rscript in a
# scheduled job), returns "N" automatically and notes this in the console.
#
# Args:
#   skippable : character vector of plate stems that are complete
#
# Returns: "Y" or "N" (always uppercase)
ask_skip_confirmed <- function(skippable, stems) {
  cat(sprintf(
    "\n%s\n The following %d of %d plate(s) already have both output CSVs:\n%s\n",
    strrep("-", .pg_width),
    length(skippable),
    length(stems),
    paste(sprintf("   - %s", skippable), collapse = "\n")
  ))
  cat(strrep("-", .pg_width), "\n")
  cat(" Skip these plates and process only the remaining ones?\n")
  cat(" Enter Y to skip, N to reprocess all: ")

  # Detect non-interactive session (e.g. Rscript, knitr, scheduled jobs)
  if (!interactive()) {
    cat("\n  [non-interactive session detected — defaulting to N (reprocess all)]\n")
    return("N")
  }

  response <- toupper(trimws(readline()))

  # Keep asking until a valid answer is given
  while (!response %in% c("Y", "N")) {
    cat("  Please enter Y or N: ")
    response <- toupper(trimws(readline()))
  }

  response
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
        source     = character(),
        version    = character()
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
    version    = "0.1.8"
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
        source     = character(),
        version    = character()
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
    version    = "0.1.8"
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
                          force_lod_list = NULL,
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

  raw_full  <- read_csv(file, show_col_types = FALSE, col_names = FALSE,
                        col_types = cols(.default = col_character()))
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
  # Priority: force_lod_list (standards-check override) > LOD_List > scalar.
  # force_lod_list is only set for plates that were forced through the
  # standards pre-check with user-supplied LOD values.
  # ----------------------------------------------------------
  targets <- NULL  # initialise here so it is always in scope for variable logging

  effective_lod_list <- if (!is.null(force_lod_list)) {
    cat(sprintf("  %-16s\u2502 using forced LOD override (standards check bypassed)\n", "LOD"))
    force_lod_list
  } else {
    LOD_List
  }

  if (!is.null(effective_lod_list)) {

    targets <- dat |>
      pull(target) |>
      unique() |>
      as.character() |>
      trimws() |>
      tolower()

    if (anyNA(targets)) {
      stop("NA present in `target` column of ", file, ". Please correct raw data.")
    }

    missing_hi <- setdiff(targets, names(effective_lod_list$LOD_Hi))
    missing_lo <- setdiff(targets, names(effective_lod_list$LOD_Lo))
    if (length(missing_hi)) stop("Missing targets in LOD_Hi: ", paste(missing_hi, collapse = ", "))
    if (length(missing_lo)) stop("Missing targets in LOD_Lo: ", paste(missing_lo, collapse = ", "))

    hi_vals <- as.numeric(unlist(effective_lod_list$LOD_Hi[targets]))
    lo_vals <- as.numeric(unlist(effective_lod_list$LOD_Lo[targets]))

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
      excess_reps = (n_rows > 2),                    # More than 2 replicates present

      # For >2 replicate groups, skip all computation — the user must reconcile
      # manually. Both delta_cq and avg_sq are forced to NA so no summary values
      # appear in the output, making it obvious that review is required.
      delta_cq = if_else(excess_reps, NA_real_, delta_cq),
      avg_sq   = if_else(excess_reps, NA_real_, avg_sq),

      # Review flags — any TRUE causes the group to appear in the review CSV.
      # rv_excess_reps overrides all other flags in review_reason (Step 5).
      rv_excess_reps  = excess_reps,
      rv_single_rep   = single_rep   & !excess_reps,
      rv_delta_cq     = !is.na(delta_cq) & (delta_cq > dCq_thr),
      rv_mixed_na_num = one_cq_na    & !excess_reps,
      rv_high_sq      = !is.na(avg_sq) & (avg_sq > LOD_Hi),
      pass_negative   = both_cq_na   # Clean negative; not a failure
    )

  n_flagged <- rep_summary |>
    summarise(n = sum(rv_excess_reps | rv_single_rep | rv_delta_cq |
                        rv_mixed_na_num | rv_high_sq,
                      na.rm = TRUE)) |>
    pull(n)

  # Log every flag outcome for every replicate group
  rep_summary |>
    rowwise() |>
    do({
      log_decision(.$sample, .$target, "RV_EXCESS_REPS",
                   if (.$rv_excess_reps) "applied" else "skipped",
                   paste0("n_rows=", .$n_rows, "; threshold=2"),
                   run_id, dec_log_path, file)

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
                            rv_excess_reps, rv_single_rep, rv_delta_cq,
                            rv_mixed_na_num, rv_high_sq),
      by = rep_key
    ) |>
    mutate(flag_name_mismatch = coalesce(flag_name_mismatch, FALSE))

  # Human-readable descriptions mapped to each flag column.
  # rv_excess_reps is listed first so it is easy to spot in the override logic below.
  reason_map <- tribble(
    ~flag,                ~reason,
    "rv_excess_reps",     "More than 2 replicates — manual reconciliation required",
    "flag_name_mismatch", "Sample name mismatch within (Fluor, Target, Content)",
    "rv_single_rep",      "Only one replicate present",
    "rv_delta_cq",        "|DeltaCq| exceeds threshold",
    "rv_mixed_na_num",    "One Cq is NA/NaN and the other is numeric",
    "rv_high_sq",         "AverageSQ > LOD_Hi"
  )

  review_cols <- c("rv_excess_reps", "flag_name_mismatch", "rv_single_rep",
                   "rv_delta_cq", "rv_mixed_na_num", "rv_high_sq")

  dat3 <- dat3 |>
    mutate(
      needs_review  = if_any(all_of(review_cols), ~ .x),
      review_reason = pmap_chr(
        pick(all_of(review_cols)),
        function(...) {
          flags_logical        <- c(...)
          names(flags_logical) <- review_cols

          # rv_excess_reps overrides all other flags: Cq and SQ are blanked for
          # these groups so there is nothing meaningful for the other checks to
          # act on. Only the excess-reps reason is shown.
          if (isTRUE(flags_logical[["rv_excess_reps"]])) {
            return(reason_map$reason[reason_map$flag == "rv_excess_reps"])
          }

          flags <- review_cols[flags_logical]
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
      # For >2 replicate groups, Cq and SQ are blanked on every row so the
      # user cannot accidentally use unchecked values downstream.
      Cq                       = if_else(rv_excess_reps, NA_character_, cq),
      `Starting Quantity (SQ)` = if_else(rv_excess_reps, NA_real_,      sq_adj),
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
# SECTION 8: Discover, Validate, and Filter Plate Files
# ============================================================

plate_files <- if (!is.null(FILES)) {
  message("FILES supplied explicitly; ignoring INPUT_DIR, FILE_PATTERN, and SEARCH_DEPTH.")
  as.character(FILES)
} else {
  list_files_depth(INPUT_DIR, FILE_PATTERN, SEARCH_DEPTH)
}

# Build and emit the file tree before any validation, so you can see
# exactly what was found (or not found) even if something goes wrong below.
tree_lines <- build_file_tree(plate_files, INPUT_DIR, SEARCH_DEPTH)
emit_file_tree(tree_lines, TREE_OUTPUT, TREE_PATH)

if (length(plate_files) == 0) {
  stop(
    "No matching files found.\n",
    "  INPUT_DIR    : ", INPUT_DIR,    "\n",
    "  FILE_PATTERN : ", FILE_PATTERN, "\n",
    "  SEARCH_DEPTH : ", SEARCH_DEPTH, "\n",
    "Check that the folder exists and contains files matching FILE_PATTERN."
  )
}

missing_files <- plate_files[!file.exists(plate_files)]
if (length(missing_files) > 0) {
  stop("The following files were found by the search but cannot be accessed:\n",
       paste(" -", missing_files, collapse = "\n"))
}

# ----------------------------------------------------------
# Log session-level variables once before the plate loop.
# These are global settings that do not vary per plate, so
# they are recorded here with a shared session run_id rather
# than being passed into process_plate() individually.
# ----------------------------------------------------------
.session_run_id <- paste0("SESSION_", format(Sys.time(), "%Y%m%d_%H%M%S"))
assign(".current_input_file", "(session)", envir = .GlobalEnv)

log_variables(
  vars = list(
    INPUT_DIR         = INPUT_DIR,
    FILE_PATTERN      = FILE_PATTERN,
    FILES             = if (is.null(FILES)) "(auto-discovered)" else paste(FILES, collapse = "|"),
    SEARCH_DEPTH      = SEARCH_DEPTH,
    TREE_OUTPUT       = TREE_OUTPUT,
    TREE_PATH         = TREE_PATH,
    OUTPUT_DIR        = OUTPUT_DIR,
    DEC_LOG_PATH      = DEC_LOG_PATH,
    VAR_LOG_PATH      = VAR_LOG_PATH,
    SKIP_COMPLETED    = SKIP_COMPLETED,
    STD_CHECK_ENABLED = STD_CHECK_ENABLED,
    N_STANDARDS       = N_STANDARDS,
    STD_LOG_PATH      = STD_LOG_PATH
  ),
  run_id       = .session_run_id,
  var_log_path = VAR_LOG_PATH,
  default_file = "(session)"
)

# ----------------------------------------------------------
# Skip-completed check
# If SKIP_COMPLETED is TRUE, identify plates where both output
# CSVs already exist and ask the user whether to skip them.
# The response is logged in the decision audit log.
# ----------------------------------------------------------
skipped_plates  <- character(0)   # stems of plates that were skipped
skip_response   <- NA_character_  # "Y", "N", or "non-interactive"

if (isTRUE(SKIP_COMPLETED)) {

  stems     <- file_stem(plate_files)
  completed <- vapply(stems, check_plate_complete, logical(1), output_dir = OUTPUT_DIR)

  if (any(completed)) {
    skippable    <- stems[completed]
    skip_response <- ask_skip_confirmed(skippable, stems)

    # Log the Y/N decision (or the non-interactive fallback) for each skippable plate
    for (s in skippable) {
      assign(".current_input_file", paste0(s, ".csv"), envir = .GlobalEnv)
      log_decision(
        sample_id    = NA,
        target       = NA,
        rule_id      = "SKIP_COMPLETED",
        outcome      = if (skip_response == "Y") "skipped" else "reprocess",
        evidence     = sprintf(
          "Both output CSVs found; user response: %s%s",
          skip_response,
          if (!interactive()) " (non-interactive default)" else ""
        ),
        run_id       = .session_run_id,
        dec_log_path = DEC_LOG_PATH,
        default_file = paste0(s, ".csv")
      )
    }

    if (skip_response == "Y") {
      skipped_plates <- skippable
      plate_files    <- plate_files[!completed]
      cat(sprintf("\n  Skipping %d plate(s); %d remaining to process.\n\n",
                  length(skipped_plates), length(plate_files)))
    } else {
      cat("\n  Reprocessing all plates.\n\n")
    }

  } else {
    cat("  No completed plates found — all plates will be processed.\n")
  }
} else {
  cat("  SKIP_COMPLETED is FALSE — processing all discovered plates.\n")
}

# After skipping, check there is still something left to do
if (length(plate_files) == 0) {
  cat("\n  All discovered plates were skipped. Nothing to process.\n")
  stop("No plates remaining after skip check. ",
       "Set SKIP_COMPLETED <- FALSE to reprocess all.", call. = FALSE)
}


# ----------------------------------------------------------
# Standards pre-check
# Reads every remaining plate file and verifies that all
# expected Std-001 … Std-NNN entries are present in the
# Content column BEFORE any rows are removed.
#
# Failing plates are collected first, then a single combined
# prompt lets the user either skip them or force them through
# with manually entered per-target LOD overrides.
# All outcomes (pass / skipped / forced / error) are written
# to STD_LOG_PATH for audit purposes.
# ----------------------------------------------------------
std_skipped_plates <- character(0)  # stems of plates removed by standards check
std_force_lods     <- list()        # named by file path; LOD override per forced plate

if (isTRUE(STD_CHECK_ENABLED) && !is.null(N_STANDARDS) && N_STANDARDS > 0) {

  cat(sprintf(
    "\n  Standards check: verifying Std-001 … Std-%03d across %d plate(s)...\n",
    N_STANDARDS, length(plate_files)
  ))

  std_results <- lapply(plate_files, check_plate_standards, n_expected = N_STANDARDS)
  names(std_results) <- plate_files

  # Log passed plates immediately; collect failures for the prompt.
  # Plates that pass but use abbreviated labelling (Std-01 style) are still
  # treated as passing — a console warning and audit-log note are emitted so
  # the non-canonical labels are visible without blocking processing.
  failing_info      <- list()
  label_warned_info <- list()   # passed plates with non-canonical label style

  for (fp in plate_files) {
    res <- std_results[[fp]]
    if (!is.null(res$error)) {
      failing_info[[length(failing_info) + 1]] <- c(list(file = fp), res)
    } else if (res$passed) {
      if (!is.null(res$label_warning)) {
        # Passes on content (no missing standards) but uses Std-01/Std-02 labelling.
        label_warned_info[[length(label_warned_info) + 1]] <- list(file = fp, res = res)
        log_standard_check(fp, N_STANDARDS, res, action = "pass",
                           notes  = paste0("Non-canonical label style ('", res$label_style,
                                           "'): ", res$label_warning,
                                           " — plate processed; no standards missing."),
                           run_id = .session_run_id, std_log_path = STD_LOG_PATH)
      } else {
        log_standard_check(fp, N_STANDARDS, res, action = "pass",
                           run_id = .session_run_id, std_log_path = STD_LOG_PATH)
      }
    } else {
      failing_info[[length(failing_info) + 1]] <- c(list(file = fp), res)
    }
  }

  # Print per-plate label warnings for any plates using abbreviated Std-01 style.
  if (length(label_warned_info) > 0) {
    cat(sprintf("\n%s\n", strrep("-", .pg_width)))
    cat(sprintf(
      " Standards label warning: %d plate(s) used abbreviated labelling (Std-01 style)\n",
      length(label_warned_info)
    ))
    cat(sprintf(
      " Expected canonical format: Std-%03d … Std-%03d\n",
      1L, N_STANDARDS
    ))
    cat(sprintf(
      " These plates will still be processed — no standards are missing.\n"
    ))
    cat(strrep("-", .pg_width), "\n")
    for (lw in label_warned_info) {
      cat(sprintf(
        "  %s\n    Label style : %s\n    Found       : %s\n    Normalised  : %s\n\n",
        file_stem(lw$file),
        lw$res$label_style,
        paste(lw$res$found,      collapse = ", "),
        paste(lw$res$found_norm, collapse = ", ")
      ))
    }
    cat(strrep("-", .pg_width), "\n\n")
  }

  n_pass <- length(plate_files) - length(failing_info)
  cat(sprintf("  Standards check: %d passed, %d failed.\n", n_pass, length(failing_info)))

  if (length(failing_info) > 0) {

    std_action <- ask_standards_action(failing_info, N_STANDARDS, STD_FORCE_LOD)

    # Process skip decisions
    if (length(std_action$skip_files) > 0) {
      std_skipped_plates <- file_stem(std_action$skip_files)
      plate_files        <- plate_files[!plate_files %in% std_action$skip_files]

      for (fp in std_action$skip_files) {
        log_standard_check(fp, N_STANDARDS, std_results[[fp]],
                           action = "skipped",
                           notes  = "User chose to skip this plate",
                           run_id = .session_run_id, std_log_path = STD_LOG_PATH)
      }
      cat(sprintf("\n  Standards check: skipping %d plate(s); %d file(s) remaining.\n\n",
                  length(std_skipped_plates), length(plate_files)))
    }

    # Process forced-through decisions
    if (length(std_action$force_lods) > 0) {
      std_force_lods <- std_action$force_lods

      for (fp in names(std_force_lods)) {
        log_standard_check(fp, N_STANDARDS, std_results[[fp]],
                           action       = "forced",
                           lod_override = std_force_lods[[fp]],
                           notes        = if (!interactive())
                             "Non-interactive session: auto-forced with STD_FORCE_LOD"
                           else
                             "User forced plate through with manual LOD override",
                           run_id       = .session_run_id,
                           std_log_path = STD_LOG_PATH)
      }
      cat(sprintf("  Standards check: %d plate(s) forced through with LOD overrides.\n\n",
                  length(std_force_lods)))
    }
  }

} else {
  cat("  STD_CHECK_ENABLED is FALSE — standards check skipped.\n")
}

# Guard: nothing left after standards filtering
if (length(plate_files) == 0) {
  cat("\n  0 file(s) remaining after standards check — nothing to do.\n")
  stop("No plates remaining after standards filtering.", call. = FALSE)
}


# ============================================================
# SECTION 9: Run Pipeline Across All Plates
# ============================================================

results <- lapply(seq_along(plate_files), function(i) {
  process_plate(
    file           = plate_files[[i]],
    LOD_List       = TARGET_LOD,           # Remove this line and set LOD_Lo / LOD_Hi
    force_lod_list = std_force_lods[[plate_files[[i]]]],   # NULL unless standards-forced
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
    status   = "processed",
    rows_in  = r$n_in,
    rows_out = r$n_out,
    review   = r$n_review,
    dry_run  = r$dry_run
  )
}))

# Append skipped plates as their own rows if any were skipped
if (length(skipped_plates) > 0) {
  skipped_tbl <- tibble(
    plate    = skipped_plates,
    status   = "skipped (outputs already existed)",
    rows_in  = NA_integer_,
    rows_out = NA_integer_,
    review   = NA_integer_,
    dry_run  = NA
  )
  summary_tbl <- bind_rows(summary_tbl, skipped_tbl)
}

if (length(std_skipped_plates) > 0) {
  std_skip_tbl <- tibble(
    plate    = std_skipped_plates,
    status   = "skipped (failed standards check)",
    rows_in  = NA_integer_,
    rows_out = NA_integer_,
    review   = NA_integer_,
    dry_run  = NA
  )
  summary_tbl <- bind_rows(summary_tbl, std_skip_tbl)
}

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