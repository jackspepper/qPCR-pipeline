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

SCRIPT_VERSION <- "0.2.0"

# ============================================================
# SECTION 1: Configuration
# ============================================================
# All user-facing settings live here. You should rarely need
# to edit anything outside Sections 1 and 2.

# --- Input ---
INPUT_DIR    <- "RawData/"   # Folder containing plate CSV files.
FILE_PATTERN <- "\\.csv$"    # Regex: which files to include (matched against filename).
FILES        <- NULL         # Optional: explicit character vector of file paths.
# If set, INPUT_DIR, FILE_PATTERN, and SEARCH_DEPTH
# are all ignored.
# e.g. FILES <- c("data_raw/plate1.csv", "data_raw/plate2.csv")

SEARCH_DEPTH <- 2            # How many subfolder levels to search within INPUT_DIR.
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
RM_PATTERNS <- c("Std", "NTC", "Neg")  # Regex patterns to match.
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
# Standards are split into Hi/Lo halves for LOD prompting:
#   Hi half : Std-001 … Std-floor(N_STANDARDS/2)       — governs LOD Hi
#   Lo half : Std-(floor(N_STANDARDS/2)+1) … Std-NNN   — governs LOD Lo
# For even N this split is symmetric (e.g. N=6 → Hi:1-3, Lo:4-6).
# For odd  N the Lo half is one standard larger (e.g. N=5 → Hi:1-2, Lo:3-5).
# N_STANDARDS must be >= 2 for the Hi/Lo split to be well-defined.

# Per-target LOD overrides for plates forced through the standards check.
# Keys must be lowercase target names matching your CSV files.
# Required in non-interactive sessions; also offered as defaults interactively.
# Set to NULL to be prompted to enter every value manually each time.
#
# NOTE: the interactive prompt now collects ONE value per LOD boundary and
# broadcasts it to all targets on the plate (because process_plate() requires
# all targets to share the same LOD values).  The list keys below must still
# name each target so the lookup succeeds, but all values within LOD_Hi (and
# within LOD_Lo) should be identical.
STD_FORCE_LOD <- NULL
# Example:
# STD_FORCE_LOD <- list(
#   LOD_Hi = list(nuc = 2000, lyta = 2000, copb = 2000),  # all equal
#   LOD_Lo = list(nuc = 0.012, lyta = 0.012, copb = 0.012)
# )

# Separate audit log that records the outcome of every standards check.
STD_LOG_PATH <- "audit/pcr_standards.csv"

# --- Run log ---
# A plain-text transcript of all console output produced during the run.
# Written (or overwritten) each time the script is sourced.  Intended as
# a quick human-readable overview: verify all plates were processed, and
# that the row counts and QC tallies at each step look reasonable.
# Set to NULL to disable entirely.
# NOTE: if the script exits with an ERROR before reaching the end, the
# sink may be left open and further R console output will not display.
# To recover, run  sink()  once in the R console.
RUN_LOG_PATH <- "audit/pcr_run_log.txt"


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
    hi_fucp   = 2000,
    fucp   = 2000,
    `hi-fucp` = 2000,
    `hi fucp` = 2000,
    hi_hpd3   = 2000,
    `hi-hpd3` = 2000,
    hpd3      = 2000,
    lyta      = 2000,
    copb      = 2000,
    speb      = 2000,
    nuc       = 2000,
    gyrb      = 2000,
    uni       = 200,
    univ      = 200,
    universal = 200,
    hh_hpd3   = 2000,
    hh_hypd   = 2000
  ),
  LOD_Lo = list(
    hi_fucp   = 0.012,
    fucp   = 0.012,
    `hi-fucp` = 0.012,
    `hi fucp` = 0.012,
    hi_hpd3   = 0.012,
    `hi-hpd3` = 0.012,
    hpd3      = 0.012,
    lyta      = 0.012,
    copb      = 0.012,
    speb      = 0.012,
    nuc       = 0.012,
    gyrb      = 0.012,
    uni       = 0.0012,
    univ      = 0.0012,
    universal = 0.0012,
    hh_hpd3   = 0.012,
    hh_hypd   = 0.012
  )
)


# Targets that are expected to always produce a detectable result.
# If a replicate group for one of these targets has ALL replicates adjusted
# to LOD_Lo / 2 (i.e. every replicate either had no Cq or an SQ below LOD_Lo),
# it is flagged RV_UNEXPECTED_NEG and sent to the review CSV for investigation.
#
# Two sub-reasons are recorded to distinguish the cause:
#   "No amplification"  — every Cq in the group is NA/NaN
#   "Below LOD_Lo"      — Cq(s) present but all SQ values were < LOD_Lo
#
# Keys must be lowercase and match the target names in your CSV files.
# Set to NULL or character(0) to disable the check entirely.
ALWAYS_POSITIVE_TARGETS <- c("uni", "univ", "universal"
  # Add targets that should always amplify, e.g.:
  # "nuc", "lyta", "gyrb"
)


# ============================================================
# SECTION 3: Library Import
# ============================================================
# tidyverse covers: dplyr, tidyr, stringr, purrr, readr, lubridate (>= 2.0)

library(tidyverse)
options(dplyr.show_progress = FALSE)
options(readr.show_progress = FALSE)
options(vroom.show_progress = FALSE)

# Runtime dependency version guards.
# These catch silent failures caused by using an older package that lacks
# functions called by this script (e.g. if_any, pick, across).
local({
  need <- list(dplyr = "1.1.0", tidyr = "1.3.0", purrr = "1.0.0",
               readr = "2.0.0", stringr = "1.5.0")
  for (pkg in names(need)) {
    ver <- tryCatch(packageVersion(pkg), error = function(e) NULL)
    if (is.null(ver))
      stop(pkg, " is not installed. Run: install.packages('tidyverse')", call. = FALSE)
    if (ver < need[[pkg]])
      stop(pkg, " >= ", need[[pkg]], " required (installed: ", ver, ").\n",
           "  Run: install.packages('tidyverse')", call. = FALSE)
  }
})

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
        write_csv(x, file, append = append, progress = FALSE, ...)
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
  # depth_requested may be NA_integer_ when FILES is supplied explicitly
  # (search depth is irrelevant in that case); the header reflects this.

  norm     <- function(p) gsub("\\\\", "/", p)
  root     <- norm(root_dir)
  if (!endsWith(root, "/")) root <- paste0(root, "/")
  root_esc <- gsub("([.+*?|(){}\\[\\]^$])", "\\\\\\1", root)

  rel          <- sort(sub(paste0("^", root_esc), "", norm(files)))
  depth_actual <- if (length(rel) > 0) max(nchar(gsub("[^/]", "", rel))) else 0

  depth_str <- if (is.na(depth_requested)) "explicit FILES — depth N/A"
               else sprintf("depth requested: %d", depth_requested)

  header <- sprintf(
    "%s  [%s | deepest file: %d level(s) | %d file(s) to process]",
    basename(root_dir), depth_str, depth_actual, length(files)
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

# Extracts the integer standard number from a label in any supported format.
# Accepts "Std-001", "Std-01", "std-3", etc.
# Returns NA_integer_ if the label does not parse.
# Used internally by normalize_std_label() and classify_missing_standards().
#
# Args:
#   s : character vector of one or more standard labels
#
# Returns: integer vector, same length as s
std_label_to_num <- function(s) {
  suppressWarnings(
    as.integer(sub("(?i)^std-0*", "", s, perl = TRUE))
  )
}


# Normalises a single standard label to canonical 3-digit form.
# Accepts both abbreviated (Std-01) and canonical (Std-001) styles,
# as well as any number of leading zeros.
# Used by both check_plate_standards() and classify_missing_standards().
#
# Args:
#   s : a single character label, e.g. "Std-01", "Std-001", "std-3"
#
# Returns: canonical form, e.g. "Std-001"
normalize_std_label <- function(s) {
  sprintf("Std-%03d", std_label_to_num(s))
}

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
                         col_types = cols(.default = col_character()),
                         progress = FALSE)
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
    # The pattern allows up to 6 digits so N_STANDARDS can safely exceed 99.
    std_present_raw <- sort(unique(std_vals[str_detect(std_vals,
                                                       regex("^Std-\\d{1,6}$",
                                                             ignore_case = TRUE))]))

    # Normalise all found labels to canonical 3-digit form (Std-001) so that
    # both "Std-01" and "Std-001" resolve identically during the missing check.
    # normalize_std_label() is defined as a top-level helper above.
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
    version            = SCRIPT_VERSION
  )
  write_csv_retry(entry, std_log_path, append = TRUE)
  invisible(entry)
}


# -------------------------------------------------------
# classify_missing_standards()
# -------------------------------------------------------
# Classifies the missing-standard pattern for a single failing plate.
# Used by ask_standards_action() to route each plate to the correct
# prompt path.
#
# End standards : Std-001 (LOD Hi endpoint) and Std-NNN (LOD Lo endpoint).
# Hi half       : Std-001 … Std-floor(N/2)   — missing ones affect LOD Hi.
# Lo half       : Std-(floor(N/2)+1) … Std-NNN — missing ones affect LOD Lo.
# Middle        : any standard that is NOT Std-001 or Std-NNN.
#
# Classification:
#   "middle_1_2"  — both endpoints present; 1–2 interior standards absent.
#   "middle_3plus"— both endpoints present; 3 or more interior standards absent.
#   "end_missing" — at least one endpoint (Std-001 or Std-NNN) is absent.
#
# Args:
#   missing    : character vector of canonical missing labels (e.g. "Std-001")
#   n_expected : N_STANDARDS
#
# Returns a named list with logical / integer / character fields.
classify_missing_standards <- function(missing, n_expected) {

  if (n_expected < 2L) {
    warning(
      "N_STANDARDS is ", n_expected, " — Hi/Lo half split requires at least 2 standards. ",
      "Both LOD_Hi and LOD_Lo will be prompted for all affected plates.",
      call. = FALSE
    )
    # Treat the single standard as belonging to both halves so both LODs are asked.
    return(list(
      hi_endpoint_missing  = any(grepl("Std-001", missing, ignore.case = TRUE)),
      lo_endpoint_missing  = any(grepl(sprintf("Std-%03d", n_expected), missing,
                                       ignore.case = TRUE)),
      n_missing_middle     = 0L,
      missing_middle_stds  = character(0),
      missing_hi_half      = length(missing) > 0L,
      missing_lo_half      = length(missing) > 0L,
      needs_both_lods      = length(missing) > 0L,
      needs_hi_only        = FALSE,
      needs_lo_only        = FALSE,
      type                 = "end_missing"
    ))
  }

  hi_nums <- seq_len(floor(n_expected / 2L))
  lo_nums <- seq.int(floor(n_expected / 2L) + 1L, n_expected)

  missing_nums <- std_label_to_num(missing)
  missing_nums <- sort(missing_nums[!is.na(missing_nums)])

  hi_endpoint_missing <- 1L         %in% missing_nums
  lo_endpoint_missing <- n_expected %in% missing_nums

  # Middle = every standard except the first and last
  middle_range     <- seq_len(n_expected)
  middle_range     <- middle_range[middle_range != 1L & middle_range != n_expected]
  missing_mid_nums <- intersect(missing_nums, middle_range)

  missing_hi_half <- any(missing_nums %in% hi_nums)
  missing_lo_half <- any(missing_nums %in% lo_nums)

  type <- if (hi_endpoint_missing || lo_endpoint_missing) {
    "end_missing"
  } else if (length(missing_mid_nums) > 2L) {
    "middle_3plus"
  } else {
    "middle_1_2"
  }

  list(
    hi_endpoint_missing  = hi_endpoint_missing,
    lo_endpoint_missing  = lo_endpoint_missing,
    n_missing_middle     = length(missing_mid_nums),
    missing_middle_stds  = sprintf("Std-%03d", missing_mid_nums),
    missing_hi_half      = missing_hi_half,
    missing_lo_half      = missing_lo_half,
    needs_both_lods      = missing_hi_half && missing_lo_half,
    needs_hi_only        = missing_hi_half && !missing_lo_half,
    needs_lo_only        = !missing_hi_half && missing_lo_half,
    type                 = type,
    hi_nums              = hi_nums,   # exposed so ask_standards_action need not recompute
    lo_nums              = lo_nums    # exposed so ask_standards_action need not recompute
  )
}


# -------------------------------------------------------
# .collect_lod_for_plate()
# -------------------------------------------------------
# Interactively collects LOD Hi and/or LOD Lo overrides for one plate.
#
# process_plate() enforces that every target on a plate must share the same
# LOD_Hi value AND the same LOD_Lo value (it hard-stops if they differ).
# Therefore a SINGLE value is collected here and broadcast to every target,
# keeping the returned list structure compatible with process_plate().
#
# Inference (Y/N fast-path):
#   LOD Hi is inferred (Y/N confirmation) when Std-001 IS present.
#   LOD Lo is inferred (Y/N confirmation) when Std-NNN IS present.
#   If the corresponding endpoint is MISSING, manual entry is always required.
#
# Reference values are taken from the first target found in TARGET_LOD.
# Because all targets on a plate must have equal LODs in TARGET_LOD for the
# pipeline to accept them, using any one target gives the correct reference.
#
# For a LOD that is unaffected (ask_hi / ask_lo = FALSE) the TARGET_LOD value
# is carried through automatically so the returned list is always fully
# populated for process_plate().
#
# Args:
#   fi            : one failing_info element ($file, $targets, …)
#   cls           : result of classify_missing_standards() for this plate
#   target_lod    : TARGET_LOD (Section 2) — shown as "normal" reference
#   std_force_lod : STD_FORCE_LOD (Section 1) — used as manual-entry defaults
#   n_expected    : N_STANDARDS
#   ask_hi / ask_lo     : whether to prompt for that LOD at all
#   infer_hi / infer_lo : TRUE → Y/N fast-path; FALSE → manual entry required
#
# Returns: list(LOD_Hi = list(<tgt> = <val>, …), LOD_Lo = list(…))
.collect_lod_for_plate <- function(fi, cls, target_lod, std_force_lod, n_expected,
                                   ask_hi, ask_lo, infer_hi, infer_lo) {

  tgts      <- fi$targets
  tgt_label <- paste(tgts, collapse = ", ")
  ref_tgt   <- tgts[[1]]   # representative target for reference-value lookup

  # Pull reference values from the first target (all must be equal per pipeline rules).
  norm_hi <- tryCatch(target_lod$LOD_Hi[[ref_tgt]],    error = function(e) NULL)
  norm_lo <- tryCatch(target_lod$LOD_Lo[[ref_tgt]],    error = function(e) NULL)
  def_hi  <- tryCatch(std_force_lod$LOD_Hi[[ref_tgt]], error = function(e) NULL)
  def_lo  <- tryCatch(std_force_lod$LOD_Lo[[ref_tgt]], error = function(e) NULL)

  # ---- LOD Hi ----
  if (ask_hi) {
    norm_hi_str <- if (!is.null(norm_hi)) sprintf("%g", norm_hi) else "(not in TARGET_LOD)"
    hi_ref_val  <- if (!is.null(norm_hi)) norm_hi else def_hi

    cat(sprintf("    Targets : %s\n", tgt_label))

    if (infer_hi && !is.null(hi_ref_val)) {
      cat(sprintf(
        "    LOD Hi  [normal = %s | Std-001 present \u2192 inferred; applies to all targets]\n",
        norm_hi_str
      ))
      cat(sprintf("    \u2192 Is LOD Hi = %g correct for this plate? [Y/n]: ", hi_ref_val))
      yn <- toupper(trimws(readline()))
      if (yn == "" || yn == "Y") {
        hi_val <- hi_ref_val
        cat(sprintf("    \u2192 Accepted: %g\n", hi_val))
      } else {
        cat(sprintf("    Enter LOD Hi (all targets)%s: ",
                    if (!is.null(def_hi)) sprintf(" [default: %g]", def_hi) else ""))
        raw_in <- trimws(readline())
        if (raw_in == "" && !is.null(def_hi)) {
          hi_val <- def_hi
          cat(sprintf("    \u2192 Accepted default: %g\n", hi_val))
        } else {
          hi_val <- suppressWarnings(as.numeric(raw_in))
          while (is.na(hi_val) || hi_val <= 0) {
            cat("    Please enter a positive number for LOD Hi: ")
            hi_val <- suppressWarnings(as.numeric(trimws(readline())))
          }
        }
      }
    } else {
      reason_str <- if (!infer_hi) "Std-001 MISSING \u2192 manual entry required"
                    else           "no TARGET_LOD value \u2192 manual entry required"
      cat(sprintf(
        "    LOD Hi  [normal = %s | %s; applies to all targets]\n",
        norm_hi_str, reason_str
      ))
      cat(sprintf("    Enter LOD Hi (all targets)%s: ",
                  if (!is.null(def_hi)) sprintf(" [default: %g]", def_hi) else ""))
      raw_in <- trimws(readline())
      if (raw_in == "" && !is.null(def_hi)) {
        hi_val <- def_hi
        cat(sprintf("    \u2192 Accepted default: %g\n", hi_val))
      } else {
        hi_val <- suppressWarnings(as.numeric(raw_in))
        while (is.na(hi_val) || hi_val <= 0) {
          cat("    Please enter a positive number for LOD Hi: ")
          hi_val <- suppressWarnings(as.numeric(trimws(readline())))
        }
      }
    }
    cat("\n")

  } else {
    # LOD Hi unaffected — use TARGET_LOD reference value (same for all targets)
    hi_val <- if (!is.null(norm_hi)) norm_hi else def_hi
  }

  # ---- LOD Lo ----
  if (ask_lo) {
    norm_lo_str <- if (!is.null(norm_lo)) sprintf("%g", norm_lo) else "(not in TARGET_LOD)"
    lo_ref_val  <- if (!is.null(norm_lo)) norm_lo else def_lo

    if (!ask_hi) cat(sprintf("    Targets : %s\n", tgt_label))  # only reprint if Hi was skipped

    if (infer_lo && !is.null(lo_ref_val)) {
      cat(sprintf(
        "    LOD Lo  [normal = %s | Std-%03d present \u2192 inferred; applies to all targets]\n",
        norm_lo_str, n_expected
      ))
      cat(sprintf("    \u2192 Is LOD Lo = %g correct for this plate? [Y/n]: ", lo_ref_val))
      yn <- toupper(trimws(readline()))
      if (yn == "" || yn == "Y") {
        lo_val <- lo_ref_val
        cat(sprintf("    \u2192 Accepted: %g\n", lo_val))
      } else {
        cat(sprintf("    Enter LOD Lo (all targets)%s: ",
                    if (!is.null(def_lo)) sprintf(" [default: %g]", def_lo) else ""))
        raw_in <- trimws(readline())
        if (raw_in == "" && !is.null(def_lo)) {
          lo_val <- def_lo
          cat(sprintf("    \u2192 Accepted default: %g\n", lo_val))
        } else {
          lo_val <- suppressWarnings(as.numeric(raw_in))
          while (is.na(lo_val) || lo_val <= 0) {
            cat("    Please enter a positive number for LOD Lo: ")
            lo_val <- suppressWarnings(as.numeric(trimws(readline())))
          }
        }
      }
    } else {
      reason_str <- if (!infer_lo) sprintf("Std-%03d MISSING \u2192 manual entry required", n_expected)
                    else           "no TARGET_LOD value \u2192 manual entry required"
      cat(sprintf(
        "    LOD Lo  [normal = %s | %s; applies to all targets]\n",
        norm_lo_str, reason_str
      ))
      cat(sprintf("    Enter LOD Lo (all targets)%s: ",
                  if (!is.null(def_lo)) sprintf(" [default: %g]", def_lo) else ""))
      raw_in <- trimws(readline())
      if (raw_in == "" && !is.null(def_lo)) {
        lo_val <- def_lo
        cat(sprintf("    \u2192 Accepted default: %g\n", lo_val))
      } else {
        lo_val <- suppressWarnings(as.numeric(raw_in))
        while (is.na(lo_val) || lo_val <= 0) {
          cat("    Please enter a positive number for LOD Lo: ")
          lo_val <- suppressWarnings(as.numeric(trimws(readline())))
        }
      }
    }
    cat("\n")

  } else {
    # LOD Lo unaffected — use TARGET_LOD reference value (same for all targets)
    lo_val <- if (!is.null(norm_lo)) norm_lo else def_lo
  }

  # Broadcast the single hi_val / lo_val to every target so the returned list
  # is fully keyed and compatible with process_plate()'s LOD resolution logic.
  lod_hi_out <- setNames(lapply(tgts, function(.) hi_val), tgts)
  lod_lo_out <- setNames(lapply(tgts, function(.) lo_val), tgts)

  list(LOD_Hi = lod_hi_out, LOD_Lo = lod_lo_out)
}


# -------------------------------------------------------
# .force_all_plates()
# -------------------------------------------------------
# Internal helper called by ask_standards_action() for option F.
# Collects LOD overrides for every failing plate. Inference (Y/N)
# is applied where the corresponding endpoint standard is present;
# manual entry is required where it is missing.
#
# Returns: list(skip_files = character(0), force_lods = named list)
.force_all_plates <- function(failing_info, classifications, n_expected,
                              target_lod, std_force_lod) {

  cat(sprintf(
    "\n  You will now enter LOD overrides for each plate.\n"
  ))
  cat(sprintf(
    "  (STD_FORCE_LOD %s  |  TARGET_LOD values shown as 'normal' for reference)\n\n",
    if (is.null(std_force_lod)) "is NOT defined" else "is defined"
  ))

  force_lods <- list()

  for (i in seq_along(failing_info)) {
    fi  <- failing_info[[i]]
    cls <- classifications[[i]]

    stem_label <- file_stem(fi$file)
    cat(sprintf("  --- %s ---\n", stem_label))
    cat(sprintf("      Found   : %s\n", paste(fi$found,   collapse = ", ")))
    cat(sprintf("      Missing : %s\n\n", paste(fi$missing, collapse = ", ")))

    if (!is.null(fi$error) || identical(cls$type, "error")) {
      if (is.null(std_force_lod))
        stop("Cannot force '", stem_label, "': file read error and STD_FORCE_LOD is NULL.",
             call. = FALSE)
      cat("  File read error — using STD_FORCE_LOD directly.\n\n")
      force_lods[[fi$file]] <- std_force_lod
      next
    }

    if (length(fi$targets) == 0) {
      cat("  Warning: no targets found in this plate.\n")
      if (is.null(std_force_lod))
        stop("Cannot force '", stem_label, "': no targets found and STD_FORCE_LOD is NULL.",
             call. = FALSE)
      cat("  Using STD_FORCE_LOD directly.\n\n")
      force_lods[[fi$file]] <- std_force_lod
      next
    }

    plate_lod <- .collect_lod_for_plate(
      fi            = fi,
      cls           = cls,
      target_lod    = target_lod,
      std_force_lod = std_force_lod,
      n_expected    = n_expected,
      ask_hi        = TRUE,
      ask_lo        = TRUE,
      infer_hi      = !cls$hi_endpoint_missing,
      infer_lo      = !cls$lo_endpoint_missing
    )
    force_lods[[fi$file]] <- plate_lod
    cat(sprintf("  LOD override recorded for %s.\n\n", stem_label))
  }

  force_type <- setNames(
    rep(list("force_override"), length(force_lods)),
    names(force_lods)
  )
  list(skip_files = character(0), force_lods = force_lods, force_type = force_type)
}


# Shows a combined console summary of all plates that failed the standards check,
# classifies each by missing-standard pattern, then offers up to three actions:
#
#   S — Skip all failing plates (excluded from this run).
#
#   M — Allow middle-only missing plates through; collect LOD overrides only for
#       endpoint-missing plates.  Only shown when middle-only plates exist.
#       • Plates missing 1–2 middle standards: Y/N LOD confirmation (both endpoints
#         are present so TARGET_LOD values are inferred; manual entry only if N).
#       • Plates missing >2 middle standards: per-plate Y/N include/exclude prompt.
#         If included they are treated as endpoint-missing (LOD entry required).
#         If excluded they are NOT processed this run.
#
#   F — Force all failing plates through; collect LOD overrides for every plate.
#       Inference (Y/N) is applied where the corresponding endpoint standard is
#       present.  Manual entry is required where an endpoint standard is missing.
#
# The "normal" TARGET_LOD values are shown alongside every prompt so the user
# can make an informed decision without consulting the script.
#
# Non-interactive: auto-forces all plates through using STD_FORCE_LOD (errors if NULL).
#
# Args:
#   failing_info  : list of named lists, one per failing plate:
#                   $file, $found, $missing, $targets, $error
#   n_expected    : N_STANDARDS
#   std_force_lod : STD_FORCE_LOD from Section 1 (may be NULL)
#   target_lod    : TARGET_LOD from Section 2
#
# Returns a named list:
#   $skip_files : character vector of file paths to remove from processing
#   $force_lods : named list of per-file LOD overrides (keyed by file path)
ask_standards_action <- function(failing_info, n_expected, std_force_lod, target_lod) {

  n_fail  <- length(failing_info)

  # ---- Classify each failing plate ----
  classifications <- lapply(failing_info, function(fi) {
    if (!is.null(fi$error)) {
      list(type = "error", hi_endpoint_missing = NA, lo_endpoint_missing = NA,
           n_missing_middle = NA_integer_, missing_middle_stds = character(),
           missing_hi_half = NA, missing_lo_half = NA,
           needs_both_lods = NA, needs_hi_only = NA, needs_lo_only = NA,
           hi_nums = integer(0), lo_nums = integer(0))
    } else {
      classify_missing_standards(fi$missing, n_expected)
    }
  })

  type_vec          <- vapply(classifications, function(c) c$type, character(1))
  is_middle_1_2     <- type_vec == "middle_1_2"
  is_middle_3p      <- type_vec == "middle_3plus"
  is_end_missing    <- type_vec == "end_missing"
  has_middle_plates <- any(is_middle_1_2 | is_middle_3p)

  # Derive the Hi/Lo split boundaries from the first non-error classification,
  # rather than recomputing them here (single authoritative source: classify_missing_standards).
  first_valid <- Find(function(c) c$type != "error", classifications)
  if (is.null(first_valid)) {
    # All plates had file-read errors; fall back to computing directly
    hi_nums <- seq_len(floor(n_expected / 2L))
    lo_nums <- seq.int(floor(n_expected / 2L) + 1L, n_expected)
  } else {
    hi_nums <- first_valid$hi_nums
    lo_nums <- first_valid$lo_nums
  }

  # ---- Print failure summary ----
  cat(sprintf("\n%s\n", strrep("-", .pg_width)))
  cat(sprintf(
    " Standards check: %d plate(s) failed (expected Std-001 \u2013 Std-%03d)\n",
    n_fail, n_expected
  ))
  cat(sprintf(
    " Hi half : Std-%03d \u2013 Std-%03d  (Std-001 = LOD Hi endpoint)\n",
    min(hi_nums), max(hi_nums)
  ))
  cat(sprintf(
    " Lo half : Std-%03d \u2013 Std-%03d  (Std-%03d = LOD Lo endpoint)\n",
    min(lo_nums), max(lo_nums), n_expected
  ))
  cat(strrep("-", .pg_width), "\n")

  for (i in seq_along(failing_info)) {
    fi  <- failing_info[[i]]
    cls <- classifications[[i]]

    if (!is.null(fi$error)) {
      cat(sprintf("  [ERROR] %s\n    Could not read file: %s\n\n",
                  file_stem(fi$file), fi$error))
      next
    }

    # Classification tag shown next to plate name
    cls_tag <- switch(cls$type,
      "middle_1_2"   = sprintf("[MIDDLE ONLY \u2014 %d interior standard(s) missing, both endpoints present]",
                               cls$n_missing_middle),
      "middle_3plus" = sprintf("[MIDDLE ONLY \u2014 %d interior standards missing (>2), both endpoints present]",
                               cls$n_missing_middle),
      "end_missing"  = "[ENDPOINT MISSING]",
      "[UNKNOWN]"
    )

    # Which LODs are affected
    lod_note <- if (isTRUE(cls$needs_both_lods)) {
      "LOD Hi AND LOD Lo both affected"
    } else if (isTRUE(cls$needs_hi_only)) {
      sprintf("LOD Hi affected (missing from Hi half: Std-%03d\u2013Std-%03d)",
              min(hi_nums), max(hi_nums))
    } else if (isTRUE(cls$needs_lo_only)) {
      sprintf("LOD Lo affected (missing from Lo half: Std-%03d\u2013Std-%03d)",
              min(lo_nums), max(lo_nums))
    } else {
      "LOD impact undetermined"
    }

    # Normal LOD reference from TARGET_LOD (first target shown as representative)
    lod_ref_line <- if (length(fi$targets) > 0) {
      ref_tgt    <- fi$targets[[1]]
      ref_hi     <- tryCatch(target_lod$LOD_Hi[[ref_tgt]], error = function(e) NULL)
      ref_lo     <- tryCatch(target_lod$LOD_Lo[[ref_tgt]], error = function(e) NULL)
      sprintf("    Normal LOD : Hi = %s, Lo = %s  (for '%s'; from TARGET_LOD)\n",
              if (!is.null(ref_hi)) sprintf("%g", ref_hi) else "n/a",
              if (!is.null(ref_lo)) sprintf("%g", ref_lo) else "n/a",
              ref_tgt)
    } else {
      "    Normal LOD : (no targets found in this plate)\n"
    }

    cat(sprintf(
      "  %s  %s\n    Found   : %s\n    Missing : %s\n%s    LOD     : %s\n\n",
      file_stem(fi$file), cls_tag,
      if (length(fi$found)   == 0) "(none)" else paste(fi$found,   collapse = ", "),
      if (length(fi$missing) == 0) "(none)" else paste(fi$missing, collapse = ", "),
      lod_ref_line,
      lod_note
    ))
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
    force_type <- setNames(
      rep(list("force_override"), length(force_lods)),
      names(force_lods)
    )
    return(list(skip_files = character(0), force_lods = force_lods,
                force_type = force_type))
  }

  # ---- Interactive: show options ----
  cat("\n Options:\n")
  cat("   S = Skip all failing plates (excluded from this run)\n")
  if (has_middle_plates) {
    cat("   M = Allow middle-only plates through with Y/N LOD confirmation;\n")
    cat("       enter LOD overrides only for endpoint-missing plates\n")
    cat("       (plates missing >2 middle standards will be asked individually)\n")
  }
  cat("   F = Force all failing plates through (enter LOD overrides for every plate)\n")

  valid_opts <- if (has_middle_plates) c("S", "M", "F") else c("S", "F")
  cat(sprintf(" Enter %s: ", paste(valid_opts, collapse = " or ")))

  response <- toupper(trimws(readline()))
  while (!response %in% valid_opts) {
    cat(sprintf("  Please enter %s: ", paste(valid_opts, collapse = " or ")))
    response <- toupper(trimws(readline()))
  }

  # ---- S: Skip all ----
  if (response == "S") {
    skip_files <- vapply(failing_info, function(fi) fi$file, character(1))
    cat(sprintf("  Skipping %d plate(s).\n\n", length(skip_files)))
    return(list(skip_files = skip_files, force_lods = list(),
                force_type = list()))
  }

  # ---- F: Force all ----
  if (response == "F") {
    result <- .force_all_plates(failing_info, classifications, n_expected,
                                target_lod, std_force_lod)
    result$force_type <- setNames(
      rep(list("force_override"), length(result$force_lods)),
      names(result$force_lods)
    )
    return(result)
  }

  # ---- M: Middle-only plates pass; endpoint-missing plates get LOD entry ----
  skip_files <- character(0)
  force_lods <- list()

  cat(sprintf(
    "\n  Option M selected \u2014 processing %d failing plate(s).\n\n", n_fail
  ))

  # Sub-step 1: Plates missing >2 middle standards — individual include/exclude prompt
  if (any(is_middle_3p)) {
    cat(sprintf("%s\n", strrep("-", .pg_width)))
    cat(sprintf(
      " %d plate(s) are missing MORE THAN 2 interior standards.\n", sum(is_middle_3p)
    ))
    cat(" Each requires an individual decision:\n")
    cat("   Y = Include (treated as endpoint-missing; LOD entry required)\n")
    cat("   N = Exclude (will NOT be processed this run)\n")
    cat(strrep("-", .pg_width), "\n")

    for (i in which(is_middle_3p)) {
      fi  <- failing_info[[i]]
      cls <- classifications[[i]]
      cat(sprintf(
        "  %s\n    Missing : %s  (%d interior standards missing)\n",
        file_stem(fi$file),
        paste(fi$missing, collapse = ", "),
        cls$n_missing_middle
      ))
      cat("  Include this plate? [Y/N]: ")
      yn <- toupper(trimws(readline()))
      while (!yn %in% c("Y", "N")) {
        cat("  Please enter Y or N: ")
        yn <- toupper(trimws(readline()))
      }
      if (yn == "Y") {
        is_end_missing[[i]] <- TRUE   # promote to endpoint-missing treatment
        cat(sprintf("  \u2192 '%s' included \u2014 will require LOD entry.\n\n",
                    file_stem(fi$file)))
      } else {
        skip_files <- c(skip_files, fi$file)
        cat(sprintf("  \u2192 '%s' excluded \u2014 will NOT be processed.\n\n",
                    file_stem(fi$file)))
      }
    }
  }

  # Sub-step 2: Middle-only 1–2 missing plates — Y/N LOD confirmation
  n_m12 <- sum(is_middle_1_2)
  if (n_m12 > 0) {
    cat(sprintf("%s\n", strrep("-", .pg_width)))
    cat(sprintf(
      " Confirming LODs for %d middle-only plate(s) (1\u20132 interior standards missing, endpoints intact):\n",
      n_m12
    ))
    cat(strrep("-", .pg_width), "\n")

    for (i in which(is_middle_1_2)) {
      fi  <- failing_info[[i]]
      cls <- classifications[[i]]
      cat(sprintf("  --- %s ---\n", file_stem(fi$file)))
      cat(sprintf("      Found   : %s\n",   paste(fi$found,   collapse = ", ")))
      cat(sprintf("      Missing : %s\n\n", paste(fi$missing, collapse = ", ")))

      if (length(fi$targets) == 0) {
        cat("  Warning: no targets found \u2014 using TARGET_LOD / STD_FORCE_LOD directly.\n\n")
        force_lods[[fi$file]] <- if (!is.null(std_force_lod)) std_force_lod else
          list(LOD_Hi = target_lod$LOD_Hi, LOD_Lo = target_lod$LOD_Lo)
        next
      }

      # Both endpoints present → infer both LODs with Y/N
      plate_lod <- .collect_lod_for_plate(
        fi = fi, cls = cls, target_lod = target_lod, std_force_lod = std_force_lod,
        n_expected = n_expected,
        ask_hi = TRUE, ask_lo = TRUE,
        infer_hi = TRUE,   # Std-001 present by definition for middle-only
        infer_lo = TRUE    # Std-NNN present by definition for middle-only
      )
      force_lods[[fi$file]] <- plate_lod
      cat(sprintf("  LOD override recorded for %s.\n\n", file_stem(fi$file)))
    }
  }

  # Sub-step 3: Endpoint-missing plates (including any promoted from >2 middle)
  n_end <- sum(is_end_missing)
  if (n_end > 0) {
    cat(sprintf("%s\n", strrep("-", .pg_width)))
    cat(sprintf(
      " Entering LOD overrides for %d endpoint-missing plate(s):\n", n_end
    ))
    cat(strrep("-", .pg_width), "\n")

    for (i in which(is_end_missing)) {
      fi  <- failing_info[[i]]
      cls <- classifications[[i]]

      cat(sprintf("  --- %s ---\n", file_stem(fi$file)))
      cat(sprintf("      Found   : %s\n",   paste(fi$found,   collapse = ", ")))
      cat(sprintf("      Missing : %s\n\n", paste(fi$missing, collapse = ", ")))

      if (length(fi$targets) == 0) {
        cat("  Warning: no targets found \u2014 using TARGET_LOD / STD_FORCE_LOD directly.\n\n")
        force_lods[[fi$file]] <- if (!is.null(std_force_lod)) std_force_lod else
          list(LOD_Hi = target_lod$LOD_Hi, LOD_Lo = target_lod$LOD_Lo)
        next
      }

      # Ask only for the LOD(s) where standards are missing on that half.
      # Inference (Y/N) applies where the corresponding endpoint IS still present.
      ask_hi <- cls$missing_hi_half
      ask_lo <- cls$missing_lo_half
      if (!ask_hi && !ask_lo) { ask_hi <- TRUE; ask_lo <- TRUE }  # guard / fallback

      plate_lod <- .collect_lod_for_plate(
        fi = fi, cls = cls, target_lod = target_lod, std_force_lod = std_force_lod,
        n_expected = n_expected,
        ask_hi   = ask_hi,
        ask_lo   = ask_lo,
        infer_hi = !cls$hi_endpoint_missing,
        infer_lo = !cls$lo_endpoint_missing
      )
      force_lods[[fi$file]] <- plate_lod
      cat(sprintf("  LOD override recorded for %s.\n\n", file_stem(fi$file)))
    }
  }

  # Tag each forced plate so Section 8 can write the correct audit note.
  # middle_confirmed = Y/N LOD confirmation (both endpoints intact)
  # force_override   = manual LOD entry (endpoint missing or >2 middle promoted)
  force_type <- lapply(names(force_lods), function(fp) {
    cls_i <- classifications[[which(vapply(failing_info,
                                           function(fi) fi$file == fp, logical(1)))]]
    if (isTRUE(cls_i$type == "middle_1_2")) "middle_confirmed" else "force_override"
  })
  force_type <- setNames(force_type, names(force_lods))

  list(skip_files = skip_files, force_lods = force_lods, force_type = force_type)
}


# -------------------------------------------------------
# Sample-names check helpers
# -------------------------------------------------------

# Reads a plate file and checks whether every row in the Sample
# column is blank (NA or empty string).  Uses the same dynamic
# header detection as process_plate() for consistency.
#
# Args:
#   file : path to the plate CSV
#
# Returns a named list:
#   $all_blank    : TRUE if every sample value is NA / empty
#   $n_rows       : total data rows found
#   $n_blank      : count of blank sample rows
#   $content_vals : up to 6 unique Content values (for display)
#   $error        : NULL on success; error message string otherwise
check_plate_sample_names <- function(file) {
  tryCatch({
    raw_full <- read_csv(file, show_col_types = FALSE, col_names = FALSE,
                         col_types = cols(.default = col_character()),
                         progress = FALSE)
    well_row <- which(raw_full[[1]] == "Well")

    if (length(well_row) == 0)
      return(list(all_blank = FALSE, n_rows = 0L, n_blank = 0L,
                  content_vals = character(),
                  error = "Could not find 'Well' header row"))

    well_row      <- well_row[1]
    col_headers   <- as.character(raw_full[well_row, ])
    raw           <- raw_full[(well_row + 1):nrow(raw_full), ]
    colnames(raw) <- col_headers
    raw           <- clean_col_names(raw)

    if (!"sample" %in% names(raw))
      return(list(all_blank = FALSE, n_rows = nrow(raw), n_blank = 0L,
                  content_vals = character(),
                  error = "No 'sample' column found after header detection"))

    sample_vals <- as.character(raw$sample)
    is_blank    <- is.na(sample_vals) | !nzchar(trimws(sample_vals))
    n_blank     <- sum(is_blank)
    n_rows      <- nrow(raw)

    content_vals <- character(0)
    if ("content" %in% names(raw)) {
      content_vals <- sort(unique(as.character(
        raw$content[!is.na(raw$content) & nzchar(trimws(raw$content))]
      )))
      content_vals <- head(content_vals, 6)
    }

    list(all_blank    = (n_blank == n_rows),
         n_rows       = n_rows,
         n_blank      = n_blank,
         content_vals = content_vals,
         error        = NULL)

  }, error = function(e) {
    list(all_blank = FALSE, n_rows = 0L, n_blank = 0L,
         content_vals = character(),
         error = conditionMessage(e))
  })
}


# Shows a combined console summary of all plates where every sample
# name is blank, then prompts the user to either skip them or assign
# the Content column as the Sample column.
#
# Non-interactive — always skips (default), since no user input is
# available to confirm the content-as-sample substitution.
#
# Args:
#   blank_info : list of named lists, one per affected plate:
#                $file, $n_rows, $content_vals, $error
#
# Returns a named list:
#   $skip_files             : character vector of file paths to skip
#   $content_as_sample_files: character vector of file paths where
#                             Content should be used as Sample
ask_sample_names_action <- function(blank_info) {

  n_blank <- length(blank_info)

  cat(sprintf("\n%s\n", strrep("-", .pg_width)))
  cat(sprintf(
    " Sample names check: %d plate(s) have NO sample names\n", n_blank
  ))
  cat(strrep("-", .pg_width), "\n")

  for (bi in blank_info) {
    if (!is.null(bi$error)) {
      cat(sprintf("  [ERROR] %s\n    Could not read file: %s\n\n",
                  file_stem(bi$file), bi$error))
    } else {
      cat(sprintf(
        "  %s\n    Rows       : %d\n    Content    : %s\n\n",
        file_stem(bi$file),
        bi$n_rows,
        if (length(bi$content_vals) == 0) "(none found)"
        else paste(bi$content_vals, collapse = ", ")
      ))
    }
  }
  cat(strrep("-", .pg_width), "\n")

  # ---- Non-interactive path ----
  if (!interactive()) {
    cat(sprintf(
      "\n  [non-interactive] Defaulting to S — skipping %d plate(s) with blank sample names.\n\n",
      n_blank
    ))
    skip_files <- vapply(blank_info, function(bi) bi$file, character(1))
    return(list(skip_files = skip_files, content_as_sample_files = character(0)))
  }

  # ---- Interactive path ----
  cat(" Options:\n")
  cat("   S = Skip all   — exclude these plates from this run\n")
  cat("   C = Use Content column as Sample name for all these plates\n")
  cat(" Enter S or C: ")

  response <- toupper(trimws(readline()))
  while (!response %in% c("S", "C")) {
    cat("  Please enter S or C: ")
    response <- toupper(trimws(readline()))
  }

  if (response == "S") {
    skip_files <- vapply(blank_info, function(bi) bi$file, character(1))
    cat(sprintf("  Skipping %d plate(s).\n\n", length(skip_files)))
    return(list(skip_files = skip_files, content_as_sample_files = character(0)))
  }

  # ---- Content-as-sample path ----
  content_files <- vapply(blank_info, function(bi) bi$file, character(1))
  cat(sprintf(
    "\n  Content column will be used as Sample name for %d plate(s).\n\n",
    length(content_files)
  ))
  list(skip_files = character(0), content_as_sample_files = content_files)
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
#
# Args:
#   n_out        : rows in the all_samples output
#   n_review     : rows flagged for review
#   elapsed_secs : wall-clock seconds from plate start to here (Sys.time() diff)
pg_summary <- function(n_out, n_review, elapsed_secs) {
  rate_str <- if (!is.na(elapsed_secs) && n_out > 0)
    sprintf(" | %.3fs/row", elapsed_secs / n_out)
  else
    ""
  cat(strrep("\u2500", .pg_width), "\n", sep = "")
  cat(sprintf(
    "  Output  | %d rows out | %d for review | %.1fs%s\n",
    n_out, n_review, elapsed_secs, rate_str
  ))
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
# Accepts the file path passed directly by the caller; no global-env side-channel.
# Both log_decision() and log_variables() require the caller to supply
# current_file explicitly, eliminating the need for assign() calls in Section 8.
.resolve_input_file <- function(current_file) {
  basename(current_file)
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
#   current_file : path of the plate file being processed (used as input_file)
log_decision <- function(sample_id, target, rule_id, outcome, evidence,
                         run_id, dec_log_path, current_file) {
  ensure_dec_log(dec_log_path)
  entry <- tibble(
    timestamp  = now(tzone = "UTC"),
    user       = Sys.info()[["user"]],
    run_id     = run_id,
    input_file = .resolve_input_file(current_file),
    sample_id  = as.character(sample_id %||% NA_character_),
    target     = as.character(target     %||% NA_character_),
    rule_id    = rule_id,
    outcome    = outcome,
    evidence   = evidence,
    source     = "R_script",
    version    = SCRIPT_VERSION
  )
  write_csv_retry(entry, dec_log_path, append = TRUE)
  invisible(entry)
}


# Appends multiple decision rows to the log in a SINGLE write call.
#
# This replaces the old rowwise()|>do() pattern with a vectorised approach
# that is both faster (one file open/write/close per step instead of one per
# row) and compatible with future dplyr releases (do() is deprecated).
#
# Args:
#   df           : data frame with columns: sample_id, target, rule_id,
#                  outcome, evidence  (all character; NA where not applicable)
#   run_id       : unique ID for this plate run
#   dec_log_path : path to the decision log CSV
#   current_file : path of the plate file being processed
log_decisions_batch <- function(df, run_id, dec_log_path, current_file) {
  if (is.null(df) || nrow(df) == 0L) return(invisible(NULL))
  ensure_dec_log(dec_log_path)
  entries <- tibble(
    timestamp  = now(tzone = "UTC"),
    user       = Sys.info()[["user"]],
    run_id     = run_id,
    input_file = .resolve_input_file(current_file),
    sample_id  = as.character(df$sample_id),
    target     = as.character(df$target),
    rule_id    = as.character(df$rule_id),
    outcome    = as.character(df$outcome),
    evidence   = as.character(df$evidence),
    source     = "R_script",
    version    = SCRIPT_VERSION
  )
  write_csv_retry(entries, dec_log_path, append = TRUE)
  invisible(entries)
}

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
#   current_file : path of the plate file being processed (used as input_file)
#   sample_id    : optional — links the entry to a specific sample
#   target       : optional — links the entry to a specific target
log_variables <- function(vars, run_id, var_log_path, current_file,
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
    input_file = .resolve_input_file(current_file),
    sample_id  = as.character(sample_id %||% NA_character_),
    target     = as.character(target     %||% NA_character_),
    var_name   = names(vars),
    var_value  = var_values,
    var_class  = var_class,
    source     = "R_script",
    version    = SCRIPT_VERSION
  )

  write_csv_retry(entry, var_log_path, append = TRUE)
  invisible(entry)
}

# [UTILITY — not called in main pipeline, available for future use]
# Convenience wrapper: logs selected columns from a single-row data frame.
log_variables_from_df <- function(df, cols, run_id, var_log_path,
                                  current_file, sample_id = NULL, target = NULL) {
  stopifnot(is.data.frame(df), all(cols %in% names(df)), nrow(df) == 1)
  vars <- as.list(df[1, cols, drop = FALSE])
  names(vars) <- cols
  log_variables(
    vars         = vars,
    run_id       = run_id,
    var_log_path = var_log_path,
    current_file = current_file,
    sample_id    = sample_id,
    target       = target
  )
}


# ============================================================
# validate_target_lod()
# ============================================================
# Pre-flight structural check called once at the start of Section 8.
#
# Verifies that TARGET_LOD is correctly formed:
#   - Both $LOD_Hi and $LOD_Lo sub-lists are present and non-empty.
#   - Every value in each sub-list is a positive, finite number.
#
# NOTE: This function does NOT check that all targets share the same
# LOD value — that constraint is per-plate, not global.  Different
# target groups (e.g. "uni" at 200 vs "nuc" at 2000) are perfectly
# valid in TARGET_LOD; process_plate() enforces equality only for
# the specific targets that appear together on a single plate.
#
# Args:
#   lod_list : TARGET_LOD (or any list with $LOD_Hi and $LOD_Lo sub-lists)
#
# Returns invisibly if valid; stops with an informative message if not.
validate_target_lod <- function(lod_list) {
  if (is.null(lod_list)) return(invisible(NULL))

  check_half <- function(half_list, lod_label) {
    if (is.null(half_list) || length(half_list) == 0L) {
      stop("TARGET_LOD$", lod_label, " is missing or empty.\n",
           "  Add at least one target entry in Section 2.", call. = FALSE)
    }
    vals <- suppressWarnings(as.numeric(unlist(half_list)))
    bad_na  <- names(half_list)[is.na(vals)]
    bad_neg <- names(half_list)[!is.na(vals) & vals <= 0]

    if (length(bad_na) > 0)
      stop("TARGET_LOD$", lod_label, " contains non-numeric value(s) for: ",
           paste(bad_na, collapse = ", "),
           "\n  All LOD values must be positive numbers. Check Section 2.",
           call. = FALSE)

    if (length(bad_neg) > 0)
      stop("TARGET_LOD$", lod_label, " contains zero or negative value(s) for: ",
           paste(bad_neg, collapse = ", "),
           "\n  All LOD values must be positive numbers. Check Section 2.",
           call. = FALSE)

    invisible(NULL)
  }

  check_half(lod_list$LOD_Hi, "LOD_Hi")
  check_half(lod_list$LOD_Lo, "LOD_Lo")
  invisible(NULL)
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
                          always_pos_targets = character(0),
                          plate_index, n_plates,
                          enable_preview, dry_run, debug_print,
                          output_dir, dec_log_path, var_log_path,
                          content_as_sample = FALSE) {

  stem   <- file_stem(file)
  run_id <- paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_", stem)

  pg_plate(plate_index, n_plates, file)
  .plate_start <- Sys.time()  # wall-clock timer for per-plate elapsed time

  # Import: locate the "Well" header row dynamically and read from there.
  pg_info("Import", "locating header row ...")

  raw_full  <- read_csv(file, show_col_types = FALSE, col_names = FALSE,
                        col_types = cols(.default = col_character()),
                        progress = FALSE)
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
  # Content-as-Sample substitution
  # Only active when content_as_sample = TRUE, which is set by
  # the sample names pre-check when the user chose option C.
  # Overwrites the sample column with the content column value
  # BEFORE Step 0 runs, so the blank-name removal step finds
  # zero absent names and passes cleanly.
  #
  # The substitution is logged per-row with rule SAMPLE_FROM_CONTENT
  # and a visible banner is printed so the user is aware it occurred.
  # ----------------------------------------------------------
  if (isTRUE(content_as_sample)) {
    n_substituted <- sum(is.na(dat$sample) | !nzchar(trimws(as.character(dat$sample))))

    cat(sprintf(
      "  %-16s\u2502 [SAMPLE_FROM_CONTENT] Content column used as Sample name (%d row(s))\n",
      "Override", n_substituted
    ))

    dat <- dat |> mutate(sample = content)

    # Log one decision row per data row so the audit trail shows exactly
    # which Content values became Sample names for this plate.
    log_decisions_batch(
      df = dat |> transmute(
        sample_id = content,
        target    = target,
        rule_id   = "SAMPLE_FROM_CONTENT",
        outcome   = "applied",
        evidence  = paste0("Sample name blank; Content '", content,
                           "' used as Sample name")
      ),
      run_id       = run_id,
      dec_log_path = dec_log_path,
      current_file = file
    )
  }

  # ----------------------------------------------------------
  # Step 0: Remove rows with no sample name
  # ----------------------------------------------------------
  pg_step_start(0, "Removing unnamed samples")

  dat <- dat |>
    mutate(
      sample_reason = case_when(
        is.na(sample) | !nzchar(trimws(as.character(sample))) ~
          "RM_Sample_removed_due_to_absent_name",
        TRUE ~ NA_character_
      )
    )

  n_unnamed <- sum(!is.na(dat$sample_reason))

  log_decisions_batch(
    df = dat |>
      filter(!is.na(sample_reason)) |>
      transmute(
        sample_id = sample,
        target    = target,
        rule_id   = sample_reason,
        outcome   = "applied",
        evidence  = "Removed: absent sample name"
      ),
    run_id       = run_id,
    dec_log_path = dec_log_path,
    current_file = file
  )

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

  log_decisions_batch(
    df = dat1 |>
      filter(!is.na(sq_adj_reason)) |>
      transmute(
        sample_id = sample,
        target    = target,
        rule_id   = sq_adj_reason,
        outcome   = "applied",
        evidence  = paste0("sq_raw=", sq_raw, "; LOD_Lo=", LOD_Lo)
      ),
    run_id       = run_id,
    dec_log_path = dec_log_path,
    current_file = file
  )

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
        log_decisions_batch(
          df = dat1 |>
            filter(rm_hit) |>
            transmute(
              sample_id = sample,
              target    = target,
              rule_id   = "RM_PATTERN",
              outcome   = "applied",
              evidence  = paste0("cols=", paste(rm_cols, collapse = ","),
                                 "; pattern=", combined_pat)
            ),
          run_id       = run_id,
          dec_log_path = dec_log_path,
          current_file = file
        )
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

  log_decisions_batch(
    df = name_checks |>
      filter(flag_name_mismatch) |>
      transmute(
        sample_id = NA_character_,
        target    = target,
        rule_id   = "QC_SN_MISMATCH",
        outcome   = "applied",
        evidence  = paste0("content=", content, "; samples=", samples)
      ),
    run_id       = run_id,
    dec_log_path = dec_log_path,
    current_file = file
  )

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

  # ----------------------------------------------------------
  # RV_UNEXPECTED_NEG: per-group flag for always-positive targets.
  # A group fires this flag when the target is in always_pos_targets AND
  # every replicate in the group was adjusted to LOD_Lo / 2.
  # Two sub-types (joined from row-level sq_adj_reason in dat2):
  #   "No amplification" — every Cq in the group was NA/NaN
  #   "Below LOD_Lo"     — Cq(s) present but all SQ values fell below LOD_Lo
  # ----------------------------------------------------------
  if (length(always_pos_targets) > 0) {
    # Derive a per-group indicator: were ALL rows in this group adjusted?
    grp_adj <- dat2 |>
      group_by(across(all_of(rep_key))) |>
      summarise(
        all_adjusted  = all(!is.na(sq_adj_reason)),   # every row hit Step 1 adjustment
        all_cq_na     = all(is.na(cq_num) | is.nan(cq_num)),
        .groups = "drop"
      )

    rep_summary <- rep_summary |>
      left_join(grp_adj, by = rep_key) |>
      mutate(
        rv_unexpected_neg = !rv_excess_reps &
                            tolower(target) %in% tolower(always_pos_targets) &
                            coalesce(all_adjusted, FALSE),
        rv_unexpected_neg_sub = case_when(
          !rv_unexpected_neg   ~ NA_character_,
          all_cq_na            ~ "No amplification",
          TRUE                 ~ "Below LOD_Lo"
        )
      ) |>
      select(-all_adjusted, -all_cq_na)
  } else {
    rep_summary <- rep_summary |>
      mutate(
        rv_unexpected_neg     = FALSE,
        rv_unexpected_neg_sub = NA_character_
      )
  }

  n_flagged <- rep_summary |>
    summarise(n = sum(rv_excess_reps | rv_single_rep | rv_delta_cq |
                        rv_mixed_na_num | rv_high_sq | rv_unexpected_neg,
                      na.rm = TRUE)) |>
    pull(n)

  # Log every flag outcome for every replicate group.
  # All six rule columns are pivoted into long form so the entire step
  # is written in a single append call (replaces rowwise/do pattern).
  local({
    rs <- rep_summary   # local alias — avoids mutating the data used downstream

    step4_log <- bind_rows(
      rs |> transmute(
        sample_id = sample, target,
        rule_id   = "RV_EXCESS_REPS",
        outcome   = if_else(rv_excess_reps, "applied", "skipped"),
        evidence  = paste0("n_rows=", n_rows, "; threshold=2")
      ),
      rs |> transmute(
        sample_id = sample, target,
        rule_id   = "RV_SINGLE_REP",
        outcome   = if_else(rv_single_rep, "applied", "skipped"),
        evidence  = paste0("n_rows=", n_rows)
      ),
      rs |> transmute(
        sample_id = sample, target,
        rule_id   = "RV_DELTA_CQ",
        outcome   = if_else(!is.na(delta_cq) & delta_cq > dCq_thr,
                            "applied", "skipped"),
        evidence  = paste0("|DeltaCq|=", delta_cq, "; thr=", dCq_thr)
      ),
      rs |> transmute(
        sample_id = sample, target,
        rule_id   = "RV_MIXED_CQ",
        outcome   = if_else(rv_mixed_na_num, "applied", "skipped"),
        evidence  = paste0("n_num_cq=", n_num_cq, "; n_rows=", n_rows)
      ),
      # PASS_NEGATIVE — only rows where both Cq values were NA AND the target
      # is NOT an always-positive target (those get RV_UNEXPECTED_NEG instead)
      rs |> filter(pass_negative &
                   !tolower(target) %in% tolower(always_pos_targets)) |>
        transmute(
        sample_id = sample, target,
        rule_id   = "PASS_NEGATIVE",
        outcome   = "pass",
        evidence  = "Both Cq values are NA/NaN (true negative)"
      ),
      rs |> transmute(
        sample_id = sample, target,
        rule_id   = "RV_HIGH_SQ",
        outcome   = if_else(rv_high_sq, "applied", "skipped"),
        evidence  = paste0("avg_sq=", avg_sq, "; LOD_Hi=", LOD_Hi)
      ),
      rs |> filter(tolower(target) %in% tolower(always_pos_targets)) |>
        transmute(
          sample_id = sample, target,
          rule_id   = "RV_UNEXPECTED_NEG",
          outcome   = if_else(rv_unexpected_neg, "applied", "skipped"),
          evidence  = if_else(
            rv_unexpected_neg,
            paste0("always_pos_target=TRUE; sub_type=", rv_unexpected_neg_sub,
                   "; LOD_Lo=", LOD_Lo),
            "always_pos_target=TRUE; group not negative"
          )
        )
    )

    log_decisions_batch(
      df           = step4_log,
      run_id       = run_id,
      dec_log_path = dec_log_path,
      current_file = file
    )
  })

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
                            rv_mixed_na_num, rv_high_sq,
                            rv_unexpected_neg, rv_unexpected_neg_sub),
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
    "rv_high_sq",         "AverageSQ > LOD_Hi",
    "rv_unexpected_neg",  "Unexpected negative for always-positive target"
  )

  review_cols <- c("rv_excess_reps", "flag_name_mismatch", "rv_single_rep",
                   "rv_delta_cq", "rv_mixed_na_num", "rv_high_sq",
                   "rv_unexpected_neg")

  dat3 <- dat3 |>
    mutate(
      needs_review  = if_any(all_of(review_cols), ~ .x),
      review_reason = pmap_chr(
        pick(all_of(c(review_cols, "rv_unexpected_neg_sub"))),
        function(...) {
          args                 <- list(...)
          flags_logical        <- unlist(args[review_cols])
          names(flags_logical) <- review_cols
          neg_sub              <- args[["rv_unexpected_neg_sub"]]

          if (isTRUE(flags_logical[["rv_excess_reps"]])) {
            return(reason_map$reason[reason_map$flag == "rv_excess_reps"])
          }

          flags <- review_cols[flags_logical]
          if (length(flags) == 0) return(NA_character_)

          reasons <- vapply(flags, function(f) {
            base <- reason_map$reason[reason_map$flag == f]
            # Append the sub-type for rv_unexpected_neg so the reason is self-explanatory
            if (f == "rv_unexpected_neg" && !is.na(neg_sub))
              paste0(base, " (", neg_sub, ")")
            else
              base
          }, character(1))

          paste(reasons, collapse = " ; ")
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
      file                = file,
      dCq_thr             = dCq_thr,
      targets             = targets,   # NULL when scalar LODs were used
      lod_hi              = LOD_Hi,
      lod_lo              = LOD_Lo,
      always_pos_targets  = always_pos_targets,
      rm_patterns         = rm_patterns,
      rm_cols_req         = rm_cols_req,
      enable_preview      = enable_preview,
      dry_run             = dry_run,
      debug_print         = debug_print,
      output_dir          = output_dir,
      dec_log_path        = dec_log_path,
      var_log_path        = var_log_path
    ),
    run_id       = run_id,
    var_log_path = var_log_path,
    current_file = file
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

  .plate_elapsed <- as.numeric(difftime(Sys.time(), .plate_start, units = "secs"))

  pg_step_done(5)
  pg_summary(nrow(all_samples), nrow(review_samples), .plate_elapsed)

  invisible(list(
    stem            = stem,
    all_path        = all_path,
    review_path     = review_path,
    n_in            = nrow(raw),
    n_out           = nrow(all_samples),
    n_review        = nrow(review_samples),
    elapsed_secs    = .plate_elapsed,
    preview_enabled = isTRUE(enable_preview),
    dry_run         = isTRUE(dry_run)
  ))
}


# ============================================================
# SECTION 8: Discover, Validate, and Filter Plate Files
# ============================================================

# ----------------------------------------------------------
# Open run log sink
# Must come before any cat()/print() output so the full
# session transcript — file tree, standards check, per-plate
# progress, and summary — is all captured in one place.
# split = TRUE means output still appears on the console.
# ----------------------------------------------------------
if (!is.null(RUN_LOG_PATH)) {
  if (!dir.exists(dirname(RUN_LOG_PATH)))
    dir.create(dirname(RUN_LOG_PATH), recursive = TRUE, showWarnings = FALSE)
  .run_log_con   <- file(RUN_LOG_PATH, open = "wt")
  .run_log_start <- Sys.time()
  sink(.run_log_con, split = TRUE, type = "output")

  cat(strrep("=", .pg_width), "\n", sep = "")
  cat(" qPCR Pipeline Run Log\n")
  cat(strrep("-", .pg_width), "\n", sep = "")
  cat(sprintf(" Directory  : %s\n", getwd()))
  cat(sprintf(" Started    : %s\n", format(.run_log_start, "%Y-%m-%d %H:%M:%S")))
  cat(sprintf(" User       : %s\n", Sys.info()[["user"]]))
  cat(sprintf(" Input      : %s\n",
              if (!is.null(FILES)) paste(basename(FILES), collapse = ", ") else INPUT_DIR))
  cat(sprintf(" Output     : %s\n", OUTPUT_DIR))
  cat(sprintf(" Audit      : %s\n", dirname(DEC_LOG_PATH)))
  cat(strrep("=", .pg_width), "\n\n", sep = "")
}

plate_files <- if (!is.null(FILES)) {
  message("FILES supplied explicitly; ignoring INPUT_DIR, FILE_PATTERN, and SEARCH_DEPTH.")
  as.character(FILES)
} else {
  list_files_depth(INPUT_DIR, FILE_PATTERN, SEARCH_DEPTH)
}

# Build and emit the file tree before any validation, so you can see
# exactly what was found (or not found) even if something goes wrong below.
# BUG-03: when FILES is supplied explicitly, use the common directory of those
# files as the tree root rather than INPUT_DIR (which may not contain them).
.tree_root <- if (!is.null(FILES) && length(plate_files) > 0) {
  dirname(plate_files[[1]])
} else {
  INPUT_DIR
}
tree_lines <- build_file_tree(
  plate_files,
  .tree_root,
  if (!is.null(FILES)) NA_integer_ else SEARCH_DEPTH
)
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
# Pre-flight: validate TARGET_LOD consistency (REC-02).
# Done before any interactive prompts so a misconfigured
# Section 2 is caught immediately rather than mid-run.
# ----------------------------------------------------------
validate_target_lod(TARGET_LOD)

# ----------------------------------------------------------
# Log session-level variables once before the plate loop.
# These are global settings that do not vary per plate, so
# they are recorded here with a shared session run_id rather
# than being passed into process_plate() individually.
# ----------------------------------------------------------
.session_run_id <- paste0("SESSION_", format(Sys.time(), "%Y%m%d_%H%M%S"))

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
  current_file = "(session)"
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
        current_file = paste0(s, ".csv")
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

    std_action <- ask_standards_action(failing_info, N_STANDARDS, STD_FORCE_LOD, TARGET_LOD)

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
        ftype <- std_action$force_type[[fp]] %||% "force_override"
        notes_str <- if (!interactive()) {
          "Non-interactive session: auto-forced with STD_FORCE_LOD"
        } else if (ftype == "middle_confirmed") {
          "User confirmed TARGET_LOD values via Y/N (middle standards only missing; endpoints intact)"
        } else {
          "User forced plate through with manual LOD override (endpoint standard missing)"
        }
        log_standard_check(fp, N_STANDARDS, std_results[[fp]],
                           action       = "forced",
                           lod_override = std_force_lods[[fp]],
                           notes        = notes_str,
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


# ----------------------------------------------------------
# Sample names pre-check
# Reads every remaining plate file and checks whether the
# entire Sample column is blank.  Plates where every sample
# name is missing are shown to the user with a summary of
# their Content values, then the user chooses to either:
#   S — skip the plate entirely this run
#   C — use the Content column as the Sample name
#
# When Content-as-Sample is chosen:
#   - process_plate() substitutes Content → Sample before
#     Step 0 runs, so the blank-name removal step sees zero
#     blank rows and passes cleanly.
#   - The substitution is flagged in the audit log with
#     rule id SAMPLE_FROM_CONTENT and printed visibly during
#     plate processing so the user can see it occurred.
#   - The Source column in both output CSVs will reflect the
#     Content values rather than an empty Sample field.
#
# Non-interactive default: skip all affected plates.
# ----------------------------------------------------------
sn_skipped_plates        <- character(0)  # stems removed by sample-names check
sample_name_override_files <- character(0) # file paths where Content → Sample

cat(sprintf(
  "\n  Sample names check: scanning %d plate(s) for blank sample columns...\n",
  length(plate_files)
))

sn_results <- lapply(plate_files, check_plate_sample_names)
names(sn_results) <- plate_files

blank_info <- list()
for (fp in plate_files) {
  res <- sn_results[[fp]]
  if (!is.null(res$error)) {
    # Read error — report but do not block; process_plate() will surface it properly
    cat(sprintf("  [!] Could not check sample names for %s: %s\n",
                file_stem(fp), res$error))
  } else if (isTRUE(res$all_blank)) {
    blank_info[[length(blank_info) + 1]] <- c(list(file = fp), res)
  }
}

n_blank_plates <- length(blank_info)
cat(sprintf("  Sample names check: %d plate(s) with fully blank sample column.\n",
            n_blank_plates))

if (n_blank_plates > 0) {

  sn_action <- ask_sample_names_action(blank_info)

  # --- Skip path ---
  if (length(sn_action$skip_files) > 0) {
    sn_skipped_plates <- file_stem(sn_action$skip_files)
    plate_files       <- plate_files[!plate_files %in% sn_action$skip_files]

    for (fp in sn_action$skip_files) {
      log_decision(
        sample_id    = NA,
        target       = NA,
        rule_id      = "SAMPLE_NAMES_BLANK_SKIP",
        outcome      = "skipped",
        evidence     = sprintf(
          "All %d sample name(s) blank; user chose to skip%s",
          sn_results[[fp]]$n_rows,
          if (!interactive()) " (non-interactive default)" else ""
        ),
        run_id       = .session_run_id,
        dec_log_path = DEC_LOG_PATH,
        current_file = fp
      )
    }
    cat(sprintf("\n  Sample names check: skipping %d plate(s); %d file(s) remaining.\n\n",
                length(sn_skipped_plates), length(plate_files)))
  }

  # --- Content-as-sample path ---
  if (length(sn_action$content_as_sample_files) > 0) {
    sample_name_override_files <- sn_action$content_as_sample_files

    for (fp in sample_name_override_files) {
      log_decision(
        sample_id    = NA,
        target       = NA,
        rule_id      = "SAMPLE_FROM_CONTENT",
        outcome      = "applied",
        evidence     = sprintf(
          "All %d sample name(s) blank; user chose Content-as-Sample. Content values: %s",
          sn_results[[fp]]$n_rows,
          paste(sn_results[[fp]]$content_vals, collapse = ", ")
        ),
        run_id       = .session_run_id,
        dec_log_path = DEC_LOG_PATH,
        current_file = fp
      )
    }
    cat(sprintf("  Sample names check: %d plate(s) will use Content as Sample name.\n\n",
                length(sample_name_override_files)))
  }
}

# Guard: nothing left after sample names filtering
if (length(plate_files) == 0) {
  cat("\n  0 file(s) remaining after sample names check — nothing to do.\n")
  stop("No plates remaining after sample names check.", call. = FALSE)
}


# ============================================================
# SECTION 9: Run Pipeline Across All Plates
# ============================================================

results <- lapply(seq_along(plate_files), function(i) {
  process_plate(
    file                = plate_files[[i]],
    LOD_List            = TARGET_LOD,
    force_lod_list      = std_force_lods[[plate_files[[i]]]],
    dCq_thr             = DELTA_CQ_THRESHOLD,
    rm_patterns         = RM_PATTERNS,
    rm_cols_req         = RM_COLUMNS,
    always_pos_targets  = ALWAYS_POSITIVE_TARGETS,
    plate_index         = i,
    n_plates            = length(plate_files),
    enable_preview      = ENABLE_PREVIEW,
    dry_run             = DRY_RUN,
    debug_print         = DEBUG_PRINT,
    output_dir          = OUTPUT_DIR,
    dec_log_path        = DEC_LOG_PATH,
    var_log_path        = VAR_LOG_PATH,
    content_as_sample   = plate_files[[i]] %in% sample_name_override_files
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
    plate        = r$stem,
    status       = "processed",
    rows_in      = r$n_in,
    rows_out     = r$n_out,
    review       = r$n_review,
    elapsed_s    = round(r$elapsed_secs, 1),
    secs_per_row = if (!is.na(r$elapsed_secs) && r$n_out > 0)
      round(r$elapsed_secs / r$n_out, 4)
    else NA_real_,
    dry_run      = r$dry_run
  )
}))

# Append skipped plates as their own rows if any were skipped
if (length(skipped_plates) > 0) {
  skipped_tbl <- tibble(
    plate        = skipped_plates,
    status       = "skipped (outputs already existed)",
    rows_in      = NA_integer_,
    rows_out     = NA_integer_,
    review       = NA_integer_,
    elapsed_s    = NA_real_,
    secs_per_row = NA_real_,
    dry_run      = NA
  )
  summary_tbl <- bind_rows(summary_tbl, skipped_tbl)
}

if (length(std_skipped_plates) > 0) {
  std_skip_tbl <- tibble(
    plate        = std_skipped_plates,
    status       = "skipped (failed standards check)",
    rows_in      = NA_integer_,
    rows_out     = NA_integer_,
    review       = NA_integer_,
    elapsed_s    = NA_real_,
    secs_per_row = NA_real_,
    dry_run      = NA
  )
  summary_tbl <- bind_rows(summary_tbl, std_skip_tbl)
}

if (length(sn_skipped_plates) > 0) {
  sn_skip_tbl <- tibble(
    plate        = sn_skipped_plates,
    status       = "skipped (blank sample names)",
    rows_in      = NA_integer_,
    rows_out     = NA_integer_,
    review       = NA_integer_,
    elapsed_s    = NA_real_,
    secs_per_row = NA_real_,
    dry_run      = NA
  )
  summary_tbl <- bind_rows(summary_tbl, sn_skip_tbl)
}

if (length(sample_name_override_files) > 0) {
  override_stems <- file_stem(sample_name_override_files)
  # These plates were processed normally — update their status in the table
  # to make the Content-as-Sample substitution visible in the summary.
  summary_tbl <- summary_tbl |>
    mutate(status = if_else(
      plate %in% override_stems & status == "processed",
      "processed (Content used as Sample name)",
      status
    ))
}

print(summary_tbl)

# ----------------------------------------------------------
# Timing breakdown — processed plates only
# Shows each plate's elapsed time, rows, and normalised rate
# so that slower plates can be assessed relative to their size
# rather than penalised for simply containing more data.
# ----------------------------------------------------------
timing_tbl <- summary_tbl |>
  filter(status == "processed", !is.na(elapsed_s)) |>
  select(plate, rows_out, elapsed_s, secs_per_row)

if (nrow(timing_tbl) > 0) {
  total_rows    <- sum(timing_tbl$rows_out,  na.rm = TRUE)
  total_elapsed <- sum(timing_tbl$elapsed_s, na.rm = TRUE)
  mean_rate     <- if (total_rows > 0) round(total_elapsed / total_rows, 4) else NA_real_
  slowest_plate <- timing_tbl$plate[which.max(timing_tbl$secs_per_row)]

  cat("\n--- Per-plate timing (processed plates) ---\n")
  print(timing_tbl)

  cat(sprintf(
    "\n  Total rows processed : %d\n  Total plate time     : %.1fs\n  Mean rate            : %.4fs/row\n  Slowest (normalised) : %s\n",
    total_rows, total_elapsed, mean_rate, slowest_plate
  ))
}


# ============================================================
# SECTION 11: Audit Log Preview
# ============================================================
# Prints the 30 most recent audit decisions and a count table
# showing how many times each rule fired per file.

if (file.exists(DEC_LOG_PATH)) {
  audit <- read_csv(DEC_LOG_PATH, show_col_types = FALSE, progress = FALSE)

  cat("\n--- 30 most recent audit decisions ---\n")
  print(audit |> arrange(desc(timestamp)) |> head(30))

  cat("\n--- Decision counts by file, rule, and outcome ---\n")
  print(audit |> count(input_file, rule_id, outcome, sort = TRUE))

  rm(audit)
} else {
  cat("No audit log found at:", DEC_LOG_PATH, "\n")
}


# ============================================================
# SECTION 12: Close Run Log
# ============================================================
# Writes a footer to the run log (elapsed time, save path) and
# closes the sink so the file is fully flushed to disk.
# The closing message is written INSIDE the sink so it appears
# in the log, then after sink() the save-path confirmation is
# printed to the console only (so the user knows where to find it).

if (!is.null(RUN_LOG_PATH) && sink.number() > 0) {
  .run_log_end <- Sys.time()
  .run_log_dur <- as.numeric(difftime(.run_log_end, .run_log_start, units = "secs"))

  # Sum of per-plate elapsed times (excludes pre/post-processing overhead)
  .plate_time_total <- sum(
    summary_tbl$elapsed_s[summary_tbl$status == "processed"],
    na.rm = TRUE
  )
  .overhead <- .run_log_dur - .plate_time_total

  cat("\n", strrep("=", .pg_width), "\n", sep = "")
  cat(" End of run log\n")
  cat(strrep("-", .pg_width), "\n", sep = "")
  cat(sprintf(" Finished       : %s\n", format(.run_log_end, "%Y-%m-%d %H:%M:%S")))
  cat(sprintf(" Total duration : %.1fs\n",  .run_log_dur))
  cat(sprintf(" Plate time     : %.1fs  (sum of per-plate elapsed times)\n", .plate_time_total))
  cat(sprintf(" Overhead       : %.1fs  (file discovery, checks, logging)\n", .overhead))
  cat(sprintf(" Log file       : %s\n",    RUN_LOG_PATH))
  cat(strrep("=", .pg_width), "\n", sep = "")

  sink(type = "output")   # detach sink — output returns to console only
  close(.run_log_con)
  cat(sprintf("\n  Run log saved to: %s\n", RUN_LOG_PATH))
}