# ============================================================
#  qPCR Pipeline Runner
#  Requires: qpcr_cleaning_pipeline.R, qpcr_consolidation.R
#
#  PURPOSE:
#    Central configuration and entry point for the qPCR
#    processing pipeline. Set your run parameters here and
#    source this script to run either or both pipeline stages.
#    Any variable defined here takes precedence over the
#    defaults in the individual pipeline scripts.
#
#  USAGE:
#    1. Set RUN_CLEANING and RUN_CONSOLIDATION (TRUE/FALSE).
#    2. Adjust configuration variables as needed.
#       Defaults match the pipeline scripts — only change
#       what differs for this experiment.
#    3. Run the whole script (source() or Ctrl+Shift+Enter).
#
#  NOTES:
#    - The pipeline scripts can still be run standalone;
#      this file is optional.
#    - RUN_CONSOLIDATION requires RUN_CLEANING to have been
#      run at least once (outputs/ must exist).
#    - To run a fresh session without leftover variables
#      from a previous run, restart R before sourcing.
#
#  DEPENDENCIES:
#    install.packages(c("tidyverse", "openxlsx"))
# ============================================================

# ============================================================
#   qPCR Pipeline Runner
#   Requires: qpcr_cleaning_pipeline.R, qpcr_consolidation.R
# ============================================================

rm(list = c("INPUT_DIR", "OUTPUT_DIR", "TARGET_LOD"))  # list every variable explicitly

# 1. Source and execute integrity check
source("R/get_version.R")
status <- verify_pipeline_integrity()
SCRIPT_VERSION <- status$version

message("--------------------------------------------------")
message(" Running qPCR Pipeline Version: ", status$version)
message(" Integrity Check: ", if(status$integrity_passed) "PASSED" else "MODIFIED️")
message("--------------------------------------------------")

# 2. Set your run parameters here (Takes precedence over defaults)
# All defaults can be found in their respective scripts, or in the docs/defaults.md
RUN_CLEANING      <- TRUE
RUN_CONSOLIDATION <- TRUE

# --- Paths ---
INPUT_DIR    <- "RawData/"
OUTPUT_DIR   <- "outputs/"
CONSOLIDATION_DIR <- "consolidated/"

# Derived paths update automatically when the above change
ALL_OUT_PATH    <- file.path(CONSOLIDATION_DIR, "qpcr_all_samples.xlsx")
REVIEW_OUT_PATH <- file.path(CONSOLIDATION_DIR, "qpcr_review_samples.xlsx")

# --- QC settings ---
DELTA_CQ_THRESHOLD <- 1.0
N_STANDARDS        <- 6
SKIP_COMPLETED     <- TRUE

# --- LOD definitions (only needed if different from script defaults) ---
# TARGET_LOD <- list(
#   LOD_Hi = list(fucp = 2000, hpd3 = 2000, lyta = 2000, uni = 200),
#   LOD_Lo = list(fucp = 0.012, hpd3 = 0.012, lyta = 0.012, uni = 0.0012)
# )

# 3. Source the core processing scripts if requested
if (RUN_CLEANING) {
  message("Executing Stage 1: Cleaning Pipeline...")
  source("./R/qpcr_cleaning_pipeline.R")
}

if (RUN_CONSOLIDATION) {
  INPUT_DIR <- OUTPUT_DIR
  message("Executing Stage 2: Consolidation Pipeline...")
  source("./R/qpcr_consolidation.R")
}
