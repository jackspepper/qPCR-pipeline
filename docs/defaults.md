# Defaults

For explanations and examples of below variable usage, see the [user_guide.md](user_guide.md)

------------------------------------------------------------------------

## qPCR Cleaning Pipeline

Script: [qpcr_cleaning_pipeline.R](../R/qpcr_cleaning_pipeline.R)

### Variables

| Variable                | Value                         |
|-------------------------|-------------------------------|
| SCRIPT_VERSION          | Calculated at script run      |
| INPUT_DIR               | "RawData/"                    |
| FILE_PATTERN            | "\\.csv\$"                    |
| FILES                   | NULL                          |
| SEARCH_DEPTH            | 2                             |
| TREE_OUTPUT             | "both"                        |
| TREE_PATH               | "audit/file_tree.txt"         |
| OUTPUT_DIR              | "outputs"                     |
| DEC_LOG_PATH            | "audit/pcr_decisions.csv"     |
| VAR_LOG_PATH            | "audit/pcr_variables.csv"     |
| DELTA_CQ_THRESHOLD      | 1.0                           |
| RM_PATTERNS             | c("Std", "NTC", "Neg")        |
| RM_COLUMNS              | c("sample")                   |
| ENABLE_PREVIEW          | FALSE                         |
| DRY_RUN                 | FALSE                         |
| DEBUG_PRINT             | FALSE                         |
| SKIP_COMPLETED          | TRUE                          |
| STD_CHECK_ENABLED       | TRUE                          |
| N_STANDARDS             | 6                             |
| STD_FORCE_LOD           | NULL                          |
| STD_LOG_PATH            | "audit/pcr_standards.csv"     |
| RUN_LOG_PATH            | "audit/pcr_run_log.txt"       |
| ALWAYS_POSITIVE_TARGETS | c("uni", "univ", "universal") |

### Targets

| Target | LOD Hi | LOD Lo | Included Alternative Names         |
|--------|--------|--------|------------------------------------|
| fucp   | 2000   | 0.012  | hi_fucp, hi-fucp, hi fucp          |
| hpd3   | 2000   | 0.012  | hi_hpd3, hi-hpd3, hh_hpd3, hh_hypd |
| lyta   | 2000   | 0.012  |                                    |
| copb   | 2000   | 0.012  |                                    |
| speb   | 2000   | 0.012  |                                    |
| nuc    | 2000   | 0.012  |                                    |
| gyrb   | 2000   | 0.012  |                                    |
| uni    | 200    | 0.0012 | univ, universal                    |

## qPCR Consolidation

Script: [qpcr_consolidation.R](../R/qpcr_consolidation.R)

| Variable                   | Value                                                    |
|----------------------------|----------------------------------------------------------|
| SCRIPT_VERSION             | Calculated at script run                                 |
| INPUT_DIR                  | "outputs/"                                               |
| ALL_PATTERN                | "\_all_samples\\.csv\$"                                  |
| REVIEW_PATTERN             | "\_review_samples\\.csv\$"                               |
| CONSOLIDATION_DIR          | "consolidated"                                           |
| ALL_OUT_PATH               | file.path(CONSOLIDATION_DIR, "qpcr_all_samples.xlsx")    |
| REVIEW_OUT_PATH            | file.path(CONSOLIDATION_DIR, "qpcr_review_samples.xlsx") |
| TABLE_STYLE                | "TableStyleMedium2"                                      |
| FONT_NAME                  | "Arial"                                                  |
| FONT_SIZE                  | 10                                                       |
| COL_WIDTH_MIN              | 10                                                       |
| MAX_WRITE_TRIES            | 5                                                        |
| WAIT_SECS                  | 3                                                        |
| UPDATE_TARGET_TO_CANONICAL | TRUE                                                     |
| AUDIT_DIR                  | "audit/"                                                 |
| CONSOLIDATION_LOG_PATH     | "audit/pcr_consolidation_log.txt"                        |

### Target Aliases

| Canonical | Targets              |
|-----------|----------------------|
| uni       | uni, univ, universal |
