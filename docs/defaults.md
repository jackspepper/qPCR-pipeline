# Defaults

For explanations and examples of below variable usage, see the [user_guide.md](user_guide.md)

------------------------------------------------------------------------

## qPCR Cleaning Pipeline

Script: [qpcr_cleaning_pipeline.R](../R/qpcr_cleaning_pipeline.R)

### Variables

| Variable                | Value                         |
|-------------------------|-------------------------------|
| input_dir               | "RawData/"                    |
| file_pattern            | "\\.csv\$"                    |
| files                   | NULL                          |
| search_depth            | 2                             |
| tree_output             | "both"                        |
| tree_path               | "audit/file_tree.txt"         |
| output_dir              | "outputs"                     |
| dec_log_path            | "audit/pcr_decisions.csv"     |
| var_log_path            | "audit/pcr_variables.csv"     |
| delta_cq_threshold      | 1.0                           |
| rm_patterns             | c("Std", "NTC", "Neg")        |
| rm_columns              | c("sample")                   |
| enable_preview          | FALSE                         |
| dry_run                 | FALSE                         |
| debug_print             | FALSE                         |
| skip_completed          | TRUE                          |
| std_check_enabled       | TRUE                          |
| n_standards             | 6                             |
| std_force_lod           | NULL                          |
| std_log_path            | "audit/pcr_standards.csv"     |
| run_log_path            | "audit/pcr_run_log.txt"       |
| always_positive_targets | c("uni", "univ", "universal") |

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
| input_dir                  | "outputs/"                                               |
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
