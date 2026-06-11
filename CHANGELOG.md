# Changelog

## 0.2.0 — current

### Cleaning pipeline
- Standards pre-check with interactive LOD override prompts
- Sample names pre-check with Content-as-Sample fallback
- RV_UNEXPECTED_NEG flag for always-positive targets
- Skip-completed-plates option
- Retry logic for network path writes
- Run log saved to audit/

### Consolidation
- run_summary sheet with decision breakdown and sample count pivots
- Target alias mapping (uni / univ / universal)
- Append / overwrite / new-file prompt for existing workbooks
- Repeat# column parsed from filename