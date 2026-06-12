# Changelog

## 0.2.1-dev - current

### cleaning and consolidation pipeline
- Adjusted scripts to be accessible via source(), allowing for use in automation scripts or as standalones

### qcr_pipeline.R
- New script that allows for reproducible scripts that make variable adjustment easier without having to modify the script files themselves
- A defaults.md file have been created to provide an easier reference for what the script defaults are when adjusting the pipeline script

### get_version.R
- Script that gets the git version of the scripts used, defaulting to the CHANGELOG.md if offline, and unknown if either 

### General
- user_guide.md is presently outdated and will need updated prior to merging with main

## 0.2.0

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