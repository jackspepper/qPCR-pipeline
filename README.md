# qPCR Data Processing Pipeline

MIQE-aligned cleaning and consolidation pipeline for CFX Maestro qPCR exports.  
Script version 0.2.0

## What it does

- **Cleaning pipeline** (`R/qpcr_cleaning_pipeline.R`): reads raw plate CSVs,
  applies QC checks, and writes cleaned outputs and an audit log.
- **Consolidation script** (`R/qpcr_consolidation.R`): gathers cleaned CSVs
  into structured Excel workbooks, one sheet per target.

## Getting started

The easiest way to get a local copy is with one R command:

```r
install.packages("usethis")
usethis::use_course("https://github.com/jackspepper/qPCR-pipeline/archive/main.zip")
```

This downloads and opens the project in RStudio automatically.

## Requirements

```r
install.packages(c("tidyverse", "openxlsx"))
```

tidyverse >= 2.0 and openxlsx >= 4.2.0 are required.

## Usage

See the [full user guide](docs/user_guide.md) for detailed instructions,
file naming conventions, and troubleshooting.

## Citation

If you use this pipeline in published work, please cite:
[add your details here]

## Licence

MIT — see [LICENSE](LICENSE)