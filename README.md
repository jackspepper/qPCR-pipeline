# qPCR Data Processing Pipeline

[![Run qPCR Pipeline Tests](https://github.com/jackspepper/qPCR-pipeline/actions/workflows/run-tests.yml/badge.svg)](https://github.com/jackspepper/qPCR-pipeline/actions/workflows/run-tests.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

An installable R package for MIQE-aligned cleaning and consolidation of CFX Maestro qPCR exports.

## What it does

- **Cleaning pipeline** ([`R/clean.R`](file:///D:/R_Projects/qPCR-pipeline-1/R/clean.R)): reads raw plate CSVs, applies QC checks, and writes cleaned outputs and an audit log.
- **Consolidation script** ([`R/consolidate.R`](file:///D:/R_Projects/qPCR-pipeline-1/R/consolidate.R)): gathers cleaned CSVs into structured Excel workbooks, one sheet per target.

## Installation

You can install the package directly from GitHub:

```r
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("jackspepper/qPCR-pipeline")
```

## Quick Start / Usage

### 1. Initialize a New Project Directory
To set up a new experiment directory with the recommended folder structure and a runner script template:

```r
library(qpcrpipeline)
use_qpcr_template("MyExperiment_2026")
```

### 2. Run the Pipeline using R Pipes (`|>` or `%>%`)
The package functions are designed to return directory paths, making them fully compatible with piped workflows. You can clean raw plate data and consolidate the results in a single pipe chain:

```r
library(qpcrpipeline)

"RawData/" |>
  run_cleaning_pipeline(
    output_dir = "outputs/",
    delta_cq_threshold = 1.0,
    n_standards = 6
  ) |>
  run_consolidation_pipeline(
    consolidation_dir = "consolidated/"
  )
```

## Requirements

- R >= 4.1.0 (for native pipe support, though `magrittr` `%>%` is supported on older versions)
- Packages: `dplyr`, `tidyr`, `openxlsx`, `readr`, `stringr`, `purrr`, `cli`, `fs`, `rlang`, `lubridate`

## Documentation

See the [full user guide](docs/user_guide.md) for detailed instructions, file naming conventions, and troubleshooting.

## Citation

If you use this pipeline in published work, please cite:
[add your details here]

## Licence

MIT — see [LICENSE](LICENSE)