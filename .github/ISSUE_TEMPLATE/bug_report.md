---
name: Bug report
about: Create a report to help us improve the pipeline
title: '[BUG] Brief description of the issue'
labels: bug
assignees: ''

---

**Describe the bug**
A clear and concise description of what the bug is.

**To Reproduce**
Steps to reproduce the behavior:
1. Open the runner script or the specific pipeline script.
2. Run the script with settings/data...
3. See error: (paste console log output or error messages here)

**Example Data Shape**
- What targets are present in the plate CSV? (e.g. `fucp`, `uni`)
- Are standards present in the `Content` column? (e.g. `Std-001` to `Std-006`)
- How many replicates per sample?
- If possible, paste a small snippet of the first few rows of the raw CSV file.

**Session Info & Environment**
Please run `sessionInfo()` in your R console and paste the output below, or at least specify:
- R version (e.g., 4.3.1)
- `tidyverse` package version
- `openxlsx` package version
- Operating System (Windows/Mac/Linux)

**Additional context**
Add any other context about the problem here (e.g., screenshots, specific CFX Maestro export settings used).
