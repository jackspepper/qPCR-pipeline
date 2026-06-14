# qPCR Data Processing Pipeline
## User Guide and Troubleshooting Reference

Scripts covered: `qpcr_cleaning_pipeline.R` · `qpcr_consolidation.R`

Script version 0.2.0 | For use with CFX Maestro exports

---

## About This Guide

This guide explains how to use the two R scripts that form your laboratory's qPCR data processing pipeline. It is written for biomedical researchers who are comfortable with qPCR experiments and CFX Maestro, but who may not have extensive experience writing or modifying R code.

**What the pipeline does:** Raw quantitative PCR data exported from CFX Maestro is cleaned, quality-checked, and flagged for manual review according to MIQE-aligned criteria. The cleaning script processes each plate individually and writes cleaned CSV files. The consolidation script then gathers those CSVs into structured Excel workbooks, one sheet per target, ready for downstream analysis.

**What you will need to do:** Before each run you will need to check a small number of settings at the top of each script, ensure your files are named correctly, and respond to a handful of yes/no prompts in the R console during processing. No programming knowledge is required.

> **Assumed starting point:** R and RStudio are already installed on your computer. You have run at least one R script before (i.e. you can open a script in RStudio and press Ctrl+Shift+Enter to run it). Your raw data was exported from CFX Maestro as CSV without any structural modifications to the file (sample names, target names, and Content labels may have been edited, but no columns have been added, removed, or reordered).

---

## Installing Required Packages

Each script requires one or more R packages to be installed before it will run. You only need to do this once per computer. Open RStudio, click on the Console panel at the bottom, and run the following command:

```r
install.packages(c("tidyverse", "openxlsx"))
```

Installation may take several minutes and will print a lot of text to the console — this is normal. You will see a message like `package X successfully unpacked` when each package is ready. If you see a red error message saying a package could not be installed, see the [Troubleshooting](#troubleshooting) section at the end of this guide.

> **Package version requirements:** The script checks that your installed packages are recent enough to run. If you installed R or tidyverse more than two years ago, you may be prompted to update. If an error appears mentioning a minimum version, run the install command above again — reinstalling updates to the latest version automatically.

---

## Setting Up Your Project Folder

Each experiment should have its own dedicated project folder. The scripts are designed to be placed at the root of this folder, with your raw CFX Maestro CSVs in a subfolder called `RawData`. When the cleaning script runs, it will automatically create the additional folders it needs (`outputs` and `audit`) if they do not already exist.

### Recommended folder layout

```
MyExperiment_2024/
├── RawData/
│     ├── 241212_Plate1_fucp_hpd3.csv
│     ├── 241212_Plate2_lyta_copb.csv
│     └── 241215_Plate3_fucp_hpd3.csv
│
├── qpcr_cleaning_pipeline.R        ← Script 1
├── qpcr_consolidation.R            ← Script 2
│
├── outputs/                        ← Created automatically by Script 1
│     ├── 241212_Plate1_fucp_hpd3_all_samples.csv
│     ├── 241212_Plate1_fucp_hpd3_review_samples.csv
│     └── ...
│
├── consolidated/                   ← Created automatically by Script 2
│     ├── qpcr_all_samples.xlsx
│     └── qpcr_review_samples.xlsx
│
└── audit/                          ← Created automatically by Script 1
      ├── pcr_decisions.csv
      ├── pcr_variables.csv
      ├── pcr_standards.csv
      ├── pcr_run_log.txt
      ├── pcr_consolidation_log.txt
      └── file_tree.txt
```

**Before you begin:** Open RStudio, then use **File → Open Project** or **File → Open File** to navigate to your experiment folder and open the cleaning script (`qpcr_cleaning_pipeline.R`). Ensure that RStudio's working directory is set to your experiment folder — you should see the correct path in the Files panel on the lower right. If it shows the wrong folder, use **Session → Set Working Directory → To Source File Location**.

---

## File Naming Conventions

The consolidation script (Script 2) reads the filename of each plate CSV to automatically populate two metadata columns in the Excel output: the run **Date** and the **Plate number**. If your filenames do not follow the expected conventions, these columns will be blank in your final workbook — the data itself is unaffected, but you will need to fill in those columns manually after consolidation.

### Recommended filename format

```
YYMMDD_PlateN_[target(s)][_optional-info].csv
```

Examples of filenames that will parse correctly:

```
241212_Plate1_fucp_hpd3.csv          → Date: 2024-12-12, Plate: 1
20241212_Plate2_lyta_copb.csv        → Date: 2024-12-12, Plate: 2
241215_Plate3_nuc_rpt2.csv           → Date: 2024-12-15, Plate: 3, Repeat: 2
241215_plate_no3_lyta.csv            → Date: 2024-12-15, Plate: 3
```

### Date parsing rules

The date must appear at the very beginning of the filename, followed immediately by an underscore. Both 6-digit (YYMMDD) and 8-digit (YYYYMMDD) formats are accepted.

| Format | Example | Parsed as |
|--------|---------|-----------|
| YYMMDD (recommended) | `241212_` | 2024-12-12 |
| YYYYMMDD | `20241212_` | 2024-12-12 |
| No date at start | `Plate1_fucp.csv` | (blank — will warn) |

### Plate number parsing rules

The word `plate` (case-insensitive) followed by a number should appear somewhere in the filename. Various separators and qualifiers are accepted:

```
Plate1    Plate_2    Plate-3    plate 4
plate#5   plate no. 6   PlateNo7
```

> **Limitation — not a blocker:** If a filename does not contain a parseable date or plate number, a warning will appear in the console during consolidation and those cells will be blank in the Excel output. Your data is still fully processed and consolidated — only those two metadata columns will be empty. You can fill them in manually in Excel after the run, or rename your files to follow the convention before re-running.

### Repeat number parsing

If a plate was repeated, you can include `rpt`, `rep`, or `repeat` followed by a number in the filename. This populates a `Repeat#` column in the consolidated Excel output:

```
241215_Plate3_fucp_rpt2.csv          → Repeat#: 2
241215_Plate3_fucp_rep2.csv          → Repeat#: 2
241215_Plate3_fucp_repeat2.csv       → Repeat#: 2
241215_Plate3_fucp_rpt.csv           → Repeat#: 1  (keyword without number = 1)
```

> **Reserved words in sample names — read carefully**
>
> The cleaning script automatically removes any row where the **Sample** column contains the words `Std`, `NTC`, or `Neg` (case-insensitive, checked as a partial match). These are treated as control or standard wells, not experimental samples. **Do not use these words in your real sample names.**
>
> Examples of names that **would be removed**:
> - Any sample containing `Std` — e.g. `Std-001`, `Std_High`
> - Any sample containing `NTC` — e.g. `NTC`, `NTC_plate2`
> - Any sample containing `Neg` — e.g. `NegControl`, `Neg_sample`

---

## Data Requirements and Assumptions

### Required CSV format

Your raw CSV files must be direct exports from CFX Maestro, without any structural modifications. The script dynamically locates the data table inside the file by searching for a row that begins with the word `Well` — this means CFX Maestro's metadata header rows at the top of the file are handled automatically and do not need to be removed.

**Minor edits that are acceptable:** Changing sample names, target names, and Content labels within CFX Maestro or directly in the CSV.

**Edits that will break the script:** Deleting, renaming, or reordering any of the columns listed below.

| Required column | Description |
|----------------|-------------|
| `Well` | Well identifier (e.g. A01, B02) |
| `Fluor` | Fluorophore channel (e.g. FAM, HEX, ROX) |
| `Target` | The gene/target being measured (e.g. fucp, hpd3, uni) |
| `Content` | Well type label — includes standard labels (Std-001 to Std-006), NTC, and sample identifiers |
| `Sample` | Sample name — your experimental sample identifier |
| `Cq` | Quantification cycle value |
| `Starting Quantity (SQ)` | Calculated starting quantity from the standard curve |

### Standards

Before processing any data, the script checks that all six expected standard wells are present in the `Content` column of each plate. Standards must be labelled as Std-001 through Std-006.

Both label formats are accepted:

| Label format | Example | Notes |
|-------------|---------|-------|
| Canonical (preferred) | `Std-001`, `Std-002`, `Std-003` … | No warning produced |
| Abbreviated | `Std-01`, `Std-02`, `Std-03` … | Warning in console and audit log, but still processed |

**Why this matters:** Standards Std-001 through Std-003 define the upper limit of detection (LOD Hi), and Std-004 through Std-006 define the lower limit (LOD Lo). If standards are missing, the script cannot reliably apply these thresholds and will ask you how to proceed.

### Targets and Limits of Detection (LOD)

Each target (gene) in your data has a defined upper and lower limit of quantification, set in Section 2 of the cleaning script.

| LOD boundary | Value | What it means |
|-------------|-------|---------------|
| LOD Hi (upper) | 2000 SQ (most targets); 200 SQ (uni/univ/universal) | Maximum reliable SQ from the standard curve. Samples above this are flagged — results are extrapolated and unreliable. |
| LOD Lo (lower) | 0.012 SQ (most targets); 0.0012 SQ (uni/univ/universal) | Minimum detectable SQ. Samples below this (or with no amplification) are set to LOD Lo ÷ 2 as a below-detection placeholder. |

> **Critical rule — targets with different LOD values cannot share a plate** plate**
>
> The pipeline processes all targets on a plate together and applies a single set of LOD values to all rows. If two targets on the same plate have different LOD Hi or LOD Lo values, the script will stop with an error. This means you cannot include both `uni` (LOD Hi = 200) and `fucp` (LOD Hi = 2000) on the same plate CSV. If you receive an error such as `Different Hi values found: 200, 2000`, check which targets are present in that plate file and separate them before re-running.

### Replicates

The script expects each experimental sample to have exactly two replicate wells per target (duplicates). Samples with one or more than two replicates will still appear in your output but will be flagged for manual review.

### The Universal Target (uni / univ / universal)

`uni`, `univ`, and `universal` are treated as an internal positive control — a target that should amplify in every sample. If any sample completely fails to amplify for this target or produces a value below LOD Lo, it is flagged as an "unexpected negative". All three spellings are recognised as the same target and consolidated onto a single sheet in the Excel output.

---

## Part 1: Running the Cleaning Pipeline

The cleaning pipeline (`qpcr_cleaning_pipeline.R`) is the first script to run. It reads your raw plate CSV files, applies quality control checks, and writes cleaned output files ready for the consolidation step.

### Step 1 — Check your settings (Section 1 of the script)

#### Input directory

```r
input_dir <- "RawData/"
```

This tells the script where to look for your plate CSV files. If you are following the recommended project folder structure, you do not need to change this.

#### Delta Cq threshold

```r
delta_cq_threshold <- 1.0
```

Controls how large a difference between duplicate Cq values is acceptable. The default of 1.0 means that if your two replicates differ by more than 1 Cq cycle, the pair is flagged for review. This is a commonly used MIQE-aligned threshold.

#### Skip completed plates

```r
skip_completed <- TRUE
```

When `TRUE`, the script checks whether output files already exist for each plate before processing. If both output CSVs already exist, you will be asked whether to skip that plate or reprocess it.

#### Number of standards

```r
n_standards <- 6
```

The number of standard wells expected on each plate. Change this only if your standard curve uses a different number of points.

### Step 2 — Check your LOD definitions (Section 2 of the script)

Section 2 defines the upper and lower limits of detection for each target. The script comes pre-configured with values for the targets used in your laboratory. You only need to edit this section if you are adding a new target that is not already listed.

If the script stops with an error such as `Missing targets in LOD_Hi: [targetname]`, a new target needs to be added to Section 2.

### Step 3 — Run the script

With the script open in RStudio, press **Ctrl+Shift+Enter** (Windows/Linux) or **Cmd+Shift+Enter** (Mac) to run the entire script. The script will begin immediately and print progress to the Console panel at the bottom of RStudio.

### Console Output Walkthrough

#### Run header

```
============================================================
 qPCR Pipeline Run Log
------------------------------------------------------------
 Directory  : C:/Users/jsmith/Projects/MyExperiment_2024
 Started    : 2024-12-12 09:14:22
 User       : jsmith
 Input      : RawData/
 Output     : outputs
 Audit      : audit
============================================================
```

Confirms the script has started and shows exactly which folder it is reading from and writing to.

#### File discovery tree

```
RawData  [depth requested: 2 │ deepest file: 0 level(s) │ 2 file(s) to process]
├── 241212_Plate1_fucp_hpd3.csv
└── 241212_Plate2_lyta_copb.csv

  File tree saved to: audit/file_tree.txt
```

Lists every CSV file the script found and will process. Check this carefully — if a file you expected is missing, it may be in the wrong folder or have the wrong extension.

#### Standards check (all plates pass)

```
------------------------------------------------------------
 Standards check: all 2 plate(s) passed
 Expected: Std-001, Std-002, Std-003, Std-004, Std-005, Std-006
------------------------------------------------------------
```

This is the ideal outcome. If the standards check fails for one or more plates, see [Interactive Decision Points](#interactive-decision-points) below.

#### Skip completed plates prompt

```
------------------------------------------------------------
 The following 1 of 2 plate(s) already have both output CSVs:
   - 241212_Plate1_fucp_hpd3
------------------------------------------------------------
 Skip these plates and process only the remaining ones?
 Enter Y to skip, N to reprocess all: _
```

**Y (skip):** The already-processed plate is left as-is and only the new plate is processed.  
**N (reprocess):** All plates are processed again from scratch.

#### Per-plate processing

```
[1/2] 241212_Plate1_fucp_hpd3.csv ════════════════════
  Import          │ header at row 19 │ 192 data rows
  LOD             │ Hi=2000     Lo=0.012    │ targets: fucp, hpd3
> Step 0 │ Removing unnamed samples ...
  Step 0 │ done │ 0 removed
> Step 1 │ SQ adjustment (below LOD_Lo → LOD_Lo / 2) ...
  Step 1 │ done │ 4 adjusted
> Step 2 │ Removing matched patterns (Std, NTC, Neg) ...
  Step 2 │ done │ 84 removed
> Step 3 │ Sample-name consistency check ...
  Step 3 │ done │ 0 group(s) with mismatched names
> Step 4 │ Replicate statistics and review flags ...
  Step 4 │ done │ 2 replicate group(s) flagged for review
> Step 5 │ Assembling output tables ...
  Step 5 │ done
────────────────────────────────────────────────────────────
  Output  │ 108 rows out │ 2 for review │ 1.3s │ 0.012s/row
```

Key things to check:
- **Import** — confirms the Well header was found and shows the row count before cleaning
- **LOD** — confirms which targets were found and which LOD values are being applied
- **Step 1** — shows how many SQ values were below LOD Lo; a high number may indicate poor assay sensitivity
- **Step 2** — shows how many rows were removed (standards, NTCs, negative controls)
- **Step 4** — shows how many replicate groups were flagged for review
- **Output** — final row count and review count

#### Run summary

```
============================================================
 Run complete
============================================================
 plate                      status      rows_in rows_out review elapsed_s
 241212_Plate1_fucp_hpd3    processed   192     108      4      1.3
 241212_Plate2_lyta_copb    processed   192     110      0      1.1

  Total rows processed : 218
  Total plate time     : 2.4s
  Mean rate            : 0.011s/row
```

---

### Interactive Decision Points

#### Standards check failure

If one or more plates are missing expected standard wells, the script prints a summary and offers options. Missing standards are classified into three scenarios:

| Scenario | What it means | Script label |
|----------|--------------|--------------|
| Both endpoints present, 1–2 interior standards missing | Std-001 and Std-006 are present. The LOD boundaries can still be inferred. | `[MIDDLE ONLY]` |
| Both endpoints present, 3+ interior standards missing | As above but more standards are missing. | `[MIDDLE ONLY — >2 missing]` |
| An endpoint standard is missing | Std-001 (defines LOD Hi) or Std-006 (defines LOD Lo) is absent. | `[ENDPOINT MISSING]` |

**Option S — Skip all failing plates:** The plates with missing standards are excluded from this run. Choose this if the plate run was compromised and the data should not be used.

**Option M — Mixed handling (recommended for middle-only failures):** The script will ask you to confirm or adjust the LOD values for middle-only plates using a Y/N prompt. Plates missing an endpoint standard will require you to manually enter LOD values.

For a plate with middle-only missing standards:

```
Targets : fucp
LOD Hi  [normal = 2000 │ Std-001 present → inferred; applies to all targets]
→ Is LOD Hi = 2000 correct for this plate? [Y/n]: _
```

**Y:** Accept the standard LOD Hi value. Choose this if you have no reason to believe the standard curve behaved differently.  
**n:** Enter a different value. Use this only if you have inspected the standard curve in CFX Maestro and determined a different value is appropriate.

**Option F — Force all plates through:** All failing plates are processed with manually entered LOD values for every plate.

#### Blank sample names

If a plate has no sample names at all in the Sample column:

```
------------------------------------------------------------
 Sample names check: 1 plate(s) have NO sample names
------------------------------------------------------------
  241212_Plate3_fucp
    Rows    : 192
    Content : Std-001, Std-002, ..., Unknown, NTC
------------------------------------------------------------
 Options:
   S = Skip all   — exclude these plates from this run
   C = Use Content column as Sample name for all these plates
 Enter S or C: _
```

**Option S — Skip:** The plate is excluded from the run. Choose this if you want to add sample names before reprocessing.

**Option C — Use Content column as Sample name:** The Content column is substituted for the missing Sample name. This is logged in the audit trail. Choose this only as a temporary measure.

---

## Part 2: Running the Consolidation Script

After the cleaning pipeline has finished, run the consolidation script (`qpcr_consolidation.R`). This reads all the cleaned CSV files from the `outputs` folder and assembles them into two Excel workbooks.

### Step 1 — Check your settings (Section 1 of the script)

#### Input directory

```r
input_dir <- "outputs/"
```

This should match the `output_dir` from the cleaning pipeline.

#### Output location

```r
CONSOLIDATION_DIR <- "consolidated"
ALL_OUT_PATH    <- file.path(CONSOLIDATION_DIR, "qpcr_all_samples.xlsx")
REVIEW_OUT_PATH <- file.path(CONSOLIDATION_DIR, "qpcr_review_samples.xlsx")
```

#### Target alias mapping

```r
TARGET_ALIASES <- list(
  uni = c("uni", "universal", "univ")
)
```

Treats `uni`, `universal`, and `univ` as the same target and consolidates them onto a single sheet named `uni`. You do not normally need to change this.

### Step 2 — Run the script

Press **Ctrl+Shift+Enter** (or **Cmd+Shift+Enter** on Mac).

### The output file conflict prompt

If the output Excel files already exist from a previous run:

```
------------------------------------------------------------
 All-samples workbook already exists:
  Modified : 2024-12-10 14:32:05
  Size     : 243.1 KB
  Sheets   : run_summary, fucp, hpd3, lyta, uni (5 sheets)
------------------------------------------------------------
 Options:
   O = Overwrite  — discard existing workbook and rebuild
   A = Append     — merge new data into existing workbook
   N = New file   — save under a different name
 Enter O, A, or N: _
```

**O — Overwrite:** The existing workbook is deleted and rebuilt from scratch. Use this when you have reprocessed all plates or want a completely fresh consolidation.

**A — Append:** New data from freshly processed plates is merged into the existing workbook. Duplicate rows are removed automatically. Use this when adding new plates to an ongoing experiment.

**N — New file:** The existing workbook is kept and the new consolidation is saved under a different name. You will be prompted to enter a suffix, or press Enter to use a timestamp automatically.

---

## Understanding Your Output Files

### Cleaning pipeline outputs (in the `outputs/` folder)

For each plate CSV processed, two CSV files are written:

**`[platename]_all_samples.csv`** — Every experimental row after cleaning. Standards, NTCs, and rows with no sample name have been removed, but all remaining rows are present regardless of whether they passed QC.

**`[platename]_review_samples.csv`** — Only rows flagged as needing manual review. This is a subset of the `_all_samples` file. If a replicate group is flagged, both rows of that pair appear here.

### Column descriptions

| Column | Source | Description |
|--------|--------|-------------|
| `Well` | CFX Maestro | Well position (e.g. A01, B02) |
| `Fluor` | CFX Maestro | Fluorophore channel (e.g. FAM, HEX) |
| `Target` | CFX Maestro | Gene/target name |
| `Content` | CFX Maestro | Well type from CFX Maestro (Unkn, Std-001, NTC, etc.) |
| `Sample` | CFX Maestro | Your sample identifier |
| `Cq` | CFX Maestro | Quantification cycle value. Blank for no-amplification samples or groups with >2 replicates. |
| `Starting Quantity (SQ)` | Modified by pipeline | SQ value. If the original was below LOD Lo or absent, set to LOD Lo ÷ 2. Blanked for groups with >2 replicates. |
| `DeltaCq` | Added by pipeline | Absolute difference between replicate Cq values. Only on the first row of each pair. |
| `AverageSQ` | Added by pipeline | Mean of replicate SQ values after LOD Lo adjustment. Only on the first row of each pair. |
| `review_reason` | Added by pipeline | Plain-English explanation of why a row was flagged. Blank if the row passed all checks. Multiple reasons separated by semicolons. |

### Review flags — what they mean and what to do

| Flag | What it means | Common causes | Suggested action |
|------|--------------|---------------|-----------------|
| `\|DeltaCq\| exceeds threshold` | Duplicate Cq values differ by more than 1.0 cycle. | Pipetting error, well-to-well variation, partial inhibition. | Check amplification curves in CFX Maestro. If one looks abnormal, exclude that replicate and note it. |
| `Only one replicate present` | Only a single well found for this sample/target. | Replicate not plated, failed to amplify, or excluded upstream. | Check the plate layout. Determine whether the single value is usable or if re-running is needed. |
| `One Cq is NA/NaN and the other is numeric` | One replicate amplified and one did not. | Partial inhibition, failed well, insufficient template. | Inspect both wells in CFX Maestro. Consider excluding the non-amplifying well. |
| `More than 2 replicates — manual reconciliation required` | Three or more wells found for this group. Cq and SQ are blanked. | Sample plated in triplicate, mislabelled layout, or two plates with the same sample names. | **Required action:** open `_all_samples.csv`, locate the group, determine the intended duplicates, and enter correct values manually. |
| `AverageSQ > LOD_Hi` | Average SQ exceeds the top of the standard curve. | Very high bacterial load, insufficient dilution. | Consider repeating with a higher dilution. Note the limitation if using the result. |
| `Unexpected negative for always-positive target (No amplification)` | Universal target produced no Cq value in any replicate. | Degraded DNA, very low concentration, failed extraction. | Investigate extraction quality before drawing conclusions from any target for this sample. |
| `Unexpected negative for always-positive target (Below LOD_Lo)` | Universal target produced Cq but SQ was below LOD Lo. | Very low bacterial DNA, poor extraction, borderline inhibition. | Interpret all results for this sample with caution. |
| `Sample name mismatch within (Fluor, Target, Content)` | Two or more different sample names found for the same Content/Target/Fluor combination. | Sample given different names in replicate wells, copy-paste error. | Check and correct sample names in CFX Maestro. Re-export and reprocess. |

### Consolidated Excel workbooks (in the `consolidated/` folder)

The consolidation script produces two Excel workbooks: `qpcr_all_samples.xlsx` and `qpcr_review_samples.xlsx`.

**Sheet layout:**
- The first sheet is always `run_summary` — a dashboard showing QC counts, standards check outcomes, and sample counts per plate and per target
- Each subsequent sheet contains data for one target (e.g. `fucp`, `hpd3`, `lyta`, `uni`)
- Each sheet is formatted as an Excel Table with auto-filters and a frozen header row

**Additional columns added by the consolidation script:**

| Column | Description |
|--------|-------------|
| `File Name` | The plate CSV filename the row came from. |
| `Date` | Run date parsed from the filename (YYYY-MM-DD). Blank if not parseable. |
| `Plate#` | Plate number parsed from the filename. Blank if not parseable. |
| `Repeat#` | Repeat number parsed from the filename. Blank if not present. |

**The `run_summary` sheet contains four sections:**
1. **Decision Breakdown** — how many times each QC rule fired per plate
2. **Standards Check Results** — pass/skipped/forced outcome per plate
3. **Final Sample Counts** — plate × target row counts for `all_samples`
4. **Review Sample Counts** — plate × target row counts for `review_samples`

---

## The Audit Folder

Every run writes to a set of log files in the `audit/` folder. These provide a complete, timestamped record of every decision made during processing.

### `pcr_decisions.csv`

One row per QC decision made on every sample and target.

| Column | Description |
|--------|-------------|
| `timestamp` | UTC date and time the decision was recorded |
| `user` | Windows/Mac username of whoever ran the script |
| `run_id` | Unique identifier for the plate processing run (format: `YYYYMMDD_HHMMSS_platename`) |
| `input_file` | The original plate CSV filename |
| `sample_id` | The sample identifier the decision applies to |
| `target` | The target (gene) the decision applies to |
| `rule_id` | The QC rule evaluated (e.g. `RV_DELTA_CQ`, `PASS_NEGATIVE`) |
| `outcome` | Whether the rule was `applied`, `skipped`, or resulted in a `pass` |
| `evidence` | Brief description of the values that led to the decision |
| `source` | Always `R_script` |
| `version` | Script version number |

> **Practical use:** To review all decisions for a specific plate, filter the `input_file` column. To see only flagged results, filter `outcome` to `applied`. To compare two runs of the same plate, filter by `run_id`.

### `pcr_variables.csv`

One row per configuration parameter used during each plate's processing run.

| Column | Description |
|--------|-------------|
| `var_name` | Parameter name (e.g. `dCq_thr`, `lod_hi`, `lod_lo`, `rm_patterns`) |
| `var_value` | The value of that parameter at the time of the run |
| `var_class` | The R data type of the value |

> **Practical use:** To verify what thresholds were applied to a historical plate run, filter by `run_id` and look at the `dCq_thr`, `lod_hi`, and `lod_lo` rows.

### `pcr_standards.csv`

One row per plate per run, recording the outcome of the standards check.

| Column | Description |
|--------|-------------|
| `n_expected` | How many standards were expected |
| `expected_standards` | Full list of expected labels, separated by pipes |
| `found_standards` | Standard labels actually found in the plate |
| `missing_standards` | Standards expected but not found. Empty if the plate passed. |
| `action` | `pass`, `skipped`, or `forced` |
| `lod_override` | LOD values entered manually if `action` was `forced`. Otherwise blank. |
| `notes` | Additional context |

### `pcr_run_log.txt`

A plain-text transcript of everything printed to the R console during the cleaning pipeline run. Overwritten each time the script runs.

> **Note:** If the script exits with an error before completing, the log file may be left in an incomplete state. If subsequent console output stops appearing in RStudio, type `sink()` in the console and press Enter to restore normal output.

### `pcr_consolidation_log.txt`

Equivalent to `pcr_run_log.txt` but for the consolidation script.

### `file_tree.txt`

A snapshot of the folder structure and file list discovered at the start of each cleaning pipeline run.

---

## Troubleshooting

### Script will not start or package errors

**`Error: tidyverse is not installed`**

Run `install.packages("tidyverse")` in the R console and wait for it to complete, then try again.

**`Error: dplyr >= 1.1.0 required (installed: 1.0.5)`**

Your installed version is too old. Run `install.packages("tidyverse")` to update.

**Console output stops appearing after a script error**

Type the following in the console and press Enter:

```r
sink()
```

This closes the log file connection the script opened. If the problem persists, restart RStudio.

---

### File discovery problems

**`Error: No matching files found`**

Check:
- Your working directory in RStudio is set to the project folder (**Session → Set Working Directory → To Source File Location**)
- The `RawData` folder exists and contains `.csv` files
- The `input_dir` setting in Section 1 matches your actual folder name (case-sensitive on Mac and Linux)

**`Could not find a row with 'Well' in column 1`**

The file does not contain the expected `Well` header row. Possible causes:
- The file was saved from different software or a different export format
- The file was opened and re-saved in Excel, which may have corrupted the format — always work with copies, not originals
- Ensure you are exporting as "Quantification Results" CSV from CFX Maestro

**`Missing required columns`**

A required column is missing or has an unexpected name. Most common cause: the plate was exported without a required column, or a column was renamed after export. Always use the original, unmodified export file.

---

### LOD and target errors

**`Error: Missing targets in LOD_Hi: newgene`**

A target in your data is not defined in Section 2. Add the target's LOD values to Section 2 following the existing format.

**`Error: Different Hi values found: 200, 2000`**

Your plate contains targets with different LOD values (e.g. `uni` and `fucp` on the same plate file). Split the plate's CSV data into separate files — one per LOD group — before running the script.

---

### Standards check failures

**A plate repeatedly fails the standards check**

Check:
- Standards in the Content column must be labelled exactly as `Std-001` through `Std-006` (or `Std-01` through `Std-06`). Other formats such as `Standard 1`, `S1`, or `STD001` will not be recognised.
- If your assay uses a different number of standards, update `n_standards` in Section 1.
- If you run standards on a single plate and apply the curve to others, set `std_check_enabled <- FALSE` in Section 1 and provide LOD values via `std_force_lod`.

---

### Output file problems

**`[write retry 1/5] Could not write to '...'`**

The script retries up to 5 times. This usually means the output file is open in Excel. Close it and the script will succeed on the next retry.

**`Error: No matching files found in outputs/`**

Run the cleaning pipeline (Script 1) first, or check that `input_dir` in the consolidation script matches `output_dir` from the cleaning pipeline.

---

### Unexpected results in the output

**My sample rows are missing from the output**

Most common reasons:
- Sample name contained a reserved word (`Std`, `NTC`, or `Neg`) — rows are removed in Step 2
- Sample name was blank — rows are removed in Step 0
- Plate was skipped — check the run summary table in the console output

**DeltaCq and AverageSQ are blank for some rows**

Expected in two situations:
- **Second row of a replicate pair** — these values are only on the first row of each pair. Look one row up.
- **More than 2 replicates (`RV_EXCESS_REPS` flag)** — all Cq, SQ, DeltaCq, and AverageSQ are deliberately blanked. The `review_reason` column will explain this. You must resolve it manually.

**A target is not showing as a sheet in Excel**

Check:
- The target name in the CSV must match a name in Section 2 of the cleaning script
- Check `TARGET_ALIASES` in the consolidation script if the target uses multiple spellings
- Target names are normalised to lowercase — look for the sheet under the lowercase version of the name

---

## Quick Reference

### Standard workflow

1. Export plate data from CFX Maestro as CSV (Quantification Results format, no structural modifications)
2. Rename each file to `YYMMDD_PlateN_[targets].csv` and place in the `RawData/` folder
3. Open `qpcr_cleaning_pipeline.R` in RStudio. Verify `input_dir` points to your `RawData` folder
4. Press **Ctrl+Shift+Enter** to run Script 1. Respond to any interactive prompts
5. Check the run summary — confirm all plates show `processed` status and row counts look reasonable
6. Open `qpcr_consolidation.R`. Verify `input_dir` is set to `outputs/`
7. Press **Ctrl+Shift+Enter** to run Script 2. Choose O/A/N if prompted about existing workbooks
8. Open `qpcr_all_samples.xlsx` and `qpcr_review_samples.xlsx` from the `consolidated/` folder
9. Review any rows in the review workbook and investigate flagged results before using them in your analysis

### Reserved words — never use in sample names

| Reserved word | What it matches (examples) | Effect |
|--------------|---------------------------|--------|
| `Std` | Std, Std-001, STD_curve, MyStd | Row removed from output |
| `NTC` | NTC, ntc, NTC_plate2 | Row removed from output |
| `Neg` | Neg, NegControl, Negative_sample, neg | Row removed from output |

### Review flag summary

| `review_reason` text | Meaning | Urgency |
|----------------------|---------|---------|
| `\|DeltaCq\| exceeds threshold` | Duplicates differ by > 1 Cq | Review |
| `Only one replicate present` | Single well only | Review |
| `One Cq is NA/NaN and the other is numeric` | Mixed amplification in duplicates | Review |
| `More than 2 replicates — manual reconciliation required` | 3+ wells for this group; Cq/SQ blanked | **Action required** |
| `AverageSQ > LOD_Hi` | Above standard curve range | Review |
| `Unexpected negative for always-positive target (No amplification)` | uni/univ/universal did not amplify | Investigate sample quality |
| `Unexpected negative for always-positive target (Below LOD_Lo)` | uni/univ/universal below detection | Investigate sample quality |
| `Sample name mismatch within (Fluor, Target, Content)` | Inconsistent sample labelling | Correct in CFX Maestro and reprocess |

### Interactive prompts at a glance

| Prompt | Options | Choose when |
|--------|---------|-------------|
| Skip completed plates? | Y = skip, N = reprocess all | Y when adding new plates to an existing experiment. N when you have changed data or settings. |
| Standards check failed | S = skip, M = mixed handling, F = force all | S if plate data is unreliable. M if only interior standards are missing. F if you have manually verified LOD values. |
| Blank sample names | S = skip, C = use Content as sample name | S if you want to add sample names first. C only as a temporary measure. |
| Output workbook already exists | O = overwrite, A = append, N = new file | O for a fresh run. A when adding new plates. N to preserve the existing version. |