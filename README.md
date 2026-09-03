# Analysis Kit

An R Shiny application for the Mixed Migration Centre. It turns a 4Mi dataset
and a **List of Analysis** workbook into a branded, MMC-styled results workbook —
without anyone editing an R script for each round.

You upload two files, the app checks that they belong together, you choose where
the output goes, and one click produces the `.xlsx`.

---

## Contents

- [What it does](#what-it-does)
- [Getting started](#getting-started)
- [The two inputs](#the-two-inputs)
- [The List of Analysis workbook](#the-list-of-analysis-workbook)
- [The checks](#the-checks)
- [Running an analysis](#running-an-analysis)
- [The output](#the-output)
- [Deploying to Posit Cloud](#deploying-to-posit-cloud)
- [Repository layout](#repository-layout)
- [Running the tests](#running-the-tests)
- [Known limits](#known-limits)

---

## What it does

The workflow is five steps, and the app shows you which one you are on:

| Step | What happens |
|---|---|
| **Dataset** | Upload the 4Mi export. The app reads it and profiles every column. |
| **List of Analysis** | Upload the workbook that says which analyses to run. |
| **Checks** | Every check runs automatically, comparing the two files against each other. |
| **Destination** | Choose the folder to save into. Served, this step is called **Delivery**, settles itself, and the workbook arrives as a download instead. |
| **Results** | Run the analyses and get the workbook. |

Nothing expensive happens until you press **Run analyses**. Uploading a file
re-runs the checks — which take milliseconds — but never the analysis.

---

## Getting started

### Requirements

R 4.1 or later. The app installs what it needs from CRAN on first launch:

`shiny`, `readxl`, `shinyFiles`, `dplyr`, `tidyr`, `stringr`, `openxlsx`

Three more are **optional** and are *not* installed for you, because a run only
reaches them if a workbook asks for them:

| Package | Needed for | Install |
|---|---|---|
| `srvyr` | confidence intervals (an analysis row with a `level`) | `install.packages("srvyr")` |
| `analysistools` | the same | `remotes::install_github("impact-initiatives/analysistools")` |
| `cleaningtools` | rebuilding select_multiple parent columns | `remotes::install_github("impact-initiatives/cleaningtools")` |

If you never set a `level`, you never need them — the fast tabulation engine
produces the same point estimates without a survey design.

### Run it

Open `analysiskit.Rproj` in RStudio and:

```r
shiny::runApp()
```

On launch you will see either a short note about the optional packages, or a
clear message naming anything required that is missing and the command to
install it. The app stops before the interface appears rather than failing
halfway through a run.

---

## The two inputs

**The dataset** — a 4Mi ONA export, `.csv` or `.xlsx`, up to about 100 MB.
Row 1 is expected to be the label row: ONA puts the question text there, and the
app sets it aside so it never counts as a respondent. Choice labels live in the
select_multiple child columns (`Q78/Economic reasons`), which is how ONA exports
them.

**The List of Analysis** — the workbook below. `.xlsx` is strongly preferred:
a `.csv` carries only one table, so everything except the analysis list falls
back to defaults, and the app tells you so.

---

## The List of Analysis workbook

📥 **[Download the template](docs/loa_template.xlsx)** — also available from the
link in the app's sidebar, which is the only route to it if you are using a
deployed copy.

📄 The full specification, including every setting and every validation rule, is
in **[docs/loa-schema.md](docs/loa-schema.md)**. What follows is the short
version.

Up to seven sheets. Only `analysis` is required; a missing sheet means "not
requested" and the pipeline's own default applies.

### `analysis` — one row per requested analysis

| Column | Notes |
|---|---|
| `analysis_type` | `prop_select_one`, `prop_select_multiple`, `mean`, `median` or `ratio` |
| `analysis_var` | the dataset variable, or the select_multiple parent |
| `level` | leave empty for no confidence interval. `0.95` asks for one |
| `sector` | free metadata — becomes one sheet per value in the output |

### `group_analysis` — the disaggregations, and their output names

| raw_data_name | new_name |
|---|---|
| Q27 | Respondent_Gender |
| Q42 | Region_of_interview |

**Write raw question codes everywhere in the workbook.** The app applies this
mapping to the dataset *and* to every other sheet, so `Q27` works in the
`analysis` sheet too and the readable name appears only in the output.

`Overall` is added automatically and must not be listed.

### `count_selections` — how many choices each respondent picked

One column, `analysis_var`, one row per select_multiple parent.

### `count_combinations` — which combination of choices they picked

| analysis_var | choice_label | display_name |
|---|---|---|
| Q78 | Economic reasons | Economic |
| Q78 | Armed conflict, generalised violence, and insecurity | Conflict |

Gives four mutually exclusive rows — *Economic + Conflict*, *Economic*,
*Conflict*, *None of these* — that add to 100%. `choice_label` must match the
export exactly, punctuation and all.

### `count_exclusive_combinations` — the strict version

Same three columns. `count_combinations` asks *"selected Economic, whatever
else"*; this asks *"selected Economic and nothing else at all"*. Rows read
*Economic only*, and the catch-all is *Other choices only*. A question can carry
both blocks.

> **These rows use a smaller denominator than every other table in the output.**
> Anyone who picked a listed choice together with an unlisted one belongs to no
> category and leaves the base. The app reports how many that is per question, as
> a warning on the Results tab — footnote it wherever you publish these
> percentages.

### `exclude_choices` — labels that leave the denominator

One `choice_label` per row (`Don't know`, `Refused`). Its own sheet rather than
a setting because 4Mi labels contain commas, and a comma-separated list would
split one label into three.

Note what this does: it changes the **denominator**. Someone who picked an
excluded choice leaves the base for that question entirely — the rows are not
merely hidden.

### `settings` — everything else

Two columns, `setting` and `value`. 42 keys are accepted, covering every
remaining argument of the analysis pipeline. A misspelled key is a *fatal error*,
not a silent skip — a typo that was ignored would look exactly like a setting
that had been applied.

```
sm_separator        /
value_columns       stat,n,n_total
extra_columns       sector
engine              auto
```

Wrap a value in double quotes to keep a leading or trailing space — a
spreadsheet drops them, so `" only"` and `" + "` need the quotes or the label
renders `Economiconly`.

Sheets named `readme` or `notes`, or beginning with `_`, are ignored, so the
workbook can carry its own instructions.

---

## The checks

Both files are compared against each other the moment they are both in — no
button. The **List of Analysis** tab shows what was read, what is wrong, and
which variables were found.

The severity rule throughout:

> **Must fix** when the run would produce *wrong or misleading* output.
> **Warning** when it would only produce *less* output.

So a variable named in the workbook but absent from the dataset **warns** — that
analysis is skipped, the rest still runs. But two grouping variables whose names
contain one another is a **must fix**, because the output table would be correct
while the map that labels its columns would not.

**Variable coverage** lists every dataset variable the workbook names, where it
is named, and whether your dataset has it. It is the quickest way to spot a
List of Analysis from the wrong round.

The run button stays disabled until nothing is fatal.

---

## Running an analysis

Choose an output folder, then press **Run analyses**.

The progress bar carries the pipeline's own messages — which variables were
skipped, which choices were excluded, which groups were set aside — and all of
them are kept in the **Run log** on the Results tab. Those are diagnostics worth
reading; they would otherwise vanish into the console.

Nothing is written to disk unless the run finishes without error. If the analysis
succeeds and only the saving fails, the results are kept and the message says so.

---

## The output

One MMC-branded `.xlsx`, styled by `format_my_xlsx_variable_x_group()`:

- one small table per question, percentages on the left, matching counts on the
  right
- disaggregation groups ordered by sample size, largest first
- one sheet per `sector`
- a `readme` sheet recording the dataset, the List of Analysis, the
  disaggregations and every setting that was applied

### Number formats

**Percentages are shown to the nearest whole number**, so
`66.8039538714992` prints as `67%`. That is a *display* format, not a rounded
value — the cell still holds every digit. It matters: a column of whole-number
percentages continues to sum and average correctly, and anyone who needs the
precision can widen the format in Excel without re-running anything.

Means and medians keep two decimals; counts are whole numbers.

To publish decimals instead, change `percent_digits` in `ak_export_settings()`
(`R/export_results.R`) — `1` gives `66.8%`.

Named `analysiskit_<dataset>_<YYYYmmdd-HHMMSS>.xlsx`. An existing file is never
overwritten — a second run in the same second gets a `_1` suffix.

A **Download** button is offered as well as the saved copy.

---

## Deploying to Posit Cloud

### Generate the manifest

Posit Cloud and Posit Connect need a `manifest.json` describing the app's files
and package versions. From the project root:

```r
source("R/generate_manifest.R")
```

This must be run **on the machine that has your real library** — the manifest
records your R version and your installed package versions, so one generated
elsewhere will not reproduce. Regenerate it whenever you add a package or change
which files the app needs, and commit the result.

### If it fails with a missing package

`rsconnect::writeManifest()` snapshots with renv, and **renv refuses to snapshot
a library whose packages have missing dependencies**. Its own message names one
package at a time; `generate_manifest.R` runs a pre-flight first and reports the
whole list with the commands that fix it.

The usual cause is the survey chain. The analysis pipeline calls `srvyr::`,
`analysistools::` and `cleaningtools::` **by name**, so renv records all three —
and their dependencies — even though a run only reaches them when a workbook asks
for a confidence level. Deploying pulls them in whether or not you use them.

If you hit `RcppArmadillo [required by survey]`:

```r
install.packages(c("RcppArmadillo", "srvyr"))
remotes::install_github("impact-initiatives/analysistools")
remotes::install_github("impact-initiatives/cleaningtools")
```

then run `source("R/generate_manifest.R")` again.

### What gets uploaded

`generate_manifest.R` bundles a **named list of files**, not the project
directory. That is deliberate: the repository root holds `.RData` and
`.Rhistory` — an R session's saved workspace and command history. If a dataset
was loaded when that workspace was written, `writeManifest(appDir = ".")` would
upload respondent data to a hosted server. `.rscignore` covers the same ground
for anyone deploying with the RStudio button instead.

`tests/` is left out too, so a deployment does not install a test framework to
run nothing.

### What changes when it is served

**The folder picker is gone, and that is deliberate — not a feature that failed
to load.** `shinyFiles` browses the filesystem of whatever machine the app is
running on. Served, that is the server's container, not your computer: a file
written there never reaches your disk and disappears when the container
recycles. Offering the picker anyway would lose runs silently.

So on a server the app builds the workbook in a per-session temporary folder and
hands it to you as a **download** instead. The fourth step is renamed
**Delivery**, marks itself complete, and says where the Download button will
appear — because a step headed "Output folder" with nothing under it reads as
broken. Run locally, the picker and the **Destination** step are unchanged.

The app detects which case it is in from the environment. If it guesses wrong for
a host it does not recognise, tell it:

```r
options(analysiskit.server = TRUE)   # or the ANALYSISKIT_SERVER env var
```

---

## Repository layout

```
app.R                     UI assembly and reactive wiring, nothing else
R/
  setup_packages.R        what the app needs, and the startup check
  deployment.R            local or served, and what that changes
  read_dataset.R          dataset ingestion and profiling
  read_loa.R              the List of Analysis reader, validator and spec
  ui_components.R         the rules behind the interface, as pure functions
  export_results.R        filenames, export settings, writing the workbook
  generate_manifest.R     deployment tooling (not bundled with the app)
functions/
  ck_analysis_ona_*.R     the analysis pipeline
  format_my_xlsx_*_ordered_n.R   the MMC export formatter
www/                      stylesheet and logo
docs/
  loa-schema.md           the full workbook specification
  loa_template.xlsx       a filled-in template to start from
tests/testthat/           the test suite
```

Everything in `R/` is pure and Shiny-free, which is what makes it testable
without starting a session. `app.R` assembles the interface and connects
reactives; it holds no analysis and no validation.

The analysis and export functions are sourced from `functions/` at startup, so
they can be replaced without touching the app.

**One caution about replacing the formatter.** Every `.R` file in `functions/`
is sourced, so two files defining `format_my_xlsx_variable_x_group()` do not
error — `source()` order silently decides which one survives, and the older
definition can win. The workbook still builds, just without the newer
arguments. When you drop in a new build, retire the old file rather than
leaving it beside the new one. A test enforces this, and every run prints the
build that is actually loaded as the first line of the formatter's log:

```
format_my_xlsx_variable_x_group [simplified build: n-ordering, empty-group drop, NaN blanking, percent_digits]
```

---

## Running the tests

From the project root:

```r
source("tests/testthat.R")
```

Needs `testthat`, `writexl`, `withr` in addition to the app's own packages. The
suite sources everything in `R/` and `functions/`, and covers the workbook
reader and validator, the variable checks, the interface rules, the export, the
deployment behaviour and the app wiring itself through `shiny::testServer()`.

Two tests skip unless the survey packages are installed. None of them write
anything outside a temporary folder, and none install packages.

---

## Known limits

- Nothing is written to `outputs/`; the destination is always a folder you pick
  or a download.
- The Results tab previews the first 12 columns of the wide table only.
- Export settings — the blocks layout, one sheet per `sector` — are fixed house
  style. If they need to vary per workbook, an `export` sheet is the natural
  place; see the open questions in
  [docs/loa-schema.md](docs/loa-schema.md).
- No `renv` lockfile for local development. `manifest.json` covers deployment.
