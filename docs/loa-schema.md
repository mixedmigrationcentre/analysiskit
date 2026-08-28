# List of Analysis (LoA) workbook schema

**Status:** implemented in `R/read_loa.R` and wired into `app.R`, tested in
`tests/testthat/`.

**Purpose.** Move every argument of `run_group_analysis_pipeline()` out of hand-edited R
and into the uploaded List of Analysis workbook, so that a run is fully described by two
files: the dataset and the LoA.

---

## 1. Overview

The LoA is an `.xlsx` workbook of up to six sheets. Only `analysis` is required.

| Sheet | Required | Drives |
|---|---|---|
| `analysis` | yes | the `loa` argument |
| `group_analysis` | no | `group_variables`, plus a variable-renaming map |
| `count_selections` | no | `count_selections` |
| `count_combinations` | no | `count_combinations` |
| `exclude_choices` | no | `exclude_choices` |
| `settings` | no | the remaining scalar arguments |

A missing or empty optional sheet means "not requested": the reader passes the
pipeline's own default (usually `NULL`), never a guessed value.

**CSV uploads carry no sheets.** A `.csv` LoA is read as the `analysis` sheet alone; the
other five are unavailable and their defaults apply. Configured runs therefore require
`.xlsx`. The app should say so rather than silently running an under-configured analysis.

Sheet names are matched case-insensitively after trimming and after collapsing spaces and
hyphens to underscores, so `Group Analysis` and `group_analysis` both resolve. An
unrecognised sheet name is a fatal error, not a silent skip — a typo in a sheet name would
otherwise look exactly like "I chose not to configure that".

Two exceptions are ignored rather than rejected, so a workbook can carry its own notes:
a sheet named `readme` or `notes`, and any sheet whose name begins with `_`.

---

## 2. Sheet `analysis`

The existing LoA table, unchanged. This is the sheet the current pipeline already consumes.

**Required columns**

| Column | Notes |
|---|---|
| `analysis_type` | one of `prop_select_one`, `prop_select_multiple`, `mean`, `median`, `ratio` |
| `analysis_var` | dataset variable, or the select_multiple parent |

**Optional columns**

| Column | Notes |
|---|---|
| `group_var` | per-row grouping; normally left empty, since `group_analysis` handles disaggregation |
| `level` | empty = no confidence interval (fast engine). `0.95` or `95` = interval (survey engine) |
| `analysis_var_numerator` | required when `analysis_type` is `ratio` |
| `analysis_var_denominator` | required when `analysis_type` is `ratio` |
| `numerator_NA_to_0` | ratio only |
| `filter_denominator_0` | ratio only |
| *(any other column)* | metadata such as `sector` or `indicator`, carried to the output by naming it in `settings/extra_columns` |

**`count_select_multiple` and `combination_select_multiple` are not valid `analysis_type`
values here.** They are produced by the pipeline from the `count_selections` and
`count_combinations` sheets and appear only in the output. Writing either into the
`analysis` sheet aborts the run (`run_group_analysis_pipeline()` rejects unknown types
outright, because `create_analysis()` would otherwise drop them silently).

---

## 3. Sheet `group_analysis`

Declares the disaggregations, and renames the raw dataset columns into names that read
properly in the output.

| Column | Required | Notes |
|---|---|---|
| `raw_data_name` | yes | the column as it appears in the dataset, e.g. `Q27` |
| `new_name` | yes | the name to use in the output, e.g. `Respondent_Gender` |
| `include` | no | `TRUE`/`FALSE`; default `TRUE`. Lets a row be parked without deleting it |

Example:

| raw_data_name | new_name | include |
|---|---|---|
| Q27 | Respondent_Gender | TRUE |
| Q42 | Region_of_interview | TRUE |

**`Overall` is implicit.** The reader always prepends `"Overall"` to `group_variables`, so
it must not appear in the sheet. Row order sets the order of the column blocks in the
output, after `Overall`.

### 3.1 What the rename touches

The mapping is applied *before* the pipeline runs, to everything that names a dataset
variable:

- the dataset column itself;
- **every select_multiple child column** sharing the `<raw_data_name><sm_separator>` prefix.
  Renaming `Q78` to `Reasons` without also rewriting `Q78/Economic reasons` to
  `Reasons/Economic reasons` would sever the parent–child link and silently empty the
  analysis. The reader renames both;
- the label row, since it travels as row 1 of the same data frame and its names are the
  lookup keys for `ck_build_label_lookup()`;
- in the `analysis` sheet: `analysis_var`, `analysis_var_numerator`,
  `analysis_var_denominator`, `group_var`;
- in the `count_selections` and `count_combinations` sheets: `analysis_var`;
- in `settings`: `weight_column`, `strata_column`.

So you write raw codes (`Q27`, `Q78`) everywhere in the workbook, and the new name appears
only in the output. This is the decision recorded on 2026-08-28: one mapping, applied
globally, rather than requiring the LoA to use post-rename names.

### 3.2 Validation

Fatal:

- `raw_data_name` or `new_name` blank
- duplicate `raw_data_name`, or duplicate `new_name`
- `new_name` equal to `Overall` (reserved)
- `new_name` already present in the dataset as a different column (would collide)
- **one `new_name` contained in another.** `ck_pivot_variable_x_group()` names output
  columns `stat_<group>_<value>`, and the `column_map` then assigns ownership with
  `grepl(paste0("_", g, "_"), fixed = TRUE)`, first match wins. With both `Region` and
  `Region_of_origin` in play, `stat_Region_of_origin_East Africa` matches `_Region_` first
  and every `Region_of_origin` column is attributed to `Region`. The output table is fine;
  the `column_map` is wrong.

  Tested symmetrically, so the pair is rejected in either row order. The pipeline breaks
  ties by the order the grouping variables are given — which is the order of the rows in
  this sheet — so a pair that happens to work today would break the moment someone
  reorders two rows. Containment anywhere counts, not just a prefix: `of` and
  `Region_of_origin` collide too.

- **a group level that lands in the wrong block.** The same matcher reads the *value*
  half of the column name. A grouping variable with a level literally called `Overall`
  produces `stat_<group>_Overall`, which matches the `_Overall` tag first and is
  attributed to the Overall block. Checked against the real levels in the uploaded
  dataset, with the label row excluded, by reproducing the pipeline's own
  first-match-wins loop (`loa_column_owner()`) rather than approximating it.

Warning, run continues:

- `raw_data_name` not found in the dataset — the row is dropped. This matches the
  pipeline, which already warns and drops absent grouping variables. The result is less
  output, not wrong output.

---

## 4. Sheet `count_selections`

One row per select_multiple parent for which to report *how many* choices each respondent
selected.

| Column | Required | Notes |
|---|---|---|
| `analysis_var` | yes | the select_multiple parent, e.g. `Q78` |
| `include` | no | `TRUE`/`FALSE`; default `TRUE` |

The three category labels, the grouped/exact mode, the row order and the heading are
run-wide and live in `settings`, matching the pipeline, which takes them as scalars rather
than per question.

**Validation** is delegated to `ck_check_count_selections()`, which already aborts with a
usable message when a variable is declared in the LoA as a single select, or has no
`<var><sm_separator>...` child columns in the export. The reader adds only: blank
`analysis_var` is fatal, and duplicates are de-duplicated with a warning.

---

## 5. Sheet `count_combinations`

Long format — one row per choice. The reader groups by `analysis_var` and builds the
named list the pipeline expects.

| Column | Required | Notes |
|---|---|---|
| `analysis_var` | yes | the select_multiple parent |
| `choice_label` | yes | the choice label **exactly as it appears in the export**, punctuation included |
| `display_name` | no | short row label. Blank falls back to the full `choice_label` |
| `include` | no | `TRUE`/`FALSE`; default `TRUE` |

Example:

| analysis_var | choice_label | display_name |
|---|---|---|
| Q78 | Economic reasons | Economic |
| Q78 | Armed conflict, generalised violence, and insecurity | Conflict |

builds

```r
count_combinations = list(
  Q78 = c(
    Economic = "Economic reasons",
    Conflict = "Armed conflict, generalised violence, and insecurity"
  )
)
```

Row order within a question sets the bit order used to build the combination labels.

**Validation** is delegated to `ck_check_choice_combinations()`, which already covers the
hard parts: a choice label that matches no child column (with a "did you mean" suggestion),
more than `max_combination_choices` choices for one question, two choices sharing a display
name, a choice that also appears in `exclude_choices`, and a question that is not a
select_multiple. The reader adds only: blank `analysis_var` or `choice_label` is fatal.

---

## 6. Sheet `exclude_choices`

| Column | Required | Notes |
|---|---|---|
| `choice_label` | yes | e.g. `Don't know`, `Refused` |
| `include` | no | `TRUE`/`FALSE`; default `TRUE` |

**Why this is a sheet and not a `settings` key.** 4Mi choice labels contain commas —
`Armed conflict, generalised violence, and insecurity` — so a comma-separated list in a
single cell would split a label into three and silently fail to match anything. One label
per row removes the delimiter problem entirely.

Note what this argument does: it changes the **denominator**. A respondent who picked an
excluded choice leaves the base for that question altogether; the rows are not merely
hidden. Grouping variables are untouched.

`ck_exclude_choices()` warns when a label matches nothing. Kept as a warning, since the
run is still valid — but the warning should reach the app's UI, as it usually means a
typo in an apostrophe or a comma.

---

## 7. Sheet `settings`

Two columns, `setting` and `value`. One row per setting. Order is irrelevant.

**Unknown keys are fatal.** A misspelled key that was silently ignored would look
identical to a setting that had been applied, which is the failure mode this sheet exists
to prevent.

Blank `value` means "not set" and the default applies.

### 7.1 Allowed keys

Types: `chr` single string · `chr[]` comma-separated list of R names (safe: these are
column names, never free text) · `lgl` `TRUE`/`FALSE` · `num` number · `enum` one of a
fixed set.

**Data and design**

| setting | type | default | notes |
|---|---|---|---|
| `sm_separator` | chr | `/` | ONA style |
| `skip_label_row` | lgl | `TRUE` | row 1 of the export is the label row |
| `blank_to_na` | lgl | `TRUE` | `""` treated as missing |
| `prepare_sm` | lgl | `TRUE` | select_multiple children to 0/1 with a not-asked mask |
| `sm_child_style` | enum | `auto` | `auto` \| `label` \| `dummy` |
| `recreate_sm_parents` | lgl | `FALSE` | needs the `cleaningtools` package **and a column literally named `uuid`** — the pipeline hardcodes that name |
| `weight_column` | chr | *(none)* | |
| `strata_column` | chr | *(none)* | affects the variance only, so it changes nothing under `engine = fast` |

**Estimation**

| setting | type | default | notes |
|---|---|---|---|
| `engine` | enum | `auto` | `auto` \| `fast` \| `survey`. `auto` routes rows with a `level` to survey, the rest to fast tabulation |
| `fallback_level` | num | `0.95` | placeholder for rows with an empty `level`; the resulting interval is blanked |
| `min_group_n` | num | *(none)* | group levels below this are set aside |
| `keep_missing_groups` | lgl | `TRUE` | report respondents missing on the grouping variable as their own group |
| `slim_design` | lgl | `TRUE` | hand the survey design only the columns it needs |
| `lonely_psu` | chr | `adjust` | `survey.lonely.psu` option |

**Denominator**

| setting | type | default | notes |
|---|---|---|---|
| `exclude_ignore_case` | lgl | `TRUE` | applies to the `exclude_choices` sheet |

**Output shape**

| setting | type | default | notes |
|---|---|---|---|
| `value_columns` | chr[] | `stat,n,n_total` | add `n_w,n_w_total` when weighting; `stat_low,stat_upp` for intervals |
| `extra_columns` | chr[] | *(none)* | `analysis`-sheet columns to carry into the output, e.g. `sector` |
| `missing_group_label` | chr | `Missing` | column label for respondents missing on a grouping variable |
| `use_group_prefix` | lgl | `TRUE` | **leave `TRUE`.** `FALSE` gives `stat_<value>`, which collides when two grouping variables share a value |
| `summary_value_label` | chr | *(none)* | written into `analysis_var_value` for mean/median/ratio rows |
| `drop_empty_prop_rows` | lgl | `TRUE` | drop the `NA`-valued placeholder row proportions produce |
| `add_analysis_type_label` | lgl | `TRUE` | add `label_analysis_type` |
| `label_choices` | lgl | `TRUE` | relabel select_multiple choice values |

**Selection counts**

| setting | type | default |
|---|---|---|
| `count_selections_mode` | enum | `grouped` (\| `exact`) |
| `count_selections_order` | enum | `descending` (\| `ascending`) |
| `count_selections_heading` | chr | `Select multiple count` |
| `count_selections_spacer` | lgl | `TRUE` |
| `count_selections_title_suffix` | chr | *(empty)* |
| `count_selections_label_none` | chr | `No choice selected` |
| `count_selections_label_one` | chr | `Selected exactly 1 choice` |
| `count_selections_label_many` | chr | `Selected more than 1 choice` |

The three labels are separate keys rather than one comma-separated cell for the same
reason as `exclude_choices`. The reader assembles them as
`c(none, one, many)` — the order the pipeline requires, regardless of
`count_selections_order`, which only controls display.

**Choice combinations**

| setting | type | default |
|---|---|---|
| `count_combinations_order` | enum | `descending` (\| `ascending`) |
| `count_combinations_none_label` | chr | `None of these` |
| `count_combinations_joiner` | chr | ` + ` |
| `count_combinations_heading` | chr | `Choice combination` |
| `count_combinations_spacer` | lgl | `TRUE` |
| `count_combinations_title_suffix` | chr | *(empty)* |
| `count_combinations_ignore_case` | lgl | `TRUE` |
| `max_combination_choices` | num | `6` |

### 7.2 Deliberately not settable

- `verbose` — owned by the app, which routes pipeline messages to the progress UI.
- `label_row` — an R data frame, not expressible in a cell.
- `analysis_type_labels` — an override table. Could become a seventh sheet if MMC needs
  labels other than the defaults; not specified until then.
- `dataset`, `loa`, `group_variables`, `count_selections`, `count_combinations`,
  `exclude_choices` — supplied by the dataset upload and the other sheets.

---

## 8. Internal representation

The reader produces one object; nothing downstream re-reads the workbook.

```r
analysis_spec <- list(
  loa                = <data.frame>,   # analysis sheet, post-rename
  group_variables    = <character>,    # c("Overall", ...)
  rename_map         = <named chr>,    # names are raw_data_name, values new_name
  count_selections   = <character>,
  count_combinations = <named list of named chr>,
  exclude_choices    = <character>,
  settings           = <named list>,   # validated, typed, only what was supplied
  problems           = <data.frame>,   # sheet, row, severity, message
  source             = <list>          # filename, format
)
```

Everything in the spec is in **post-rename** names, matching the dataset
`run_analysis_spec()` hands to the pipeline. The workbook is written in raw codes
throughout; this is where that is resolved.

`problems` carries both severities. `severity == "error"` anywhere means the run is
blocked and the app renders the list; `"warning"` rows are shown but do not block.
Neither `validate_loa()` nor `build_analysis_spec()` ever stops — only
`run_analysis_spec()` does, and only on an error.

### 8.1 Functions — `R/read_loa.R`

| Function | Responsibility |
|---|---|
| `read_loa_workbook(path)` | read every recognised sheet; no interpretation |
| `validate_loa(workbook, dataset)` | every check in this document; returns `problems` |
| `loa_parse_settings(sheet)` | coerce and allow-list the `settings` sheet |
| `build_analysis_spec(workbook, dataset)` | apply the rename map and assemble `analysis_spec` |
| `apply_rename_map(dataset, map, sm_separator)` | rename a dataset — parents *and* select_multiple children |
| `loa_column_owner(column, group_variables)` | reproduce the pipeline's column-ownership rule |
| `analysis_spec_args(dataset, spec)` | build the pipeline argument list, without running it |
| `run_analysis_spec(dataset, spec, pipeline)` | `do.call()` — the only place the two meet |
| `loa_has_errors(problems)` | is the run blocked |

All pure and testable without a Shiny session. `run_analysis_spec()` takes the pipeline
as an argument so the assembly can be tested without the analysis code loaded, and
accepts `...` for run-time concerns the workbook should not own, such as `verbose`.

Deep checks are **delegated, not duplicated**: when `ck_check_count_selections()` and
`ck_check_choice_combinations()` are on the search path they are called, and their
condition is converted into a problem row. Their messages are good, and a second
implementation would drift from them. Delegation is skipped while the workbook still has
fatal problems of its own, since those validators expect a coherent input.

### 8.2 Tests

`tests/testthat/` — run with `source("tests/testthat.R")` from the project root. The
suite sources `R/` and, when it holds anything, `functions/`.

`test-read_loa-pipeline.R` carries the contract with the analysis pipeline. Two of its
tests skip while `functions/` is empty and start running the moment the analysis file
lands there: one asserts the transcribed signature still matches the real one, the other
runs a workbook end to end and checks that every `column_map` row agrees with
`loa_column_owner()`. The other two run now — one builds a workbook that sets *every*
allow-listed setting and binds it against a stub with the pipeline's exact 46-argument
signature, which is what catches a setting that assembles into an argument the pipeline
does not have.

---

## 9. Validation principle

> **Fatal** when the run would produce *wrong or misleading* output.
> **Warning** when it would produce *less* output.

That is why a missing grouping variable warns (one fewer column block) while a prefix
collision between two `new_name` values is fatal (a `column_map` that mislabels which
disaggregation a column belongs to).

Where the pipeline already validates something — `ck_check_count_selections()`,
`ck_check_choice_combinations()`, the `analysis_type` allow-list — the reader does not
duplicate the check. Those messages are good, and a second implementation would drift.

---

## 10. Variable coverage

`loa_variable_coverage(workbook, dataset)` is the check that runs the moment both files
are in the app. It returns one row per *reference*: every dataset variable the workbook
names, where it is named, and whether the uploaded dataset has it.

| role | named in |
|---|---|
| `analysis_var` | `analysis` sheet |
| `ratio_numerator`, `ratio_denominator` | `analysis` sheet, ratio rows |
| `group_var` | `group_analysis` sheet, and any per-row `group_var` in `analysis` |
| `count_selections` | `count_selections` sheet |
| `count_combinations` | `count_combinations` sheet |
| `weight_column`, `strata_column` | `settings` sheet |

Presence uses the same rule as the pipeline (`loa_var_present()`), so a select_multiple
parent counts as present when its child columns are there even though ONA did not export
the concatenated parent. Names are matched **as written in the workbook** — raw dataset
codes — against the raw dataset, before any renaming.

Severity follows what actually happens next, which is why it is not uniform:

- `count_selections` and `count_combinations` are **fatal**. `ck_check_count_selections()`
  and `ck_check_choice_combinations()` stop the run outright, so warning here and aborting
  later would be worse than saying so up front.
- everything else **warns**: the pipeline drops the row and produces less output.
- *nothing* matching is **fatal** on its own — `ck_stack_loa()` stops with "No runnable
  analyses", and the realistic cause is last round's List of Analysis against this round's
  export, which deserves one clear message rather than a warning per row.

`loa_coverage_summary()` collapses this to one row per variable for display; the app shows
it under **Variable coverage** on the List of Analysis tab.

## 11. The interface

`app.R` assembles the UI and connects the reactives; nothing else. The rules behind the
interface live in `R/ui_components.R` as pure functions, so they are tested without a
Shiny session.

- A four-step tracker — Dataset, List of Analysis, Checks, Results — is always on screen.
  `ak_step_states()` derives the state of each step; `ak_can_run()` decides whether the
  run is offered. State is carried by shape *and* colour, so it survives a colour-blind
  reader or a greyscale print.
- **Validation is reactive; analysis is not.** Every check reruns the moment either file
  changes, so uploading a different dataset re-checks the same workbook against it. The
  analysis itself runs only from the button, through `observeEvent()`.
- Progress is reported where work actually happens: `withProgress()` around each file
  read, and around the run, where the pipeline's own `message()` output is captured and
  routed to the progress bar so a long run reads as progress rather than a frozen screen.
  Validation deliberately has no progress bar — it takes milliseconds and feeds several
  outputs, so a bar would only flash.
- The Run button is disabled unless both files are in, no check is fatal, and the pipeline
  is actually available. The observer still refuses the call independently, so the button
  is a convenience rather than the guard.

## 12. Output

A completed run is written straight to a folder the analyst chooses, as one branded
`.xlsx` built by `format_my_xlsx_variable_x_group()` — the same kind of file as
`4Mi_results_QN6.xlsx`.

**The folder is chosen before the run, not after.** `shinyFiles::shinyDirButton()` browses
the filesystem the app is running on, which is the analyst's own machine (see §11). It is
its own step in the tracker, and `ak_check_folder()` confirms the folder exists and is
writable *before* the analysis starts, so a long run is never lost to a read-only
destination. Nothing is written unless the run completes without error; if the analysis
succeeds and only the save fails, the results are kept and the interface says the save is
what went wrong.

Served from a remote host this would browse the *server's* disk, which is almost never
what anyone wants — a download handler would be the right answer there instead.

**Export settings** (`ak_export_settings()`) are derived from the run rather than
hard-coded, and reproduce the example output with the default workbook:

| Argument | Value | Where it comes from |
|---|---|---|
| `layout` | `blocks` | house style |
| `value_columns` | `stat` | `settings/value_columns`, minus the count columns |
| `total_columns` | `n`, `n_total` | the count columns of `settings/value_columns` |
| `split_by` | `sector` | that column when the table has one, else the first carried metadata column, else `none` |

Splitting one list guarantees `length(c(value_columns, total_columns))` matches the number
of columns the pipeline wrote per group block; get that wrong and the formatter cuts the
blocks in the wrong places.

**Filenames** are `analysiskit_<dataset stem>_<YYYYmmdd-HHMMSS>.xlsx`, and an existing file
is never overwritten — a second run in the same second gets a `_1` suffix. The workbook's
readme sheet carries the provenance: dataset, List of Analysis, disaggregations and every
setting that was applied.

**One adaptation is needed before formatting.** `ck_insert_count_separators()` appends a
`row_type` column at the end of the wide table and inserts spacer and heading rows above
each derived block. Handed to the formatter unchanged, the trailing `row_type` is read as
a statistic column and dropped with a warning, and the marker rows — which carry no
`sector` — are split onto a sheet of their own called "not specified".
`ak_prepare_for_export()` handles both: in the blocks layout, where every question already
gets its own titled table, the markers are redundant and are removed; in the matrix layout
they are kept and `row_type` is moved in among the identifier columns.

## 13. Not yet done

- Nothing is written to `outputs/`, and there is no download handler — the destination is
  always a folder the user picks. A served deployment would need one.
- The Results tab previews the first 12 columns of the wide table only.
- The survey engine (`level` set on an analysis row) needs `srvyr` and
  `analysistools`, which are not installed by default. The fast engine — every row with an
  empty `level` — needs neither.
- The export side (`format_my_xlsx_variable_x_group()`) has its own arguments —
  `layout`, `split_by`, `total_columns`, `hidden_columns` — which this schema does not
  cover. They may deserve an `export` sheet, or may be fixed house style. Decide with
  question 3 below.
- `app.R` sources everything in `functions/` at startup, so the analysis and export
  functions can be replaced without touching the app.
- No `renv`, and no deployment documentation.

## 14. Open questions

1. Should the app let a user override workbook settings in the UI before running, or is
   the workbook the single source of truth for a run?
2. Should `analysis_type_labels` become a seventh sheet, or are the seven built-in labels
   fixed for MMC?
3. Is there an existing MMC output template the wide table must conform to? That decides
   whether `value_columns` and the heading/spacer settings are user choices or fixed
   house style.
