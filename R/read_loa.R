# =============================================================================
# List of Analysis (LoA) workbook: reader, validator and analysis specification
# =============================================================================
#
# Turns the multi-sheet LoA workbook documented in docs/loa-schema.md into one
# analysis_spec object, which run_analysis_spec() hands to
# run_group_analysis_pipeline().
#
# Responsibility split, deliberately kept narrow:
#   read_loa_workbook()   reads sheets. No interpretation, no judgement.
#   validate_loa()        every check. Returns problems, never stops.
#   build_analysis_spec() applies the rename map and assembles the spec.
#   run_analysis_spec()   the only place the spec meets the pipeline.
#
# Nothing here depends on Shiny, and nothing here depends on the pipeline being
# loaded: the ck_* validators are called only when they are on the search path.
# =============================================================================


# -----------------------------------------------------------------------------
# 0. Small helpers
# -----------------------------------------------------------------------------

#' Trim Leading and Trailing Whitespace
#'
#' Trims only. Internal whitespace is never collapsed, because 4Mi choice
#' labels must match the export character for character and squishing them
#' would break a label that legitimately contains a double space.
#'
#' @param x A vector.
#' @return A character vector with `""` and the literal `"NA"` as `NA`.
#' @keywords internal
loa_trim <- function(x) {
  x <- trimws(as.character(x))
  x[!is.na(x) & (x == "" | x == "NA")] <- NA_character_
  x
}


#' Is Every Cell of This Row Empty
#' @param df A data frame.
#' @return A logical vector, one element per row.
#' @keywords internal
loa_blank_rows <- function(df) {
  if (nrow(df) == 0L) {
    return(logical(0))
  }
  filled <- vapply(df, function(col) !is.na(loa_trim(col)), logical(nrow(df)))
  if (is.null(dim(filled))) filled <- matrix(filled, nrow = nrow(df))
  rowSums(filled) == 0L
}


#' Coerce a Spreadsheet Cell to Logical
#'
#' Accepts what people actually type in Excel. Anything else returns `NA`, and
#' the caller reports it rather than guessing.
#'
#' @param x A vector.
#' @return A logical vector.
#' @keywords internal
loa_as_logical <- function(x) {
  v <- tolower(loa_trim(x))
  out <- rep(NA, length(v))
  out[v %in% c("true", "t", "yes", "y", "1")] <- TRUE
  out[v %in% c("false", "f", "no", "n", "0")] <- FALSE
  out
}


#' Coerce a Spreadsheet Cell to a Number
#' @param x A vector.
#' @return A numeric vector; unparseable values are `NA`.
#' @keywords internal
loa_as_number <- function(x) {
  suppressWarnings(as.numeric(loa_trim(x)))
}


#' Normalise a Sheet Name for Matching
#'
#' Lower case, trimmed, with spaces and hyphens collapsed to underscores, so
#' `"Group Analysis"` and `"group-analysis"` both resolve to `group_analysis`.
#'
#' @param x Character vector of sheet names.
#' @return A character vector.
#' @keywords internal
loa_normalise_sheet_name <- function(x) {
  x <- tolower(trimws(as.character(x)))
  gsub("[[:space:]-]+", "_", x)
}


#' An Empty Problems Table
#' @return A zero-row data frame with the problems columns.
#' @keywords internal
loa_no_problems <- function() {
  data.frame(
    sheet = character(0),
    row = integer(0),
    severity = character(0),
    message = character(0),
    stringsAsFactors = FALSE
  )
}


#' Build a Problems Table
#'
#' @param sheet Sheet the problem belongs to.
#' @param row Workbook row number (header is row 1), or `NA` for a
#'   sheet-level problem.
#' @param severity `"error"` or `"warning"`.
#' @param message The message shown to the user.
#' @return A data frame of problems.
#' @keywords internal
loa_problem <- function(sheet, row, severity, message) {
  data.frame(
    sheet = sheet,
    row = as.integer(row),
    severity = severity,
    message = message,
    stringsAsFactors = FALSE
  )
}


#' Does a Problems Table Contain a Fatal Problem
#' @param problems A problems data frame.
#' @return `TRUE` when at least one row has severity `"error"`.
#' @export
loa_has_errors <- function(problems) {
  is.data.frame(problems) &&
    nrow(problems) > 0 &&
    any(problems$severity == "error")
}


# -----------------------------------------------------------------------------
# 1. The sheet and settings contracts
# -----------------------------------------------------------------------------

#' Sheets the Reader Recognises
#' @return A character vector of normalised sheet names.
#' @export
loa_known_sheets <- function() {
  c(
    "analysis", "group_analysis", "count_selections",
    "count_combinations", "exclude_choices", "settings"
  )
}


#' Sheets Ignored Rather Than Rejected
#'
#' A workbook is allowed to carry its own notes. Anything else unrecognised is
#' a fatal error: a typo in a sheet name would otherwise be indistinguishable
#' from a deliberate decision not to configure that sheet.
#'
#' @param x Normalised sheet names.
#' @return A logical vector.
#' @keywords internal
loa_is_ignored_sheet <- function(x) {
  x %in% c("readme", "notes") | startsWith(x, "_")
}


#' `analysis_type` Values the Pipeline Accepts
#' @return A character vector.
#' @export
loa_known_analysis_types <- function() {
  c("prop_select_one", "prop_select_multiple", "mean", "median", "ratio")
}


#' Analysis Types the Pipeline Produces Rather Than Accepts
#'
#' These appear in the output, built from the `count_selections` and
#' `count_combinations` sheets. Writing one into the `analysis` sheet aborts
#' the run, so it earns its own message.
#'
#' @return A character vector.
#' @keywords internal
loa_derived_analysis_types <- function() {
  c("count_select_multiple", "combination_select_multiple")
}


#' The `settings` Sheet Allow-List
#'
#' One row per accepted key. `arg` is the `run_group_analysis_pipeline()`
#' argument it sets; `type` decides the coercion; `values` restricts an enum.
#' A key absent from this table is a fatal error.
#'
#' Defaults deliberately live in the pipeline, not here: a blank or absent
#' setting is simply not passed, so there is one definition of every default.
#'
#' @return A data frame with `setting`, `arg`, `type` and `values`.
#' @export
loa_settings_schema <- function() {
  s <- function(setting, type, values = NA_character_, arg = setting) {
    data.frame(
      setting = setting, arg = arg, type = type, values = values,
      stringsAsFactors = FALSE
    )
  }

  rbind(
    # data and design
    s("sm_separator", "chr"),
    s("skip_label_row", "lgl"),
    s("blank_to_na", "lgl"),
    s("prepare_sm", "lgl"),
    s("sm_child_style", "enum", "auto,label,dummy"),
    s("recreate_sm_parents", "lgl"),
    s("weight_column", "chr"),
    s("strata_column", "chr"),

    # estimation
    s("engine", "enum", "auto,fast,survey"),
    s("fallback_level", "num"),
    s("min_group_n", "num"),
    s("keep_missing_groups", "lgl"),
    s("slim_design", "lgl"),
    s("lonely_psu", "chr"),

    # denominator
    s("exclude_ignore_case", "lgl"),

    # output shape
    s("value_columns", "chr[]"),
    s("extra_columns", "chr[]"),
    s("missing_group_label", "chr"),
    s("use_group_prefix", "lgl"),
    s("summary_value_label", "chr"),
    s("drop_empty_prop_rows", "lgl"),
    s("add_analysis_type_label", "lgl"),
    s("label_choices", "lgl"),

    # selection counts
    s("count_selections_mode", "enum", "grouped,exact"),
    s("count_selections_order", "enum", "descending,ascending"),
    s("count_selections_heading", "chr"),
    s("count_selections_spacer", "lgl"),
    s("count_selections_title_suffix", "chr"),
    s("count_selections_label_none", "chr", arg = "count_selections_labels"),
    s("count_selections_label_one", "chr", arg = "count_selections_labels"),
    s("count_selections_label_many", "chr", arg = "count_selections_labels"),

    # choice combinations
    s("count_combinations_order", "enum", "descending,ascending"),
    s("count_combinations_none_label", "chr"),
    s("count_combinations_joiner", "chr"),
    s("count_combinations_heading", "chr"),
    s("count_combinations_spacer", "lgl"),
    s("count_combinations_title_suffix", "chr"),
    s("count_combinations_ignore_case", "lgl"),
    s("max_combination_choices", "num")
  )
}


#' The Pipeline's Own Selection-Count Labels
#'
#' `count_selections_labels` is a length-three vector, so a workbook that sets
#' only one of the three still needs the other two. These are the pipeline's
#' defaults, and the only place in this file where a pipeline default is
#' restated.
#'
#' @return A character vector of length three: none, exactly one, more than one.
#' @keywords internal
loa_default_selection_labels <- function() {
  c(
    "No choice selected",
    "Selected exactly 1 choice",
    "Selected more than 1 choice"
  )
}


# -----------------------------------------------------------------------------
# 2. Reading
# -----------------------------------------------------------------------------

#' Read a List of Analysis Workbook
#'
#' Reads every recognised sheet and interprets nothing. Unrecognised sheet
#' names are recorded rather than rejected here, so that
#' \code{\link{validate_loa}} remains the single place that decides what is a
#' problem.
#'
#' Configuration sheets are read as text so their types are decided by this
#' file rather than by whatever \pkg{readxl} guessed from the first few rows.
#' The `analysis` sheet keeps its guessed types, so a numeric `level` column
#' stays numeric.
#'
#' A `.csv` upload carries no sheets: it is read as the `analysis` table alone,
#' and `format` is `"CSV"` so the caller can say so.
#'
#' @param path Path to a `.xlsx` or `.csv` file.
#' @param filename Optional original filename, for messages. Defaults to
#'   `basename(path)`.
#' @return A list with `sheets` (named list of data frames), `format`,
#'   `filename`, `sheet_names` (as written in the workbook), `unknown_sheets`
#'   and `ignored_sheets`.
#' @export
read_loa_workbook <- function(path, filename = NULL) {
  if (is.null(filename)) filename <- basename(path)
  extension <- tolower(tools::file_ext(filename))

  if (!extension %in% c("csv", "xlsx")) {
    stop(
      "Unsupported List of Analysis file type. Please upload a CSV or XLSX file.",
      call. = FALSE
    )
  }

  if (extension == "csv") {
    analysis <- utils::read.csv(
      path,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      na.strings = c("", "NA", "N/A", "NULL")
    )

    return(list(
      sheets = list(analysis = analysis),
      format = "CSV",
      filename = filename,
      sheet_names = "analysis",
      unknown_sheets = character(0),
      ignored_sheets = character(0)
    ))
  }

  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop(
      "Reading XLSX files requires the 'readxl' package. Install it with install.packages('readxl').",
      call. = FALSE
    )
  }

  sheet_names <- readxl::excel_sheets(path)
  normalised <- loa_normalise_sheet_name(sheet_names)

  ignored <- loa_is_ignored_sheet(normalised)
  known <- normalised %in% loa_known_sheets()
  unknown <- !known & !ignored

  sheets <- list()
  for (i in which(known)) {
    # Configuration sheets are read as text; the analysis table is not, so that
    # `level` arrives numeric.
    col_types <- if (normalised[i] == "analysis") NULL else "text"

    df <- as.data.frame(
      readxl::read_excel(path, sheet = sheet_names[i], col_types = col_types),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    # Excel leaves trailing blank rows behind constantly; they are not data.
    if (nrow(df) > 0) df <- df[!loa_blank_rows(df), , drop = FALSE]
    rownames(df) <- NULL

    sheets[[normalised[i]]] <- df
  }

  list(
    sheets = sheets,
    format = "XLSX",
    filename = filename,
    sheet_names = sheet_names,
    unknown_sheets = sheet_names[unknown],
    ignored_sheets = sheet_names[ignored]
  )
}


# -----------------------------------------------------------------------------
# 3. Settings
# -----------------------------------------------------------------------------

#' Parse and Type-Check the `settings` Sheet
#'
#' Returns only the settings that were actually supplied, so every unset
#' argument keeps the pipeline's own default.
#'
#' @param sheet The `settings` data frame, or `NULL`.
#' @return A list with `settings` (named list, pipeline argument names) and
#'   `problems`.
#' @export
loa_parse_settings <- function(sheet) {
  out <- list(settings = list(), problems = loa_no_problems())

  if (is.null(sheet) || nrow(sheet) == 0) {
    return(out)
  }

  problems <- list()
  add <- function(row, severity, message) {
    problems[[length(problems) + 1]] <<-
      loa_problem("settings", row, severity, message)
  }

  if (!all(c("setting", "value") %in% names(sheet))) {
    add(NA, "error", paste0(
      "The settings sheet must have 'setting' and 'value' columns. Found: ",
      paste(names(sheet), collapse = ", "), "."
    ))
    out$problems <- do.call(rbind, problems)
    return(out)
  }

  schema <- loa_settings_schema()
  key <- loa_trim(sheet$setting)
  value <- loa_trim(sheet$value)

  parsed <- list()
  labels <- stats::setNames(rep(NA_character_, 3), c("none", "one", "many"))

  for (i in seq_along(key)) {
    excel_row <- i + 1L
    k <- key[i]

    if (is.na(k)) {
      add(excel_row, "error", "This row has a value but no setting name.")
      next
    }
    if (anyDuplicated(key[seq_len(i)]) > 0 && k %in% key[seq_len(i - 1L)]) {
      add(excel_row, "error", paste0(
        "'", k, "' is set more than once. Keep one row per setting."
      ))
      next
    }

    hit <- match(k, schema$setting)
    if (is.na(hit)) {
      near <- schema$setting[
        startsWith(schema$setting, substr(k, 1, 6)) |
          startsWith(k, substr(schema$setting, 1, 6))
      ]
      add(excel_row, "error", paste0(
        "'", k, "' is not a recognised setting.",
        if (length(near) > 0) {
          paste0(" Did you mean: ", paste(utils::head(near, 4), collapse = ", "), "?")
        } else {
          " See docs/loa-schema.md for the full list."
        }
      ))
      next
    }

    if (is.na(value[i])) next # blank means "use the pipeline default"

    type <- schema$type[hit]
    arg <- schema$arg[hit]
    v <- value[i]

    converted <- switch(
      type,
      chr = v,
      `chr[]` = {
        parts <- loa_trim(strsplit(v, ",", fixed = TRUE)[[1]])
        parts[!is.na(parts)]
      },
      lgl = loa_as_logical(v),
      num = loa_as_number(v),
      enum = {
        allowed <- strsplit(schema$values[hit], ",", fixed = TRUE)[[1]]
        if (tolower(v) %in% allowed) tolower(v) else NA_character_
      }
    )

    bad <- switch(
      type,
      lgl = is.na(converted),
      num = is.na(converted),
      enum = is.na(converted),
      `chr[]` = length(converted) == 0,
      FALSE
    )

    if (isTRUE(bad)) {
      add(excel_row, "error", switch(
        type,
        lgl = paste0("'", k, "' must be TRUE or FALSE. Found '", v, "'."),
        num = paste0("'", k, "' must be a number. Found '", v, "'."),
        enum = paste0(
          "'", k, "' must be one of ",
          paste(strsplit(schema$values[hit], ",", fixed = TRUE)[[1]], collapse = ", "),
          ". Found '", v, "'."
        ),
        `chr[]` = paste0("'", k, "' is a comma-separated list and cannot be empty.")
      ))
      next
    }

    # The three label keys assemble one length-three argument.
    if (arg == "count_selections_labels") {
      slot <- sub("^count_selections_label_", "", k)
      labels[[slot]] <- converted
    } else {
      parsed[[arg]] <- converted
    }
  }

  if (any(!is.na(labels))) {
    defaults <- loa_default_selection_labels()
    filled <- ifelse(is.na(labels), defaults, labels)
    # Order is fixed by the pipeline: none, exactly one, more than one -
    # whatever count_selections_order does to the display order.
    parsed[["count_selections_labels"]] <- unname(filled)
  }

  out$settings <- parsed
  out$problems <- if (length(problems) > 0) do.call(rbind, problems) else loa_no_problems()
  out
}


# -----------------------------------------------------------------------------
# 4. Renaming
# -----------------------------------------------------------------------------

#' Build the Rename Map from the `group_analysis` Sheet
#'
#' @param sheet The `group_analysis` data frame, or `NULL`.
#' @param dataset Optional dataset, used to drop rows whose `raw_data_name` is
#'   absent.
#' @param sm_separator Select_multiple separator.
#' @return A named character vector: names are `raw_data_name`, values are
#'   `new_name`. Empty when there is nothing to rename.
#' @keywords internal
loa_rename_map <- function(sheet, dataset = NULL, sm_separator = "/") {
  empty <- stats::setNames(character(0), character(0))
  if (is.null(sheet) || nrow(sheet) == 0) return(empty)
  if (!all(c("raw_data_name", "new_name") %in% names(sheet))) return(empty)

  raw <- loa_trim(sheet$raw_data_name)
  new <- loa_trim(sheet$new_name)
  keep <- !is.na(raw) & !is.na(new)

  if ("include" %in% names(sheet)) {
    inc <- loa_as_logical(sheet$include)
    keep <- keep & (is.na(inc) | inc)
  }

  if (!is.null(dataset)) {
    keep <- keep & loa_var_present(raw, dataset, sm_separator)
  }

  keep <- keep & !duplicated(raw) & !duplicated(new)
  if (!any(keep)) return(empty)

  stats::setNames(new[keep], raw[keep])
}


#' Is a Variable Usable in the Dataset
#'
#' A variable counts as present when it is a column, or when it is a
#' select_multiple parent whose child columns are there. ONA exports do not
#' always carry the concatenated parent column, so testing column names alone
#' would declare every select_multiple missing.
#'
#' @param vars Character vector of variable names.
#' @param dataset The dataset.
#' @param sm_separator Select_multiple separator.
#' @return A logical vector the same length as `vars`.
#' @keywords internal
loa_var_present <- function(vars, dataset, sm_separator = "/") {
  nms <- names(dataset)
  vapply(
    vars,
    function(v) {
      if (is.na(v)) return(FALSE)
      v %in% nms || any(startsWith(nms, paste0(v, sm_separator)))
    },
    logical(1),
    USE.NAMES = FALSE
  )
}


#' Apply a Rename Map to a Dataset
#'
#' Renames the column itself **and every select_multiple child** sharing the
#' `<old><sm_separator>` prefix. Renaming `Q78` without rewriting
#' `Q78/Economic reasons` would sever the parent-child link, and every
#' select_multiple analysis of that question would silently return nothing.
#'
#' The whole map is applied in one pass over the original names, so a map that
#' happens to rename `A` to `B` and `B` to `C` cannot chain `A` through to `C`.
#'
#' @param dataset A data frame.
#' @param map A named character vector: names are the current names, values the
#'   new ones.
#' @param sm_separator Select_multiple separator.
#' @return `dataset` with its columns renamed.
#' @export
apply_rename_map <- function(dataset, map, sm_separator = "/") {
  if (length(map) == 0 || ncol(dataset) == 0) {
    return(dataset)
  }

  old_names <- names(dataset)
  new_names <- old_names

  for (raw in names(map)) {
    new <- map[[raw]]

    exact <- old_names == raw
    new_names[exact] <- new

    prefix <- paste0(raw, sm_separator)
    child <- startsWith(old_names, prefix)
    if (any(child)) {
      new_names[child] <- paste0(
        new, sm_separator,
        substr(old_names[child], nchar(prefix) + 1L, nchar(old_names[child]))
      )
    }
  }

  names(dataset) <- new_names
  dataset
}


#' Apply a Rename Map to a Vector of Variable Names
#' @param x Character vector of variable names.
#' @param map A named character vector as in \code{\link{apply_rename_map}}.
#' @return `x` with mapped names replaced.
#' @keywords internal
loa_rename_values <- function(x, map) {
  if (length(map) == 0 || length(x) == 0) return(x)
  hit <- match(as.character(x), names(map))
  ifelse(is.na(hit), as.character(x), unname(map[hit]))
}


# -----------------------------------------------------------------------------
# 5. Output column ownership
# -----------------------------------------------------------------------------

#' Which Grouping Variable the Pipeline Will Attribute a Column To
#'
#' Reproduces the ownership rule in `run_group_analysis_pipeline()`: `Overall`
#' first, then each grouping variable in turn, matching the fixed substring
#' `_<group>_`, first match wins. Reproduced rather than approximated, so this
#' check cannot drift from the behaviour it is guarding.
#'
#' @param column A statistic column name, e.g. `stat_Region_of_origin_esa`.
#' @param group_variables Grouping variables in the order the pipeline sees
#'   them, excluding `Overall`.
#' @return The owning grouping variable, or `NA` when nothing matches.
#' @keywords internal
loa_column_owner <- function(column, group_variables) {
  for (g in c("Overall", setdiff(group_variables, "Overall"))) {
    tag <- if (identical(g, "Overall")) "_Overall" else paste0("_", g, "_")
    if (grepl(tag, column, fixed = TRUE)) {
      return(g)
    }
  }
  NA_character_
}


#' Find Grouping Variables Whose Output Columns Would Be Misattributed
#'
#' The wide table names its columns `stat_<group>_<value>` and the `column_map`
#' then works out which disaggregation each column belongs to by looking for
#' `_<group>_`, first match wins. Two consequences, both silent:
#'
#' \itemize{
#'   \item a grouping variable whose name is contained in another's - `Region`
#'     and `Region_of_origin` - takes ownership of the other's columns;
#'   \item a grouping variable with a level literally called `Overall`, or a
#'     level that completes another variable's tag, lands in the wrong block.
#' }
#'
#' The table itself is still correct; the map that says what the columns mean
#' is not. That is why this is fatal rather than a warning.
#'
#' @param group_variables Grouping variables, excluding `Overall`.
#' @param dataset Optional dataset, used to test the real levels.
#' @param raw_names Optional named vector mapping each grouping variable to the
#'   raw dataset column its levels come from.
#' @param missing_group_label Label used for respondents missing on a grouping
#'   variable.
#' @return A data frame with `group_variable`, `column` and `attributed_to`.
#' @keywords internal
loa_group_tag_clashes <- function(group_variables,
                                  dataset = NULL,
                                  raw_names = NULL,
                                  missing_group_label = "Missing") {
  found <- list()
  note <- function(g, other, example) {
    found[[length(found) + 1]] <<- data.frame(
      group_variable = g, clashes_with = other, example = example,
      stringsAsFactors = FALSE
    )
  }

  # --- name against name -----------------------------------------------------
  # Tested symmetrically rather than through loa_column_owner(), because the
  # pipeline breaks ties by the order the grouping variables are given - which
  # is the order of the rows in the sheet. A pair that happens to work today
  # would break the moment someone reorders two rows, so the pair is rejected
  # either way.
  for (b in group_variables) {
    haystack <- paste0("stat_", b, "_")

    if (grepl("_Overall", haystack, fixed = TRUE)) {
      note(b, "Overall", paste0(haystack, "<value>"))
    }
    for (a in setdiff(group_variables, b)) {
      if (grepl(paste0("_", a, "_"), haystack, fixed = TRUE)) {
        note(b, a, paste0(haystack, "<value>"))
      }
    }
  }

  # --- name against the real levels ------------------------------------------
  # Here the pipeline's own first-match-wins rule is reproduced exactly, since
  # the question is which block a specific column would actually land in.
  if (!is.null(dataset)) {
    for (g in group_variables) {
      source_col <- if (!is.null(raw_names) && g %in% names(raw_names)) {
        raw_names[[g]]
      } else {
        g
      }
      if (!source_col %in% names(dataset)) next

      values <- loa_trim(dataset[[source_col]])
      levels_g <- unique(values[!is.na(values)])
      if (anyNA(values)) levels_g <- c(levels_g, missing_group_label)

      for (lv in levels_g) {
        column <- paste0("stat_", g, "_", lv)
        owner <- loa_column_owner(column, group_variables)
        if (!identical(owner, g)) {
          note(g, if (is.na(owner)) "no grouping variable" else owner, column)
        }
      }
    }
  }

  if (length(found) == 0) {
    return(data.frame(
      group_variable = character(0), clashes_with = character(0),
      example = character(0), stringsAsFactors = FALSE
    ))
  }

  unique(do.call(rbind, found))
}


# -----------------------------------------------------------------------------
# 6. Variable coverage
# -----------------------------------------------------------------------------

#' Every Dataset Variable the Workbook Names, and Whether It Is There
#'
#' The check that runs the moment both files are in: each variable the workbook
#' refers to, where it is referred to, and whether the uploaded dataset
#' actually has it. Presence is judged by \code{\link{loa_var_present}}, so a
#' select_multiple parent counts when its child columns are present even though
#' ONA did not export the concatenated parent column.
#'
#' Names are matched as written in the workbook - raw dataset codes - against
#' the raw dataset, before any renaming.
#'
#' @param workbook The list from \code{\link{read_loa_workbook}}, or a bare
#'   named list of sheets.
#' @param dataset The dataset, with the ONA label row still on top.
#' @param sm_separator Select_multiple separator. Read from the `settings`
#'   sheet when not given.
#' @return A data frame with `variable`, `role`, `sheet`, `row` and `present`,
#'   one row per reference.
#' @export
loa_variable_coverage <- function(workbook, dataset, sm_separator = NULL) {
  wb <- if (!is.null(workbook$sheets)) workbook else list(sheets = workbook)
  sheets <- wb$sheets

  empty <- data.frame(
    variable = character(0), role = character(0), sheet = character(0),
    row = integer(0), present = logical(0), stringsAsFactors = FALSE
  )
  if (is.null(dataset) || ncol(dataset) == 0) {
    return(empty)
  }

  if (is.null(sm_separator)) {
    sm_separator <- loa_parse_settings(sheets$settings)$settings$sm_separator %||% "/"
  }

  refs <- list()
  ref <- function(variable, role, sheet, row) {
    keep <- !is.na(variable)
    if (!any(keep)) return(invisible(NULL))
    refs[[length(refs) + 1]] <<- data.frame(
      variable = variable[keep], role = role, sheet = sheet,
      row = as.integer(row)[keep], stringsAsFactors = FALSE
    )
  }

  # --- analysis --------------------------------------------------------------
  a <- sheets$analysis
  if (!is.null(a) && nrow(a) > 0) {
    rows <- seq_len(nrow(a)) + 1L

    is_ratio <- if ("analysis_type" %in% names(a)) {
      tolower(loa_trim(a$analysis_type)) %in% "ratio"
    } else {
      rep(FALSE, nrow(a))
    }

    if ("analysis_var" %in% names(a)) {
      v <- loa_trim(a$analysis_var)
      v[is_ratio] <- NA_character_ # a ratio names its variables elsewhere
      ref(v, "analysis_var", "analysis", rows)
    }

    for (cc in c("analysis_var_numerator", "analysis_var_denominator")) {
      if (cc %in% names(a)) {
        v <- loa_trim(a[[cc]])
        v[!is_ratio] <- NA_character_
        ref(v, sub("^analysis_var_", "ratio_", cc), "analysis", rows)
      }
    }

    if ("group_var" %in% names(a)) {
      ref(loa_trim(a$group_var), "group_var", "analysis", rows)
    }
  }

  # --- group_analysis --------------------------------------------------------
  ga <- sheets$group_analysis
  if (!is.null(ga) && nrow(ga) > 0 && "raw_data_name" %in% names(ga)) {
    inc <- if ("include" %in% names(ga)) loa_as_logical(ga$include) else rep(TRUE, nrow(ga))
    inc[is.na(inc)] <- TRUE
    v <- loa_trim(ga$raw_data_name)
    v[!inc] <- NA_character_
    ref(v, "group_var", "group_analysis", seq_len(nrow(ga)) + 1L)
  }

  # --- count_selections ------------------------------------------------------
  cs <- sheets$count_selections
  if (!is.null(cs) && nrow(cs) > 0 && "analysis_var" %in% names(cs)) {
    inc <- if ("include" %in% names(cs)) loa_as_logical(cs$include) else rep(TRUE, nrow(cs))
    inc[is.na(inc)] <- TRUE
    v <- loa_trim(cs$analysis_var)
    v[!inc] <- NA_character_
    ref(v, "count_selections", "count_selections", seq_len(nrow(cs)) + 1L)
  }

  # --- count_combinations ----------------------------------------------------
  cc <- sheets$count_combinations
  if (!is.null(cc) && nrow(cc) > 0 && "analysis_var" %in% names(cc)) {
    inc <- if ("include" %in% names(cc)) loa_as_logical(cc$include) else rep(TRUE, nrow(cc))
    inc[is.na(inc)] <- TRUE
    v <- loa_trim(cc$analysis_var)
    v[!inc] <- NA_character_
    ref(v, "count_combinations", "count_combinations", seq_len(nrow(cc)) + 1L)
  }

  # --- settings --------------------------------------------------------------
  settings <- loa_parse_settings(sheets$settings)$settings
  for (cc in c("weight_column", "strata_column")) {
    if (!is.null(settings[[cc]])) {
      ref(loa_trim(settings[[cc]]), cc, "settings", NA_integer_)
    }
  }

  if (length(refs) == 0) {
    return(empty)
  }

  out <- do.call(rbind, refs)
  out$present <- loa_var_present(out$variable, dataset, sm_separator)
  rownames(out) <- NULL
  out
}


#' How Bad Is a Missing Variable, by Where It Was Named
#'
#' Ordinarily an absent variable costs a row of output, not correctness, so it
#' warns. The two exceptions are the derived analyses: the pipeline's own
#' validators \emph{stop} when a `count_selections` or `count_combinations`
#' question is not a usable select_multiple, so a warning followed by a hard
#' abort would be worse than saying so up front.
#'
#' @param role The `role` column of \code{\link{loa_variable_coverage}}.
#' @return `"error"` or `"warning"`, one per element.
#' @keywords internal
loa_coverage_severity <- function(role) {
  ifelse(role %in% c("count_selections", "count_combinations"), "error", "warning")
}


#' Turn a Coverage Table into Problems
#'
#' @param coverage The output of \code{\link{loa_variable_coverage}}.
#' @return A problems data frame.
#' @keywords internal
loa_coverage_problems <- function(coverage) {
  if (nrow(coverage) == 0) {
    return(loa_no_problems())
  }

  consequence <- c(
    analysis_var = "this analysis will be skipped",
    ratio_numerator = "this ratio will be skipped",
    ratio_denominator = "this ratio will be skipped",
    group_var = "this disaggregation will not appear in the output",
    count_selections = "the run will stop when it reaches the selection counts",
    count_combinations = "the run will stop when it reaches the choice combinations",
    weight_column = "the analysis will run unweighted",
    strata_column = "the analysis will run without strata"
  )

  missing <- coverage[!coverage$present, , drop = FALSE]
  found <- list()

  if (nrow(missing) > 0) {
    found[[1]] <- loa_problem(
      missing$sheet,
      missing$row,
      loa_coverage_severity(missing$role),
      paste0(
        "'", missing$variable, "' is not a column of the dataset",
        " (referenced as ", missing$role, "), so ",
        ifelse(
          missing$role %in% names(consequence),
          consequence[missing$role],
          "it cannot be used"
        ), "."
      )
    )
  }

  # Nothing left to analyse is a different failure from a few skipped rows:
  # ck_stack_loa() stops outright, so say so before the run rather than during.
  analysable <- coverage[
    coverage$role %in% c("analysis_var", "ratio_numerator", "ratio_denominator"), ,
    drop = FALSE
  ]
  if (nrow(analysable) > 0 && !any(analysable$present)) {
    found[[length(found) + 1]] <- loa_problem(
      "analysis", NA, "error",
      paste0(
        "None of the ", length(unique(analysable$variable)),
        " variable(s) requested in the analysis sheet are in this dataset. ",
        "Check that the List of Analysis and the dataset belong to the same ",
        "round: the column names look unrelated."
      )
    )
  }

  if (length(found) == 0) loa_no_problems() else do.call(rbind, found)
}


#' Summarise Variable Coverage, One Row per Variable
#'
#' The display form: each variable once, with every place it is used and
#' whether the dataset has it.
#'
#' @param coverage The output of \code{\link{loa_variable_coverage}}.
#' @return A data frame with `Variable`, `Used as`, `References` and `Status`.
#' @export
loa_coverage_summary <- function(coverage) {
  if (nrow(coverage) == 0) {
    return(data.frame(
      Variable = character(0), `Used as` = character(0),
      References = integer(0), Status = character(0),
      check.names = FALSE, stringsAsFactors = FALSE
    ))
  }

  variables <- unique(coverage$variable)
  roles <- vapply(
    variables,
    function(v) paste(unique(coverage$role[coverage$variable == v]), collapse = ", "),
    character(1),
    USE.NAMES = FALSE
  )
  n_refs <- vapply(
    variables, function(v) sum(coverage$variable == v), integer(1), USE.NAMES = FALSE
  )
  present <- vapply(
    variables, function(v) all(coverage$present[coverage$variable == v]),
    logical(1), USE.NAMES = FALSE
  )

  out <- data.frame(
    Variable = variables,
    `Used as` = roles,
    References = n_refs,
    Status = ifelse(present, "Found", "Not in dataset"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  # Missing first: it is the reason anyone opens this table.
  out[order(present, out$Variable), , drop = FALSE]
}


# -----------------------------------------------------------------------------
# 7. Validation
# -----------------------------------------------------------------------------

#' Run a Pipeline Validator and Convert its Error into a Problem
#'
#' The pipeline already validates `count_selections` and `count_combinations`,
#' with better messages than a second implementation would produce. Rather than
#' duplicate those checks, they are called here when available and their
#' condition is turned into a problem row.
#'
#' @param fn Name of the validator function.
#' @param args List of arguments.
#' @param sheet Sheet to attribute any problem to.
#' @return A problems data frame.
#' @keywords internal
loa_delegate_check <- function(fn, args, sheet) {
  if (!exists(fn, mode = "function")) {
    return(loa_no_problems())
  }

  tryCatch(
    {
      do.call(get(fn, mode = "function"), args)
      loa_no_problems()
    },
    error = function(e) {
      loa_problem(sheet, NA, "error", conditionMessage(e))
    },
    warning = function(w) {
      loa_problem(sheet, NA, "warning", conditionMessage(w))
    }
  )
}


#' Validate a List of Analysis Workbook
#'
#' Every check the schema defines, in one place. Never stops: it returns the
#' problems so the app can show all of them at once instead of one per run.
#'
#' The severity rule, applied throughout:
#' **fatal** when the run would produce wrong or misleading output; **warning**
#' when it would only produce less output.
#'
#' @param workbook The list returned by \code{\link{read_loa_workbook}}, or a
#'   bare named list of sheets.
#' @param dataset Optional dataset, with the ONA label row still on top. When
#'   supplied, variable-existence and column-ownership checks are run.
#' @return A data frame with `sheet`, `row`, `severity` and `message`.
#' @export
validate_loa <- function(workbook, dataset = NULL) {
  wb <- if (!is.null(workbook$sheets)) workbook else list(sheets = workbook)
  sheets <- wb$sheets

  found <- list()
  add <- function(x) if (nrow(x) > 0) found[[length(found) + 1]] <<- x
  problem <- function(sheet, row, severity, message) {
    add(loa_problem(sheet, row, severity, message))
  }

  # --- 0. Workbook level -----------------------------------------------------
  for (nm in wb$unknown_sheets %||% character(0)) {
    problem("workbook", NA, "error", paste0(
      "Sheet '", nm, "' is not part of the List of Analysis schema. ",
      "Expected one of: ", paste(loa_known_sheets(), collapse = ", "),
      ". Rename it to 'notes', or prefix it with '_', to have it ignored."
    ))
  }

  if (identical(wb$format, "CSV")) {
    problem("workbook", NA, "warning", paste0(
      "A CSV List of Analysis carries no sheets, so only the analysis table ",
      "was read. Grouping variables, selection counts, choice combinations, ",
      "excluded choices and settings all fall back to their defaults. ",
      "Upload an XLSX workbook to configure them."
    ))
  }

  if (is.null(sheets$analysis)) {
    problem("workbook", NA, "error",
            "The List of Analysis must contain an 'analysis' sheet.")
    return(loa_collect(found))
  }

  # --- 1. Settings first: later checks need sm_separator ---------------------
  parsed <- loa_parse_settings(sheets$settings)
  add(parsed$problems)

  sm_separator <- parsed$settings$sm_separator %||% "/"
  missing_group_label <- parsed$settings$missing_group_label %||% "Missing"
  skip_label_row <- parsed$settings$skip_label_row %||% TRUE

  # The ONA label row is text and is not a respondent; reading levels from it
  # would invent a group for every disaggregation.
  data_rows <- dataset
  if (!is.null(dataset) && isTRUE(skip_label_row) && nrow(dataset) >= 1) {
    data_rows <- dataset[-1, , drop = FALSE]
  }

  # --- 2. analysis -----------------------------------------------------------
  analysis <- sheets$analysis
  required <- c("analysis_type", "analysis_var")
  absent <- setdiff(required, names(analysis))

  if (length(absent) > 0) {
    problem("analysis", NA, "error", paste0(
      "The analysis sheet is missing required column(s): ",
      paste(absent, collapse = ", "), "."
    ))
  } else {
    a_type <- tolower(loa_trim(analysis$analysis_type))
    a_var <- loa_trim(analysis$analysis_var)

    for (i in seq_len(nrow(analysis))) {
      excel_row <- i + 1L

      if (is.na(a_type[i])) {
        problem("analysis", excel_row, "error", "analysis_type is empty.")
        next
      }

      if (a_type[i] %in% loa_derived_analysis_types()) {
        problem("analysis", excel_row, "error", paste0(
          "'", a_type[i], "' is produced by the pipeline, not requested here. ",
          "Use the ",
          if (a_type[i] == "count_select_multiple") {
            "count_selections"
          } else {
            "count_combinations"
          },
          " sheet instead."
        ))
        next
      }

      if (!a_type[i] %in% loa_known_analysis_types()) {
        problem("analysis", excel_row, "error", paste0(
          "'", analysis$analysis_type[i], "' is not a supported analysis_type. ",
          "Allowed: ", paste(loa_known_analysis_types(), collapse = ", "), "."
        ))
        next
      }

      if (a_type[i] == "ratio") {
        cols <- c("analysis_var_numerator", "analysis_var_denominator")
        have <- vapply(
          cols,
          function(cc) cc %in% names(analysis) && !is.na(loa_trim(analysis[[cc]][i])),
          logical(1)
        )
        if (!all(have)) {
          problem("analysis", excel_row, "error", paste0(
            "A ratio needs both ", paste(cols[!have], collapse = " and "), "."
          ))
        }
      } else if (is.na(a_var[i])) {
        problem("analysis", excel_row, "error", "analysis_var is empty.")
      }
    }

    # Same analysis twice produces duplicate rows the pivot silently de-dupes.
    if (nrow(analysis) > 1) {
      grp <- if ("group_var" %in% names(analysis)) {
        loa_trim(analysis$group_var)
      } else {
        rep(NA_character_, nrow(analysis))
      }
      dup <- duplicated(data.frame(a_type, a_var, grp, stringsAsFactors = FALSE))
      for (i in which(dup)) {
        problem("analysis", i + 1L, "warning", paste0(
          "This analysis repeats an earlier row (", a_type[i], " of ", a_var[i],
          "). Only the first will appear in the output."
        ))
      }
    }
  }

  # --- 3. group_analysis -----------------------------------------------------
  ga <- sheets$group_analysis
  rename_map <- stats::setNames(character(0), character(0))

  if (!is.null(ga) && nrow(ga) > 0) {
    required <- c("raw_data_name", "new_name")
    absent <- setdiff(required, names(ga))

    if (length(absent) > 0) {
      problem("group_analysis", NA, "error", paste0(
        "The group_analysis sheet is missing required column(s): ",
        paste(absent, collapse = ", "), "."
      ))
    } else {
      raw <- loa_trim(ga$raw_data_name)
      new <- loa_trim(ga$new_name)

      if ("include" %in% names(ga)) {
        inc <- loa_as_logical(ga$include)
        # A blank include means "yes"; anything unrecognised is reported rather
        # than guessed, since guessing wrong silently drops a disaggregation.
        for (i in which(!is.na(loa_trim(ga$include)) & is.na(inc))) {
          problem("group_analysis", i + 1L, "error", paste0(
            "include must be TRUE or FALSE. Found '", ga$include[i], "'."
          ))
        }
        inc[is.na(inc)] <- TRUE
      } else {
        inc <- rep(TRUE, nrow(ga))
      }

      for (i in seq_len(nrow(ga))) {
        excel_row <- i + 1L
        if (!inc[i]) next

        if (is.na(raw[i]) || is.na(new[i])) {
          problem("group_analysis", excel_row, "error",
                  "Both raw_data_name and new_name are required.")
          next
        }
        if (tolower(new[i]) == "overall") {
          problem("group_analysis", excel_row, "error", paste0(
            "'Overall' is added automatically and must not be listed here."
          ))
          next
        }
        if (i > 1 && raw[i] %in% raw[seq_len(i - 1L)][inc[seq_len(i - 1L)]]) {
          problem("group_analysis", excel_row, "error", paste0(
            "'", raw[i], "' is renamed more than once."
          ))
          next
        }
        if (i > 1 && new[i] %in% new[seq_len(i - 1L)][inc[seq_len(i - 1L)]]) {
          problem("group_analysis", excel_row, "error", paste0(
            "Two rows both rename to '", new[i], "'. Output names must be distinct."
          ))
          next
        }

        if (!is.null(dataset)) {
          # Presence is not tested here: loa_variable_coverage() owns every
          # "is this variable in the dataset" question, so the answer is
          # reported once and in one voice.
          if (new[i] %in% names(dataset) && new[i] != raw[i]) {
            problem("group_analysis", excel_row, "error", paste0(
              "'", new[i], "' is already a column of the dataset. Renaming ",
              "'", raw[i], "' to it would produce two columns with one name."
            ))
            next
          }
        }
      }

      rename_map <- loa_rename_map(ga, dataset, sm_separator)

      clashes <- loa_group_tag_clashes(
        group_variables = unname(rename_map),
        dataset = data_rows,
        raw_names = stats::setNames(names(rename_map), unname(rename_map)),
        missing_group_label = missing_group_label
      )

      for (g in unique(clashes$group_variable)) {
        hit <- clashes[clashes$group_variable == g, , drop = FALSE]
        problem("group_analysis", NA, "error", paste0(
          "Output columns for '", g, "' would be attributed to '",
          hit$clashes_with[1], "' instead (e.g. ", hit$example[1], "). ",
          "The wide table names its columns stat_<group>_<value>, and the ",
          "column map decides which disaggregation a column belongs to by ",
          "matching '_<group>_', first match wins. The table itself stays ",
          "correct; the map that labels it does not. Rename one of them so ",
          "that neither name contains the other."
        ))
      }
    }
  }

  # --- 4. count_selections ---------------------------------------------------
  cs <- sheets$count_selections
  count_selections <- character(0)

  if (!is.null(cs) && nrow(cs) > 0) {
    if (!"analysis_var" %in% names(cs)) {
      problem("count_selections", NA, "error",
              "The count_selections sheet must have an 'analysis_var' column.")
    } else {
      v <- loa_trim(cs$analysis_var)
      inc <- if ("include" %in% names(cs)) loa_as_logical(cs$include) else rep(TRUE, nrow(cs))
      inc[is.na(inc)] <- TRUE

      for (i in seq_len(nrow(cs))) {
        if (!inc[i]) next
        if (is.na(v[i])) {
          problem("count_selections", i + 1L, "error", "analysis_var is empty.")
        } else if (i > 1 && v[i] %in% v[seq_len(i - 1L)][inc[seq_len(i - 1L)]]) {
          problem("count_selections", i + 1L, "warning", paste0(
            "'", v[i], "' is listed more than once; the duplicate is ignored."
          ))
        }
      }

      count_selections <- unique(stats::na.omit(v[inc]))
    }
  }

  # --- 5. count_combinations -------------------------------------------------
  cc <- sheets$count_combinations
  count_combinations <- list()

  if (!is.null(cc) && nrow(cc) > 0) {
    required <- c("analysis_var", "choice_label")
    absent <- setdiff(required, names(cc))

    if (length(absent) > 0) {
      problem("count_combinations", NA, "error", paste0(
        "The count_combinations sheet is missing required column(s): ",
        paste(absent, collapse = ", "), "."
      ))
    } else {
      v <- loa_trim(cc$analysis_var)
      label <- loa_trim(cc$choice_label)

      for (i in seq_len(nrow(cc))) {
        if (is.na(v[i])) {
          problem("count_combinations", i + 1L, "error", "analysis_var is empty.")
        }
        if (is.na(label[i])) {
          problem("count_combinations", i + 1L, "error", "choice_label is empty.")
        }
      }

      count_combinations <- loa_combination_list(cc)
    }
  }

  # --- 6. exclude_choices ----------------------------------------------------
  ec <- sheets$exclude_choices

  if (!is.null(ec) && nrow(ec) > 0) {
    if (!"choice_label" %in% names(ec)) {
      problem("exclude_choices", NA, "error",
              "The exclude_choices sheet must have a 'choice_label' column.")
    } else {
      v <- loa_trim(ec$choice_label)
      inc <- if ("include" %in% names(ec)) loa_as_logical(ec$include) else rep(TRUE, nrow(ec))
      inc[is.na(inc)] <- TRUE

      for (i in seq_len(nrow(ec))) {
        if (inc[i] && is.na(v[i])) {
          problem("exclude_choices", i + 1L, "error", "choice_label is empty.")
        }
      }
    }
  }

  # --- 7. Delegate the deep checks to the pipeline's own validators ----------
  # Run against the renamed dataset, because that is what the pipeline sees.
  if (!is.null(dataset) && !loa_has_errors(loa_collect(found))) {
    renamed <- apply_rename_map(data_rows, rename_map, sm_separator)
    renamed_loa <- loa_apply_map_to_analysis(sheets$analysis, rename_map)

    if (length(count_selections) > 0) {
      add(loa_delegate_check(
        "ck_check_count_selections",
        list(
          count_selections = loa_rename_values(count_selections, rename_map),
          dataset = renamed,
          loa = renamed_loa,
          sm_separator = sm_separator
        ),
        "count_selections"
      ))
    }

    if (length(count_combinations) > 0) {
      names(count_combinations) <-
        loa_rename_values(names(count_combinations), rename_map)

      add(loa_delegate_check(
        "ck_check_choice_combinations",
        list(
          combinations = count_combinations,
          dataset = renamed,
          loa = renamed_loa,
          sm_separator = sm_separator,
          ignore_case = parsed$settings$count_combinations_ignore_case %||% TRUE,
          exclude_choices = loa_exclude_choices(sheets$exclude_choices),
          max_choices = parsed$settings$max_combination_choices %||% 6
        ),
        "count_combinations"
      ))
    }

  }

  # --- 8. Every variable the workbook names, against the dataset -------------
  if (!is.null(dataset)) {
    add(loa_coverage_problems(loa_variable_coverage(wb, dataset, sm_separator)))
  }

  loa_collect(found)
}


#' Bind a List of Problem Tables
#' @param found A list of problems data frames.
#' @return One problems data frame, errors first.
#' @keywords internal
loa_collect <- function(found) {
  if (length(found) == 0) return(loa_no_problems())
  out <- do.call(rbind, found)
  out <- out[order(out$severity != "error", out$sheet, out$row, na.last = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  out
}


#' Default for `NULL`
#' @param x A value.
#' @param y The fallback.
#' @return `x`, or `y` when `x` is `NULL`.
#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x


# -----------------------------------------------------------------------------
# 8. Assembling the specification
# -----------------------------------------------------------------------------

#' Build the count_combinations List from its Sheet
#' @param sheet The `count_combinations` data frame.
#' @return A named list of named character vectors.
#' @keywords internal
loa_combination_list <- function(sheet) {
  if (is.null(sheet) || nrow(sheet) == 0) return(list())
  if (!all(c("analysis_var", "choice_label") %in% names(sheet))) return(list())

  v <- loa_trim(sheet$analysis_var)
  label <- loa_trim(sheet$choice_label)
  display <- if ("display_name" %in% names(sheet)) {
    loa_trim(sheet$display_name)
  } else {
    rep(NA_character_, nrow(sheet))
  }

  inc <- if ("include" %in% names(sheet)) loa_as_logical(sheet$include) else rep(TRUE, nrow(sheet))
  inc[is.na(inc)] <- TRUE

  keep <- inc & !is.na(v) & !is.na(label)
  if (!any(keep)) return(list())

  # An unnamed choice falls back to the full export label as its row label,
  # which is what ck_check_choice_combinations() does with an unnamed vector.
  display[is.na(display)] <- label[is.na(display)]

  out <- list()
  for (parent in unique(v[keep])) {
    rows <- which(keep & v == parent)
    out[[parent]] <- stats::setNames(label[rows], display[rows])
  }
  out
}


#' Build the exclude_choices Vector from its Sheet
#' @param sheet The `exclude_choices` data frame.
#' @return A character vector.
#' @keywords internal
loa_exclude_choices <- function(sheet) {
  if (is.null(sheet) || nrow(sheet) == 0) return(character(0))
  if (!"choice_label" %in% names(sheet)) return(character(0))

  v <- loa_trim(sheet$choice_label)
  inc <- if ("include" %in% names(sheet)) loa_as_logical(sheet$include) else rep(TRUE, nrow(sheet))
  inc[is.na(inc)] <- TRUE

  unique(stats::na.omit(v[inc]))
}


#' Apply the Rename Map to the Variable Columns of the analysis Sheet
#' @param analysis The `analysis` data frame.
#' @param map The rename map.
#' @return `analysis` with its variable columns rewritten.
#' @keywords internal
loa_apply_map_to_analysis <- function(analysis, map) {
  if (is.null(analysis) || length(map) == 0) return(analysis)

  cols <- c(
    "analysis_var", "analysis_var_numerator",
    "analysis_var_denominator", "group_var"
  )
  for (cc in intersect(cols, names(analysis))) {
    analysis[[cc]] <- loa_rename_values(analysis[[cc]], map)
  }
  analysis
}


#' Build an Analysis Specification from a List of Analysis Workbook
#'
#' The single internal representation of a requested run. Everything downstream
#' reads this; nothing re-reads the workbook.
#'
#' Variable names are resolved here: the workbook is written in raw dataset
#' codes throughout, and the spec comes back in post-rename names, matching the
#' dataset \code{\link{run_analysis_spec}} will hand to the pipeline.
#'
#' This never stops. Inspect `problems` (or call
#' \code{\link{loa_has_errors}}) before running.
#'
#' @param workbook The list returned by \code{\link{read_loa_workbook}}, or a
#'   bare named list of sheets.
#' @param dataset Optional dataset, with the ONA label row still on top.
#' @return An object of class `analysis_spec`: a list with `loa`,
#'   `group_variables`, `rename_map`, `count_selections`, `count_combinations`,
#'   `exclude_choices`, `settings` and `problems`.
#' @export
build_analysis_spec <- function(workbook, dataset = NULL) {
  wb <- if (!is.null(workbook$sheets)) workbook else list(sheets = workbook)
  sheets <- wb$sheets

  problems <- validate_loa(wb, dataset)
  parsed <- loa_parse_settings(sheets$settings)
  sm_separator <- parsed$settings$sm_separator %||% "/"

  rename_map <- loa_rename_map(sheets$group_analysis, dataset, sm_separator)

  combinations <- loa_combination_list(sheets$count_combinations)
  if (length(combinations) > 0) {
    names(combinations) <- loa_rename_values(names(combinations), rename_map)
  }

  count_selections <- character(0)
  cs <- sheets$count_selections
  if (!is.null(cs) && nrow(cs) > 0 && "analysis_var" %in% names(cs)) {
    v <- loa_trim(cs$analysis_var)
    inc <- if ("include" %in% names(cs)) loa_as_logical(cs$include) else rep(TRUE, nrow(cs))
    inc[is.na(inc)] <- TRUE
    count_selections <- loa_rename_values(
      unique(stats::na.omit(v[inc])), rename_map
    )
  }

  spec <- list(
    loa = loa_apply_map_to_analysis(sheets$analysis, rename_map),
    group_variables = c("Overall", unname(rename_map)),
    rename_map = rename_map,
    count_selections = count_selections,
    count_combinations = combinations,
    exclude_choices = loa_exclude_choices(sheets$exclude_choices),
    settings = parsed$settings,
    problems = problems,
    source = list(filename = wb$filename, format = wb$format)
  )

  class(spec) <- c("analysis_spec", "list")
  spec
}


#' @export
print.analysis_spec <- function(x, ...) {
  cat("<analysis_spec>\n")
  cat("  analyses          :", nrow(x$loa), "\n")
  cat("  group variables   :", paste(x$group_variables, collapse = ", "), "\n")
  cat("  renamed           :", length(x$rename_map), "\n")
  cat("  count_selections  :", length(x$count_selections), "\n")
  cat("  count_combinations:", length(x$count_combinations), "\n")
  cat("  exclude_choices   :", length(x$exclude_choices), "\n")
  cat("  settings          :", length(x$settings), "\n")
  cat(
    "  problems          :",
    sum(x$problems$severity == "error"), "error(s),",
    sum(x$problems$severity == "warning"), "warning(s)\n"
  )
  invisible(x)
}


# -----------------------------------------------------------------------------
# 9. Running
# -----------------------------------------------------------------------------

#' Assemble the Pipeline Call for a Specification
#'
#' Split out from \code{\link{run_analysis_spec}} so the argument list can be
#' inspected and tested without running an analysis.
#'
#' @param dataset The dataset, with the ONA label row still on top.
#' @param spec An `analysis_spec`.
#' @return A named list of arguments for `run_group_analysis_pipeline()`.
#' @export
analysis_spec_args <- function(dataset, spec) {
  sm_separator <- spec$settings$sm_separator %||% "/"

  args <- list(
    dataset = apply_rename_map(dataset, spec$rename_map, sm_separator),
    loa = spec$loa,
    group_variables = spec$group_variables
  )

  if (length(spec$count_selections) > 0) {
    args$count_selections <- spec$count_selections
  }
  if (length(spec$count_combinations) > 0) {
    args$count_combinations <- spec$count_combinations
  }
  if (length(spec$exclude_choices) > 0) {
    args$exclude_choices <- spec$exclude_choices
  }

  # Only settings the workbook actually supplied are passed, so every unset
  # argument keeps the pipeline's own default.
  utils::modifyList(args, spec$settings)
}


#' Run a Pipeline from an Analysis Specification
#'
#' The only place the workbook and the analysis pipeline meet.
#'
#' @param dataset The dataset, with the ONA label row still on top.
#' @param spec An `analysis_spec` from \code{\link{build_analysis_spec}}.
#' @param pipeline The function to call. Injectable so the assembly can be
#'   tested without the analysis code loaded.
#' @param ... Further arguments passed to `pipeline`, overriding the spec. Used
#'   by the app for run-time concerns such as `verbose`.
#' @return Whatever `pipeline` returns.
#' @export
run_analysis_spec <- function(dataset,
                              spec,
                              pipeline = NULL,
                              ...) {
  if (loa_has_errors(spec$problems)) {
    errors <- spec$problems[spec$problems$severity == "error", , drop = FALSE]
    stop(
      paste0(
        "The List of Analysis has ", nrow(errors),
        " problem(s) that must be fixed before running:\n  - ",
        paste(
          paste0(
            errors$sheet,
            ifelse(is.na(errors$row), "", paste0(" row ", errors$row)),
            ": ", errors$message
          ),
          collapse = "\n  - "
        )
      ),
      call. = FALSE
    )
  }

  if (is.null(pipeline)) {
    if (!exists("run_group_analysis_pipeline", mode = "function")) {
      stop(
        "run_group_analysis_pipeline() is not available. Source the analysis functions first.",
        call. = FALSE
      )
    }
    pipeline <- get("run_group_analysis_pipeline", mode = "function")
  }

  do.call(pipeline, utils::modifyList(analysis_spec_args(dataset, spec), list(...)))
}
