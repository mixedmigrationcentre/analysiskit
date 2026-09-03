# =============================================================================
# Writing a completed run to a destination folder
# =============================================================================
#
# Output assembly is kept separate from the analysis: run_group_analysis_pipeline()
# produces results, format_my_xlsx_variable_x_group() styles them, and this file
# decides where the file goes and what it is called.
#
# Nothing here writes anything as a side effect of a reactive recalculating.
# ak_export_results() is called once, from the Run handler, and only after the
# analysis has completed without error.
# =============================================================================


#' Columns That Are Counts Rather Than Statistics
#'
#' The formatter needs the statistic prefixes and the count prefixes separately:
#' counts are formatted as whole numbers, kept narrow, and are what the sample
#' sizes and the empty-group test are read from.
#'
#' @return A character vector.
#' @keywords internal
ak_count_columns <- function() {
  c("n", "n_total", "n_w", "n_w_total")
}


#' Export Settings for a Completed Run
#'
#' Derived from the run rather than hard-coded, so the split stays correct when
#' the workbook asks for different `value_columns`. With the defaults this
#' reproduces the settings behind `4Mi_results_QN6.xlsx`: the blocks layout, one
#' sheet per sector, `stat` as the statistic and `n` / `n_total` as the counts.
#'
#' `length(c(value_columns, total_columns))` must equal the number of columns
#' the pipeline wrote per group block, or the formatter cuts the blocks in the
#' wrong places. Splitting one list guarantees that by construction.
#'
#' @param results The list returned by `run_group_analysis_pipeline()`.
#' @param spec The `analysis_spec` behind the run.
#' @return A named list of arguments for `format_my_xlsx_variable_x_group()`.
#' @export
ak_export_settings <- function(results, spec) {
  pipeline_columns <- spec$settings$value_columns %||% c("stat", "n", "n_total")

  total_columns <- intersect(pipeline_columns, ak_count_columns())
  value_columns <- setdiff(pipeline_columns, ak_count_columns())

  if (length(value_columns) == 0) {
    stop(
      paste0(
        "The List of Analysis asks only for count columns (",
        paste(pipeline_columns, collapse = ", "),
        "), so there is no statistic to report. Add 'stat' to value_columns."
      ),
      call. = FALSE
    )
  }

  # One sheet per sector, as in the example output. Splitting on a column the
  # table does not have aborts the export, so the choice is made from what is
  # actually there: sector by preference, otherwise the first metadata column
  # the workbook carried through, otherwise a single sheet.
  wide_names <- names(results$combined_results)
  carried <- intersect(spec$settings$extra_columns %||% character(0), wide_names)
  split_by <- if ("sector" %in% wide_names) {
    "sector"
  } else if (length(carried) > 0) {
    carried[1]
  } else {
    "none"
  }

  list(
    layout = "blocks",
    value_columns = value_columns,
    total_columns = if (length(total_columns) > 0) total_columns else NULL,
    split_by = split_by,
    column_map = results$column_map,
    repeat_overall = TRUE,
    drop_empty_groups = TRUE,
    # MMC house style: percentages are published to the nearest whole number.
    # Set here rather than left to the formatter's default so the choice is
    # visible at the point where the app decides how its output looks.
    percent_digits = 0
  )
}


#' Prepare the Wide Table for the Formatter
#'
#' Two things about the pipeline's wide table need attention before it is
#' formatted, and both come from the separator rows.
#'
#' `ck_insert_count_separators()` adds a blank spacer and a heading above each
#' derived block, marked in a `row_type` column that it appends at the *end* of
#' the table. Left alone:
#'
#' \itemize{
#'   \item the formatter reads everything after the first `stat_` column as a
#'     statistic, so a trailing `row_type` is reported as an unrecognised
#'     column and dropped with a warning;
#'   \item the marker rows carry no `sector`, so splitting sheets by sector
#'     sends them all to a sheet of their own called "not specified".
#' }
#'
#' The spacer and heading exist to separate a derived block from the choice
#' rows above it in the **matrix** layout, where everything is one continuous
#' table. The **blocks** layout already gives every question its own titled
#' table, so there the markers are redundant and are removed; in the matrix
#' layout they are kept and `row_type` is moved in among the identifier
#' columns, where the formatter expects identifiers to be.
#'
#' @param wide The `combined_results` table.
#' @param layout The formatter layout the table is headed for.
#' @return `wide`, ready to format.
#' @export
ak_prepare_for_export <- function(wide, layout = c("blocks", "matrix")) {
  layout <- match.arg(layout)

  if (!"row_type" %in% names(wide)) {
    return(wide)
  }

  if (layout == "blocks") {
    keep <- is.na(wide$row_type) | wide$row_type == "data"
    wide <- wide[keep, setdiff(names(wide), "row_type"), drop = FALSE]
    rownames(wide) <- NULL
    return(wide)
  }

  first_stat <- which(startsWith(names(wide), "stat_"))
  if (length(first_stat) == 0) {
    return(wide)
  }

  others <- setdiff(names(wide), "row_type")
  before <- others[seq_len(first_stat[1] - 1L)]
  after <- setdiff(others, before)

  wide[, c(before, "row_type", after), drop = FALSE]
}


#' A Reproducible Output Filename
#'
#' Names the file after the dataset it came from and the moment it was
#' produced, so two runs of the same dataset never collide and a file found
#' later can be traced back.
#'
#' @param dataset_name Original dataset filename.
#' @param when The run time. Defaults to now.
#' @param prefix Leading token.
#' @return A filename ending in `.xlsx`.
#' @export
ak_output_filename <- function(dataset_name,
                               when = Sys.time(),
                               prefix = "analysiskit") {
  stem <- tools::file_path_sans_ext(basename(as.character(dataset_name)))
  stem <- gsub("[^A-Za-z0-9._-]+", "_", stem)
  stem <- gsub("^_+|_+$", "", stem)
  if (!nzchar(stem)) stem <- "results"

  paste0(prefix, "_", stem, "_", format(when, "%Y%m%d-%H%M%S"), ".xlsx")
}


#' Find a Path That Does Not Already Exist
#'
#' A second run inside the same second would otherwise overwrite the first.
#'
#' @param folder Destination folder.
#' @param filename Preferred filename.
#' @param limit How many suffixes to try before giving up.
#' @return A full path that does not exist.
#' @keywords internal
ak_unique_path <- function(folder, filename, limit = 99L) {
  path <- file.path(folder, filename)
  if (!file.exists(path)) {
    return(path)
  }

  stem <- tools::file_path_sans_ext(filename)
  for (i in seq_len(limit)) {
    candidate <- file.path(folder, paste0(stem, "_", i, ".xlsx"))
    if (!file.exists(candidate)) {
      return(candidate)
    }
  }

  stop(
    paste0("Could not find an unused filename in ", folder, "."),
    call. = FALSE
  )
}


#' Check That a Folder Can Be Written To
#'
#' Checked before the analysis is exported rather than after, so a run is never
#' lost to a folder that turns out to be read-only.
#'
#' @param folder Path to check.
#' @return `NULL` when the folder is usable, otherwise a message explaining
#'   why it is not.
#' @export
ak_check_folder <- function(folder) {
  if (is.null(folder) || length(folder) != 1 || is.na(folder) || !nzchar(folder)) {
    return("No destination folder has been chosen yet.")
  }
  if (!dir.exists(folder)) {
    return(paste0("The folder '", folder, "' does not exist."))
  }
  if (file.access(folder, mode = 2) != 0) {
    return(paste0(
      "The folder '", folder, "' cannot be written to. Choose another one."
    ))
  }
  NULL
}


#' Provenance Lines for the Workbook's Readme Sheet
#'
#' The record of what produced the file travels inside the file, rather than in
#' a second document that can be separated from it.
#'
#' @param spec The `analysis_spec` behind the run.
#' @param dataset_name Original dataset filename.
#' @param when The run time.
#' @return A character vector of readme lines.
#' @export
ak_provenance <- function(spec, dataset_name, when = Sys.time()) {
  settings <- spec$settings

  described <- if (length(settings) == 0) {
    "pipeline defaults throughout"
  } else {
    paste(
      vapply(
        names(settings),
        function(nm) paste0(nm, " = ", paste(settings[[nm]], collapse = ", ")),
        character(1),
        USE.NAMES = FALSE
      ),
      collapse = "; "
    )
  }

  c(
    paste0("Produced by Analysis Kit on ", format(when, "%Y-%m-%d %H:%M:%S"), "."),
    paste0("Dataset: ", dataset_name, "."),
    paste0(
      "List of Analysis: ",
      spec$source$filename %||% "not recorded",
      " (", nrow(spec$loa), " analyses)."
    ),
    paste0(
      "Disaggregated by: ",
      paste(spec$group_variables, collapse = ", "), "."
    ),
    paste0("Settings: ", described, ".")
  )
}


#' Write a Completed Run to a Folder
#'
#' Called once, from the Run handler, and only after the analysis has finished
#' without error. Returns the path written so the interface can name it.
#'
#' @param results The list returned by `run_group_analysis_pipeline()`.
#' @param spec The `analysis_spec` behind the run.
#' @param folder Destination folder, already chosen by the user.
#' @param dataset_name Original dataset filename, used for the output name.
#' @param when The run time.
#' @param formatter The export function. Injectable so the wiring can be tested
#'   without building a real workbook.
#' @param verbose Passed to the formatter.
#' @return The full path of the file written.
#' @export
ak_export_results <- function(results,
                              spec,
                              folder,
                              dataset_name,
                              when = Sys.time(),
                              formatter = NULL,
                              verbose = FALSE) {
  problem <- ak_check_folder(folder)
  if (!is.null(problem)) {
    stop(problem, call. = FALSE)
  }

  if (is.null(formatter)) {
    if (!exists("format_my_xlsx_variable_x_group", mode = "function")) {
      stop(
        paste0(
          "format_my_xlsx_variable_x_group() is not available. Add the export ",
          "functions to the functions/ folder and restart the application."
        ),
        call. = FALSE
      )
    }
    formatter <- get("format_my_xlsx_variable_x_group", mode = "function")
  }

  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop(
      "Writing the output workbook requires the 'openxlsx' package. Install it with install.packages('openxlsx').",
      call. = FALSE
    )
  }

  path <- ak_unique_path(folder, ak_output_filename(dataset_name, when))
  settings <- ak_export_settings(results, spec)

  args <- c(
    list(
      table_group_x_variable = ak_prepare_for_export(
        results$combined_results, settings$layout
      ),
      file_path = path,
      overwrite = FALSE,
      readme_text = ak_provenance(spec, dataset_name, when),
      verbose = verbose
    ),
    settings
  )

  do.call(formatter, args)

  if (!file.exists(path)) {
    stop(
      paste0("The export reported success but no file appeared at ", path, "."),
      call. = FALSE
    )
  }

  path
}
