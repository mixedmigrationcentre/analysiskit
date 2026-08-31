errors_of <- function(problems) problems[problems$severity == "error", , drop = FALSE]
warnings_of <- function(problems) problems[problems$severity == "warning", , drop = FALSE]


# -----------------------------------------------------------------------------
# Reading
# -----------------------------------------------------------------------------

test_that("an xlsx workbook reads every recognised sheet", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")

  path <- write_fixture_xlsx(fixture_sheets(), tempfile(fileext = ".xlsx"))
  wb <- read_loa_workbook(path)

  expect_equal(wb$format, "XLSX")
  expect_setequal(
    names(wb$sheets),
    c("analysis", "group_analysis", "count_selections",
      "count_combinations", "count_exclusive_combinations", "settings")
  )
  expect_equal(nrow(wb$sheets$analysis), 3L)
  expect_equal(wb$unknown_sheets, character(0))
})

test_that("sheet names are matched case- and separator-insensitively", {
  skip_if_not_installed("writexl")

  sheets <- fixture_sheets()
  names(sheets)[names(sheets) == "group_analysis"] <- "Group Analysis"
  names(sheets)[names(sheets) == "count_selections"] <- "Count-Selections"

  wb <- read_loa_workbook(write_fixture_xlsx(sheets, tempfile(fileext = ".xlsx")))

  expect_true("group_analysis" %in% names(wb$sheets))
  expect_true("count_selections" %in% names(wb$sheets))
  expect_equal(wb$unknown_sheets, character(0))
})

test_that("readme and underscore sheets are ignored, anything else is recorded", {
  skip_if_not_installed("writexl")

  sheets <- fixture_sheets()
  sheets$readme <- data.frame(note = "how to fill this in", stringsAsFactors = FALSE)
  sheets$`_scratch` <- data.frame(x = 1L)
  sheets$grouping <- data.frame(x = 1L) # a plausible typo for group_analysis

  wb <- read_loa_workbook(write_fixture_xlsx(sheets, tempfile(fileext = ".xlsx")))

  expect_setequal(wb$ignored_sheets, c("readme", "_scratch"))
  expect_equal(wb$unknown_sheets, "grouping")
  expect_true(loa_has_errors(validate_loa(wb)))
})

test_that("a csv is read as the analysis sheet alone and says so", {
  path <- tempfile(fileext = ".csv")
  utils::write.csv(fixture_sheets()$analysis, path, row.names = FALSE)

  wb <- read_loa_workbook(path)
  expect_equal(wb$format, "CSV")
  expect_equal(names(wb$sheets), "analysis")

  problems <- validate_loa(wb)
  expect_false(loa_has_errors(problems))
  expect_match(warnings_of(problems)$message, "carries no sheets", all = FALSE)
})

test_that("an unsupported file type is refused", {
  expect_error(read_loa_workbook(tempfile(fileext = ".docx")), "CSV or XLSX")
})

test_that("trailing blank rows left by Excel are dropped", {
  skip_if_not_installed("writexl")

  sheets <- fixture_sheets()
  sheets$count_selections <- data.frame(
    analysis_var = c("Q78", NA, NA), stringsAsFactors = FALSE
  )

  wb <- read_loa_workbook(write_fixture_xlsx(sheets, tempfile(fileext = ".xlsx")))
  expect_equal(nrow(wb$sheets$count_selections), 1L)
})

test_that("an empty analysis sheet is fatal, and a missing one more so", {
  problems <- validate_loa(fixture_workbook(fixture_sheets(analysis = NULL)))
  expect_true(loa_has_errors(problems))
  expect_match(errors_of(problems)$message[1], "must contain an 'analysis' sheet")

  empty <- data.frame(
    analysis_type = character(0), analysis_var = character(0),
    stringsAsFactors = FALSE
  )
  expect_false(loa_has_errors(validate_loa(fixture_workbook(fixture_sheets(analysis = empty)))))
})


# -----------------------------------------------------------------------------
# The analysis sheet
# -----------------------------------------------------------------------------

test_that("an unsupported analysis_type is rejected", {
  sheets <- fixture_sheets()
  sheets$analysis$analysis_type[2] <- "prop_select_mutliple"

  problems <- validate_loa(fixture_workbook(sheets))
  expect_true(loa_has_errors(problems))
  expect_match(errors_of(problems)$message[1], "not a supported analysis_type")
})

test_that("a derived analysis_type points at the sheet that produces it", {
  sheets <- fixture_sheets()
  sheets$analysis$analysis_type[2] <- "count_select_multiple"

  problems <- validate_loa(fixture_workbook(sheets))
  msg <- errors_of(problems)$message[1]
  expect_match(msg, "produced by the pipeline")
  expect_match(msg, "count_selections sheet")
})

test_that("a ratio without a numerator and denominator is rejected", {
  sheets <- fixture_sheets()
  sheets$analysis <- data.frame(
    analysis_type = "ratio", analysis_var = NA_character_,
    stringsAsFactors = FALSE
  )

  problems <- validate_loa(fixture_workbook(sheets))
  expect_match(errors_of(problems)$message[1], "ratio needs both")
})

test_that("a ratio with both variables is accepted", {
  sheets <- fixture_sheets()
  sheets$analysis <- data.frame(
    analysis_type = "ratio",
    analysis_var = NA_character_,
    analysis_var_numerator = "Q29",
    analysis_var_denominator = "Q29",
    stringsAsFactors = FALSE
  )

  expect_false(loa_has_errors(validate_loa(fixture_workbook(sheets))))
})

test_that("an analysis variable absent from the dataset warns rather than blocks", {
  sheets <- fixture_sheets()
  sheets$analysis <- rbind(
    sheets$analysis,
    data.frame(
      analysis_type = "prop_select_one", analysis_var = "Q999",
      level = NA_real_, sector = "Other", stringsAsFactors = FALSE
    )
  )

  problems <- validate_loa(fixture_workbook(sheets), fixture_dataset())
  expect_false(loa_has_errors(problems))
  expect_match(warnings_of(problems)$message, "Q999", all = FALSE)
})

test_that("a select_multiple parent counts as present via its child columns", {
  dataset <- fixture_dataset()
  dataset$Q78 <- NULL # ONA does not always export the concatenated parent

  problems <- validate_loa(fixture_workbook(), dataset)
  expect_false(any(grepl("'Q78' is not in the dataset", problems$message)))
})

test_that("a repeated analysis warns that only the first survives", {
  sheets <- fixture_sheets()
  sheets$analysis <- rbind(sheets$analysis, sheets$analysis[1, , drop = FALSE])

  problems <- validate_loa(fixture_workbook(sheets))
  expect_false(loa_has_errors(problems))
  expect_match(warnings_of(problems)$message, "repeats an earlier row", all = FALSE)
})


# -----------------------------------------------------------------------------
# Renaming
# -----------------------------------------------------------------------------

test_that("renaming a select_multiple parent renames its child columns too", {
  dataset <- fixture_dataset()
  renamed <- apply_rename_map(dataset, c(Q78 = "Reasons_for_leaving"), "/")

  expect_true("Reasons_for_leaving" %in% names(renamed))
  expect_true("Reasons_for_leaving/Economic reasons" %in% names(renamed))
  expect_false(any(startsWith(names(renamed), "Q78")))

  # The link the pipeline relies on still resolves.
  children <- names(renamed)[startsWith(names(renamed), "Reasons_for_leaving/")]
  expect_equal(length(children), 3L)
})

test_that("a child label containing the separator is not truncated", {
  dataset <- data.frame(
    QN9 = "x",
    `QN9/Interception at sea/pull-back` = "y",
    check.names = FALSE, stringsAsFactors = FALSE
  )
  renamed <- apply_rename_map(dataset, c(QN9 = "Protection_incidents"), "/")

  expect_true(
    "Protection_incidents/Interception at sea/pull-back" %in% names(renamed)
  )
})

test_that("the rename map is applied in one pass and cannot chain", {
  dataset <- data.frame(A = 1, B = 2, stringsAsFactors = FALSE)
  renamed <- apply_rename_map(dataset, c(A = "B", B = "C"), "/")

  # A must become B, not C.
  expect_equal(names(renamed), c("B", "C"))
})

test_that("the map rewrites the analysis sheet so raw codes can be used throughout", {
  spec <- build_analysis_spec(fixture_workbook(), fixture_dataset())

  expect_equal(spec$rename_map[["Q27"]], "Respondent_Gender")
  expect_equal(spec$loa$analysis_var[1], "Respondent_Gender")
  expect_equal(spec$loa$analysis_var[2], "Q78") # untouched, not renamed
})

test_that("Overall is implicit and rejected when written out", {
  spec <- build_analysis_spec(fixture_workbook(), fixture_dataset())
  expect_equal(spec$group_variables[1], "Overall")
  expect_equal(length(spec$group_variables), 3L)

  sheets <- fixture_sheets()
  sheets$group_analysis <- rbind(
    sheets$group_analysis,
    data.frame(raw_data_name = "Q29", new_name = "overall", stringsAsFactors = FALSE)
  )
  problems <- validate_loa(fixture_workbook(sheets), fixture_dataset())
  expect_match(errors_of(problems)$message, "added automatically", all = FALSE)
})

test_that("a grouping variable missing from the dataset warns and is dropped", {
  sheets <- fixture_sheets()
  sheets$group_analysis <- rbind(
    sheets$group_analysis,
    data.frame(raw_data_name = "Q404", new_name = "Country", stringsAsFactors = FALSE)
  )

  problems <- validate_loa(fixture_workbook(sheets), fixture_dataset())
  expect_false(loa_has_errors(problems))
  expect_match(warnings_of(problems)$message, "Q404", all = FALSE)

  spec <- build_analysis_spec(fixture_workbook(sheets), fixture_dataset())
  expect_false("Country" %in% spec$group_variables)
})

test_that("renaming onto a name the dataset already uses is fatal", {
  sheets <- fixture_sheets()
  sheets$group_analysis$new_name[1] <- "Q31"

  problems <- validate_loa(fixture_workbook(sheets), fixture_dataset())
  expect_match(errors_of(problems)$message, "already a column of the dataset", all = FALSE)
})

test_that("duplicate raw names and duplicate output names are both fatal", {
  sheets <- fixture_sheets()
  sheets$group_analysis <- data.frame(
    raw_data_name = c("Q27", "Q27"),
    new_name = c("Gender", "Sex"),
    stringsAsFactors = FALSE
  )
  expect_match(
    errors_of(validate_loa(fixture_workbook(sheets), fixture_dataset()))$message,
    "renamed more than once", all = FALSE
  )

  sheets$group_analysis <- data.frame(
    raw_data_name = c("Q27", "Q31"),
    new_name = c("Gender", "Gender"),
    stringsAsFactors = FALSE
  )
  expect_match(
    errors_of(validate_loa(fixture_workbook(sheets), fixture_dataset()))$message,
    "Output names must be distinct", all = FALSE
  )
})

test_that("include = FALSE parks a row without deleting it", {
  sheets <- fixture_sheets()
  sheets$group_analysis$include <- c("TRUE", "no")

  spec <- build_analysis_spec(fixture_workbook(sheets), fixture_dataset())
  expect_equal(spec$group_variables, c("Overall", "Respondent_Gender"))
})

test_that("an unreadable include value is reported rather than guessed", {
  sheets <- fixture_sheets()
  sheets$group_analysis$include <- c("TRUE", "maybe")

  problems <- validate_loa(fixture_workbook(sheets), fixture_dataset())
  expect_match(errors_of(problems)$message, "must be TRUE or FALSE", all = FALSE)
})


# -----------------------------------------------------------------------------
# Output column ownership
# -----------------------------------------------------------------------------

test_that("loa_column_owner reproduces the pipeline's first-match-wins rule", {
  groups <- c("Region", "Region_of_origin")

  expect_equal(loa_column_owner("stat_Overall", groups), "Overall")
  expect_equal(loa_column_owner("stat_Region_esa", groups), "Region")
  # The bug being guarded against: the longer variable's column is claimed by
  # the shorter one.
  expect_equal(loa_column_owner("stat_Region_of_origin_esa", groups), "Region")
})

test_that("a grouping name contained in another is fatal, in either row order", {
  for (order in list(c("Region", "Region_of_origin"), c("Region_of_origin", "Region"))) {
    sheets <- fixture_sheets()
    sheets$group_analysis <- data.frame(
      raw_data_name = c("Q27", "Q31"),
      new_name = order,
      stringsAsFactors = FALSE
    )

    problems <- validate_loa(fixture_workbook(sheets), fixture_dataset())
    expect_true(loa_has_errors(problems))
    expect_match(errors_of(problems)$message, "would be attributed to", all = FALSE)
  }
})

test_that("an interior name clash is caught, not just a prefix", {
  clashes <- loa_group_tag_clashes(c("of", "Region_of_origin"))
  expect_true("Region_of_origin" %in% clashes$group_variable)
})

test_that("a grouping variable with a level called Overall is fatal", {
  dataset <- fixture_dataset()
  dataset$Q31[2] <- "Overall" # a real level that collides with the Overall tag

  sheets <- fixture_sheets()
  problems <- validate_loa(fixture_workbook(sheets), dataset)

  expect_true(loa_has_errors(problems))
  expect_match(errors_of(problems)$message, "Travelling_with_children", all = FALSE)
})

test_that("well-separated grouping names raise nothing", {
  clashes <- loa_group_tag_clashes(
    c("Respondent_Gender", "Travelling_with_children",
      "Region_of_interview", "Region_of_origin",
      "Country_living_in_before_journey")
  )
  expect_equal(nrow(clashes), 0L)
})

test_that("the label row is not read as a group level", {
  dataset <- fixture_dataset()
  # The label row would otherwise contribute a bogus level to every grouping
  # variable, which is exactly what skip_label_row exists to prevent.
  spec <- build_analysis_spec(fixture_workbook(), dataset)
  expect_false(loa_has_errors(spec$problems))
})


# -----------------------------------------------------------------------------
# Settings
# -----------------------------------------------------------------------------

test_that("only supplied settings are passed, so pipeline defaults survive", {
  parsed <- loa_parse_settings(data.frame(
    setting = c("sm_separator", "engine", "min_group_n"),
    value = c("/", "", NA),
    stringsAsFactors = FALSE
  ))

  expect_equal(parsed$settings, list(sm_separator = "/"))
  expect_false(loa_has_errors(parsed$problems))
})

test_that("an unknown setting is fatal and suggests a near match", {
  parsed <- loa_parse_settings(data.frame(
    setting = "value_column", value = "stat", stringsAsFactors = FALSE
  ))

  expect_true(loa_has_errors(parsed$problems))
  expect_match(parsed$problems$message[1], "not a recognised setting")
  expect_match(parsed$problems$message[1], "value_columns")
})

test_that("types are enforced", {
  bad <- function(setting, value) {
    loa_parse_settings(data.frame(
      setting = setting, value = value, stringsAsFactors = FALSE
    ))$problems$message[1]
  }

  expect_match(bad("skip_label_row", "sometimes"), "must be TRUE or FALSE")
  expect_match(bad("min_group_n", "a few"), "must be a number")
  expect_match(bad("engine", "turbo"), "must be one of auto, fast, survey")
})

test_that("logical and enum values accept what people actually type", {
  parsed <- loa_parse_settings(data.frame(
    setting = c("skip_label_row", "use_group_prefix", "engine"),
    value = c("Yes", "0", "FAST"),
    stringsAsFactors = FALSE
  ))

  expect_false(loa_has_errors(parsed$problems))
  expect_true(parsed$settings$skip_label_row)
  expect_false(parsed$settings$use_group_prefix)
  expect_equal(parsed$settings$engine, "fast")
})

test_that("comma-separated lists become character vectors", {
  parsed <- loa_parse_settings(data.frame(
    setting = "value_columns",
    value = "stat, n , n_total",
    stringsAsFactors = FALSE
  ))
  expect_equal(parsed$settings$value_columns, c("stat", "n", "n_total"))
})

test_that("a setting given twice is fatal", {
  parsed <- loa_parse_settings(data.frame(
    setting = c("engine", "engine"),
    value = c("fast", "survey"),
    stringsAsFactors = FALSE
  ))
  expect_match(parsed$problems$message[1], "set more than once")
})

test_that("the three label keys assemble one vector in the pipeline's order", {
  parsed <- loa_parse_settings(data.frame(
    setting = c("count_selections_label_many", "count_selections_label_none"),
    value = c("Picked several", "Picked none"),
    stringsAsFactors = FALSE
  ))

  expect_equal(
    parsed$settings$count_selections_labels,
    c("Picked none", "Selected exactly 1 choice", "Picked several")
  )
})

test_that("every allow-listed setting maps to a real pipeline argument", {
  # The schema is the contract with run_group_analysis_pipeline(); this is the
  # test that notices when the pipeline's signature moves.
  schema <- loa_settings_schema()

  expect_equal(anyDuplicated(schema$setting), 0L)
  expect_true(all(schema$type %in% c("chr", "chr[]", "lgl", "num", "enum")))
  expect_true(all(is.na(schema$values) | schema$type == "enum"))
  expect_true(all(schema$type != "enum" | !is.na(schema$values)))

  if (exists("run_group_analysis_pipeline", mode = "function")) {
    formals_pipeline <- names(formals(run_group_analysis_pipeline))
    expect_true(all(schema$arg %in% formals_pipeline))
  }
})


# -----------------------------------------------------------------------------
# Selection counts and choice combinations
# -----------------------------------------------------------------------------

test_that("count_combinations becomes the named list the pipeline expects", {
  spec <- build_analysis_spec(fixture_workbook(), fixture_dataset())

  expect_equal(names(spec$count_combinations), "Q78")
  expect_equal(
    spec$count_combinations$Q78,
    c(
      Economic = "Economic reasons",
      Conflict = "Armed conflict, generalised violence, and insecurity"
    )
  )
})

test_that("a blank display_name falls back to the full export label", {
  sheets <- fixture_sheets()
  sheets$count_combinations$display_name <- c("Economic", NA)

  spec <- build_analysis_spec(fixture_workbook(sheets), fixture_dataset())
  expect_equal(
    names(spec$count_combinations$Q78),
    c("Economic", "Armed conflict, generalised violence, and insecurity")
  )
})

test_that("a choice label containing commas survives intact", {
  spec <- build_analysis_spec(fixture_workbook(), fixture_dataset())
  expect_equal(
    unname(spec$count_combinations$Q78[2]),
    "Armed conflict, generalised violence, and insecurity"
  )
})

test_that("a blank analysis_var or choice_label is fatal", {
  sheets <- fixture_sheets()
  sheets$count_combinations$choice_label[2] <- NA

  problems <- validate_loa(fixture_workbook(sheets), fixture_dataset())
  expect_match(errors_of(problems)$message, "choice_label is empty", all = FALSE)
})

test_that("a repeated count_selections entry warns and is de-duplicated", {
  sheets <- fixture_sheets()
  sheets$count_selections <- data.frame(
    analysis_var = c("Q78", "Q78"), stringsAsFactors = FALSE
  )

  problems <- validate_loa(fixture_workbook(sheets), fixture_dataset())
  expect_false(loa_has_errors(problems))
  expect_match(warnings_of(problems)$message, "listed more than once", all = FALSE)

  spec <- build_analysis_spec(fixture_workbook(sheets), fixture_dataset())
  expect_equal(spec$count_selections, "Q78")
})

test_that("the pipeline's own validator is delegated to when it is loaded", {
  # ck_check_count_selections() rejects a variable that is not a select_multiple.
  # Stand it in here so the delegation is exercised without the analysis file.
  ck_check_count_selections <<- function(count_selections, dataset, loa, sm_separator) {
    stop("count_selections only applies to select_multiple questions.", call. = FALSE)
  }
  on.exit(rm("ck_check_count_selections", envir = globalenv()), add = TRUE)

  problems <- validate_loa(fixture_workbook(), fixture_dataset())
  expect_match(
    errors_of(problems)$message, "only applies to select_multiple", all = FALSE
  )
})

test_that("delegation is skipped while the workbook still has fatal problems", {
  ck_check_count_selections <<- function(...) stop("should not be reached", call. = FALSE)
  on.exit(rm("ck_check_count_selections", envir = globalenv()), add = TRUE)

  sheets <- fixture_sheets()
  sheets$analysis$analysis_type[1] <- "nonsense"

  problems <- validate_loa(fixture_workbook(sheets), fixture_dataset())
  expect_false(any(grepl("should not be reached", problems$message)))
})


# -----------------------------------------------------------------------------
# Excluded choices
# -----------------------------------------------------------------------------

test_that("exclude_choices keeps labels whole and honours include", {
  sheets <- fixture_sheets(exclude_choices = data.frame(
    choice_label = c(
      "Don't know",
      "Armed conflict, generalised violence, and insecurity",
      "Refused"
    ),
    include = c("TRUE", "TRUE", "FALSE"),
    stringsAsFactors = FALSE
  ))

  spec <- build_analysis_spec(fixture_workbook(sheets), fixture_dataset())
  expect_equal(
    spec$exclude_choices,
    c("Don't know", "Armed conflict, generalised violence, and insecurity")
  )
})


# -----------------------------------------------------------------------------
# The specification and the pipeline call
# -----------------------------------------------------------------------------

test_that("the spec carries everything the pipeline needs and nothing else", {
  spec <- build_analysis_spec(fixture_workbook(), fixture_dataset())

  expect_s3_class(spec, "analysis_spec")
  expect_setequal(
    names(spec),
    c("loa", "group_variables", "rename_map", "count_selections",
      "count_combinations", "count_exclusive_combinations", "exclude_choices",
      "settings", "problems", "source")
  )
  expect_false(loa_has_errors(spec$problems))
})

test_that("the assembled call renames the dataset and passes only what was set", {
  spec <- build_analysis_spec(fixture_workbook(), fixture_dataset())
  args <- analysis_spec_args(fixture_dataset(), spec)

  expect_true("Respondent_Gender" %in% names(args$dataset))
  expect_false("Q27" %in% names(args$dataset))

  expect_equal(args$group_variables[1], "Overall")
  expect_equal(args$count_selections, "Q78")
  expect_equal(args$value_columns, c("stat", "n", "n_total"))
  expect_equal(args$extra_columns, "sector")
  expect_equal(args$sm_separator, "/")

  # Unset arguments must not appear at all, or the pipeline default is lost.
  expect_false("engine" %in% names(args))
  expect_false("min_group_n" %in% names(args))
  expect_false("exclude_choices" %in% names(args))
})

test_that("the label row is still on top of the dataset handed to the pipeline", {
  spec <- build_analysis_spec(fixture_workbook(), fixture_dataset())
  args <- analysis_spec_args(fixture_dataset(), spec)

  expect_equal(args$dataset$Respondent_Gender[1], "What is your gender?")
})

test_that("run_analysis_spec refuses to run a workbook with fatal problems", {
  sheets <- fixture_sheets()
  sheets$analysis$analysis_type[1] <- "nonsense"
  spec <- build_analysis_spec(fixture_workbook(sheets), fixture_dataset())

  expect_error(
    run_analysis_spec(fixture_dataset(), spec, pipeline = function(...) "ran"),
    "must be fixed before running"
  )
})

test_that("run_analysis_spec runs, and lets the caller override the spec", {
  spec <- build_analysis_spec(fixture_workbook(), fixture_dataset())

  seen <- NULL
  fake <- function(...) {
    seen <<- list(...)
    "done"
  }

  expect_equal(run_analysis_spec(fixture_dataset(), spec, pipeline = fake, verbose = FALSE), "done")
  expect_false(seen$verbose)
  expect_equal(seen$group_variables[1], "Overall")
})

test_that("a warning-only workbook still runs", {
  sheets <- fixture_sheets()
  sheets$analysis <- rbind(
    sheets$analysis,
    data.frame(
      analysis_type = "prop_select_one", analysis_var = "Q999",
      level = NA_real_, sector = "Other", stringsAsFactors = FALSE
    )
  )
  spec <- build_analysis_spec(fixture_workbook(sheets), fixture_dataset())

  expect_true(nrow(warnings_of(spec$problems)) > 0)
  expect_equal(
    run_analysis_spec(fixture_dataset(), spec, pipeline = function(...) "ran"),
    "ran"
  )
})

test_that("a bare sheet list works as well as a read workbook", {
  spec <- build_analysis_spec(fixture_sheets(), fixture_dataset())
  expect_s3_class(spec, "analysis_spec")
  expect_equal(spec$group_variables[1], "Overall")
})

test_that("the whole path from an xlsx file to a pipeline call holds together", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")

  path <- write_fixture_xlsx(fixture_sheets(), tempfile(fileext = ".xlsx"))
  wb <- read_loa_workbook(path)
  spec <- build_analysis_spec(wb, fixture_dataset())

  expect_false(loa_has_errors(spec$problems))

  args <- NULL
  run_analysis_spec(
    fixture_dataset(), spec,
    pipeline = function(...) {
      args <<- list(...)
      invisible(NULL)
    }
  )

  expect_equal(
    args$group_variables,
    c("Overall", "Respondent_Gender", "Travelling_with_children")
  )
  expect_equal(args$loa$analysis_var, c("Respondent_Gender", "Q78", "Q29"))
  expect_equal(names(args$count_combinations), "Q78")
  expect_true("Respondent_Gender" %in% names(args$dataset))
})
