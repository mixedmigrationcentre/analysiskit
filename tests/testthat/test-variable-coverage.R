# The check that runs the moment both files are in: every dataset variable the
# workbook names, matched against the dataset the app just read.

test_that("every variable the workbook names is reported once per reference", {
  coverage <- loa_variable_coverage(fixture_workbook(), fixture_dataset())

  expect_setequal(unique(coverage$variable), c("Q27", "Q78", "Q29", "Q31"))
  expect_true(all(coverage$present))

  # Q27 is both analysed and used as a disaggregation, so it appears twice.
  expect_setequal(
    coverage$role[coverage$variable == "Q27"],
    c("analysis_var", "group_var")
  )
  # Q78 is analysed, counted, combined and combined exclusively, so it earns a
  # reference for each.
  expect_setequal(
    coverage$role[coverage$variable == "Q78"],
    c(
      "analysis_var", "count_selections",
      "count_combinations", "count_exclusive_combinations"
    )
  )
  # 1 analysis + 1 count + 2 combination rows + 2 exclusive rows.
  expect_equal(sum(coverage$variable == "Q78"), 6L)
})

test_that("a select_multiple parent counts as present via its children", {
  dataset <- fixture_dataset()
  dataset$Q78 <- NULL # ONA often omits the concatenated parent column

  coverage <- loa_variable_coverage(fixture_workbook(), dataset)
  expect_true(all(coverage$present[coverage$variable == "Q78"]))
})

test_that("a missing analysis variable warns and names the consequence", {
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

  hit <- problems[grepl("Q999", problems$message), , drop = FALSE]
  expect_equal(nrow(hit), 1L)
  expect_equal(hit$severity, "warning")
  expect_equal(hit$sheet, "analysis")
  expect_equal(hit$row, 5L) # header is row 1, so the fourth data row is row 5
  expect_match(hit$message, "this analysis will be skipped")
})

test_that("a missing count_selections variable is fatal, not a warning", {
  # ck_check_count_selections() stops the run outright, so warning here and
  # aborting later would be worse than saying so up front.
  sheets <- fixture_sheets()
  sheets$count_selections <- data.frame(
    analysis_var = "Q404", stringsAsFactors = FALSE
  )

  problems <- validate_loa(fixture_workbook(sheets), fixture_dataset())
  hit <- problems[grepl("Q404", problems$message), , drop = FALSE]

  expect_equal(hit$severity, "error")
  expect_match(hit$message, "the run will stop")
})

test_that("a missing count_combinations parent is fatal", {
  sheets <- fixture_sheets()
  sheets$count_combinations$analysis_var <- c("Q404", "Q404")

  problems <- validate_loa(fixture_workbook(sheets), fixture_dataset())
  hit <- problems[grepl("Q404", problems$message), , drop = FALSE]

  expect_true(nrow(hit) > 0)
  expect_true(all(hit$severity == "error"))
})

test_that("a ratio's numerator and denominator are checked, and its analysis_var is not", {
  # The gap this closes: the ratio branch used to be skipped entirely, so a
  # mistyped numerator went unreported until the pipeline dropped the row.
  sheets <- fixture_sheets()
  sheets$analysis <- data.frame(
    analysis_type = "ratio",
    analysis_var = NA_character_,
    analysis_var_numerator = "Q29",
    analysis_var_denominator = "Q404",
    stringsAsFactors = FALSE
  )

  coverage <- loa_variable_coverage(fixture_workbook(sheets), fixture_dataset())
  from_analysis <- coverage[coverage$sheet == "analysis", , drop = FALSE]

  expect_setequal(from_analysis$role, c("ratio_numerator", "ratio_denominator"))
  expect_true(coverage$present[coverage$variable == "Q29"])
  expect_false(coverage$present[coverage$variable == "Q404"])

  problems <- validate_loa(fixture_workbook(sheets), fixture_dataset())
  expect_match(
    problems$message[grepl("Q404", problems$message)],
    "this ratio will be skipped"
  )
})

test_that("weight and strata columns from the settings sheet are checked", {
  sheets <- fixture_sheets()
  sheets$settings <- rbind(
    sheets$settings,
    data.frame(
      setting = c("weight_column", "strata_column"),
      value = c("Q29", "no_such_column"),
      stringsAsFactors = FALSE
    )
  )

  coverage <- loa_variable_coverage(fixture_workbook(sheets), fixture_dataset())
  expect_true(coverage$present[coverage$role == "weight_column"])
  expect_false(coverage$present[coverage$role == "strata_column"])

  problems <- validate_loa(fixture_workbook(sheets), fixture_dataset())
  hit <- problems[grepl("no_such_column", problems$message), , drop = FALSE]
  expect_equal(hit$severity, "warning")
  expect_match(hit$message, "without strata")
})

test_that("an excluded group_analysis row is not checked for presence", {
  sheets <- fixture_sheets()
  sheets$group_analysis <- rbind(
    sheets$group_analysis,
    data.frame(
      raw_data_name = "Q404", new_name = "Country", stringsAsFactors = FALSE
    )
  )
  sheets$group_analysis$include <- c("TRUE", "TRUE", "FALSE")

  coverage <- loa_variable_coverage(fixture_workbook(sheets), fixture_dataset())
  expect_false("Q404" %in% coverage$variable)
})

test_that("a dataset that shares nothing with the workbook is fatal", {
  # The realistic mistake: last round's List of Analysis against this round's
  # export. Warning per row would bury it; ck_stack_loa() stops anyway.
  dataset <- fixture_dataset()
  names(dataset) <- paste0("R2_", names(dataset))

  problems <- validate_loa(fixture_workbook(), dataset)
  expect_true(loa_has_errors(problems))
  expect_match(
    problems$message, "belong to the same", all = FALSE
  )
})

test_that("one missing variable among many does not trigger the whole-file error", {
  sheets <- fixture_sheets()
  sheets$analysis <- rbind(
    sheets$analysis,
    data.frame(
      analysis_type = "prop_select_one", analysis_var = "Q999",
      level = NA_real_, sector = "Other", stringsAsFactors = FALSE
    )
  )

  problems <- validate_loa(fixture_workbook(sheets), fixture_dataset())
  expect_false(any(grepl("belong to the same", problems$message)))
})

test_that("coverage is empty rather than an error when no dataset is loaded", {
  expect_equal(nrow(loa_variable_coverage(fixture_workbook(), NULL)), 0L)
  expect_false(loa_has_errors(validate_loa(fixture_workbook())))
})

test_that("the coverage summary lists each variable once, missing first", {
  sheets <- fixture_sheets()
  sheets$analysis <- rbind(
    sheets$analysis,
    data.frame(
      analysis_type = "prop_select_one", analysis_var = "Q999",
      level = NA_real_, sector = "Other", stringsAsFactors = FALSE
    )
  )

  summary <- loa_coverage_summary(
    loa_variable_coverage(fixture_workbook(sheets), fixture_dataset())
  )

  expect_equal(anyDuplicated(summary$Variable), 0L)
  expect_equal(summary$Variable[1], "Q999")
  expect_equal(summary$Status[1], "Not in dataset")
  expect_equal(summary$References[summary$Variable == "Q27"], 2L)
})

test_that("presence is judged before renaming, in the workbook's own names", {
  # The workbook is written in raw codes throughout, so Q27 is what gets
  # checked - never Respondent_Gender, which the dataset does not have yet.
  coverage <- loa_variable_coverage(fixture_workbook(), fixture_dataset())
  expect_true("Q27" %in% coverage$variable)
  expect_false("Respondent_Gender" %in% coverage$variable)
})
