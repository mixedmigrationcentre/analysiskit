# Tests of the contract between the LoA workbook and
# run_group_analysis_pipeline().
#
# The analysis functions are not yet in the repository, so the end-to-end test
# skips until they are. The binding test below runs regardless, against a stub
# carrying the pipeline's exact signature: it is what catches a settings key
# that assembles into an argument the pipeline does not have.

pipeline_formals <- function() {
  c(
    "dataset", "loa", "group_variables", "skip_label_row", "label_row",
    "weight_column", "strata_column", "value_columns", "extra_columns",
    "exclude_choices", "exclude_ignore_case", "count_selections",
    "count_selections_mode", "count_selections_labels",
    "count_selections_order", "count_selections_heading",
    "count_selections_spacer", "count_selections_title_suffix",
    "count_combinations", "count_combinations_ignore_case",
    "count_combinations_none_label", "count_combinations_joiner",
    "count_combinations_order", "count_combinations_heading",
    "count_combinations_spacer", "count_combinations_title_suffix",
    "max_combination_choices", "fallback_level", "engine", "min_group_n",
    "slim_design", "keep_missing_groups", "sm_separator", "prepare_sm",
    "sm_child_style", "blank_to_na", "label_choices",
    "add_analysis_type_label", "analysis_type_labels", "recreate_sm_parents",
    "drop_empty_prop_rows", "summary_value_label", "missing_group_label",
    "use_group_prefix", "lonely_psu", "verbose"
  )
}

# A function with the pipeline's signature and no body worth speaking of.
pipeline_stub <- function(record) {
  args <- pipeline_formals()
  f <- function() NULL
  formals(f) <- stats::setNames(rep(list(quote(expr = )), length(args)), args)
  body(f) <- quote({
    record(as.list(environment()))
    invisible(NULL)
  })
  environment(f) <- list2env(list(record = record), parent = globalenv())
  f
}


test_that("a fully populated workbook binds to the pipeline's real signature", {
  # Every allow-listed setting at once: if any of them assembles into an
  # argument run_group_analysis_pipeline() does not accept, do.call() fails
  # here with "unused argument" rather than in front of a user.
  schema <- loa_settings_schema()

  value_for <- function(setting, type, values) {
    switch(
      type,
      chr = if (setting == "weight_column") "weight" else "x",
      `chr[]` = "stat,n,n_total",
      lgl = "TRUE",
      num = switch(
        setting,
        # Blanket "1" would set max_combination_choices to 1, which the real
        # ck_check_choice_combinations() rightly rejects for a two-choice
        # question. Each numeric setting gets a value that is valid for it.
        fallback_level = "0.95",
        max_combination_choices = "6",
        "1"
      ),
      enum = strsplit(values, ",", fixed = TRUE)[[1]][1]
    )
  }

  settings <- data.frame(
    setting = schema$setting,
    value = mapply(value_for, schema$setting, schema$type, schema$values),
    stringsAsFactors = FALSE
  )

  sheets <- fixture_sheets(
    settings = settings,
    exclude_choices = data.frame(
      choice_label = "Don't know", stringsAsFactors = FALSE
    )
  )
  # sm_separator must stay "/" or the fixture's child columns stop resolving.
  settings$value[settings$setting == "sm_separator"] <- "/"
  sheets$settings <- settings

  spec <- build_analysis_spec(fixture_workbook(sheets), fixture_dataset())
  expect_false(loa_has_errors(spec$problems))

  seen <- NULL
  expect_silent(
    run_analysis_spec(
      fixture_dataset(), spec,
      pipeline = pipeline_stub(function(a) seen <<- a),
      verbose = FALSE
    )
  )

  expect_true(all(names(seen) %in% pipeline_formals()))
  expect_equal(seen$count_selections, "Q78")
  expect_equal(seen$exclude_choices, "Don't know")
  # All three label keys were set, so all three are the supplied value rather
  # than the pipeline default, and they arrive as one length-three vector.
  expect_equal(seen$count_selections_labels, rep("x", 3L))
})

test_that("the settings allow-list covers the pipeline's signature exactly", {
  # 46 formals: 6 come from sheets, 3 are deliberately not settable, and the
  # rest are the settings sheet. A pipeline argument in none of those three
  # groups is one the workbook silently cannot reach.
  from_sheets <- c(
    "dataset", "loa", "group_variables",
    "count_selections", "count_combinations", "exclude_choices"
  )
  not_settable <- c("label_row", "analysis_type_labels", "verbose")

  covered <- unique(c(from_sheets, not_settable, loa_settings_schema()$arg))

  expect_setequal(pipeline_formals(), covered)
})

test_that("the transcribed signature matches the real one when it is loaded", {
  skip_if_not(
    exists("run_group_analysis_pipeline", mode = "function"),
    "analysis functions are not in the repository yet"
  )
  expect_equal(names(formals(run_group_analysis_pipeline)), pipeline_formals())
})


test_that("a workbook runs end to end against the real pipeline", {
  skip_if_not(
    exists("run_group_analysis_pipeline", mode = "function"),
    "analysis functions are not in the repository yet"
  )
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  skip_if_not_installed("stringr")

  dataset <- fixture_dataset(n = 40)
  spec <- build_analysis_spec(fixture_workbook(), dataset)
  expect_false(loa_has_errors(spec$problems))

  results <- suppressMessages(
    run_analysis_spec(dataset, spec, verbose = FALSE)
  )

  expect_true(is.data.frame(results$combined_results))
  expect_true(nrow(results$combined_results) > 0)

  # The disaggregation reads by its output name, not the raw question code.
  expect_true(any(grepl("Respondent_Gender", names(results$combined_results))))
  expect_false(any(grepl("_Q27_", names(results$combined_results))))

  # Every statistic column is attributed to the grouping variable it belongs
  # to - the column_map check the validator guards.
  map <- results$column_map
  expect_true(all(map$group_variable %in% spec$group_variables))
  for (i in seq_len(nrow(map))) {
    expect_equal(
      loa_column_owner(map$column[i], spec$group_variables),
      map$group_variable[i]
    )
  }

  # Both derived analyses appear, under the question they came from.
  expect_true("count_select_multiple" %in% results$combined_results$analysis_type)
  expect_true("combination_select_multiple" %in% results$combined_results$analysis_type)
  expect_equal(results$selection_counts$analysis_var, "Q78")
  expect_equal(results$choice_combinations$analysis_var, "Q78")
})
