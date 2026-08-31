# count_exclusive_combinations: the strict reading of a choice combination.
#
# The behaviour that matters is not that it runs, but that it runs on a
# different base - so that is what these assert.

test_that("the sheet is recognised and becomes the named list the pipeline wants", {
  spec <- build_analysis_spec(fixture_workbook(), fixture_dataset())

  expect_equal(names(spec$count_exclusive_combinations), "Q78")
  expect_equal(
    spec$count_exclusive_combinations$Q78,
    c(
      Economic = "Economic reasons",
      Conflict = "Armed conflict, generalised violence, and insecurity"
    )
  )
})

test_that("it is passed to the pipeline, and only when it is asked for", {
  spec <- build_analysis_spec(fixture_workbook(), fixture_dataset())
  args <- analysis_spec_args(fixture_dataset(), spec)
  expect_equal(names(args$count_exclusive_combinations), "Q78")

  # Absent sheet, absent argument: the pipeline's own default must survive.
  bare <- build_analysis_spec(
    fixture_workbook(fixture_sheets(count_exclusive_combinations = NULL)),
    fixture_dataset()
  )
  expect_equal(length(bare$count_exclusive_combinations), 0L)
  expect_false(
    "count_exclusive_combinations" %in%
      names(analysis_spec_args(fixture_dataset(), bare))
  )
})

test_that("the derived analysis_type is rejected in the analysis sheet, with a pointer", {
  sheets <- fixture_sheets()
  sheets$analysis$analysis_type[2] <- "exclusive_combination_select_multiple"

  problems <- validate_loa(fixture_workbook(sheets), fixture_dataset())
  message <- problems$message[problems$severity == "error"][1]

  expect_match(message, "produced by the pipeline")
  expect_match(message, "count_exclusive_combinations sheet")
})

test_that("a missing variable on the sheet is fatal, like the other combination sheet", {
  # ck_check_choice_combinations() stops the run, so a warning followed by an
  # abort would be worse than saying so up front.
  sheets <- fixture_sheets()
  sheets$count_exclusive_combinations$analysis_var <- c("Q404", "Q404")

  problems <- validate_loa(fixture_workbook(sheets), fixture_dataset())
  hit <- problems[grepl("Q404", problems$message), , drop = FALSE]

  expect_true(nrow(hit) > 0)
  expect_true(all(hit$severity == "error"))
  expect_match(hit$message, "exclusive choice combinations", all = FALSE)
})

test_that("blank cells are reported against the right sheet", {
  sheets <- fixture_sheets()
  sheets$count_exclusive_combinations$choice_label[2] <- NA

  problems <- validate_loa(fixture_workbook(sheets), fixture_dataset())
  hit <- problems[problems$sheet == "count_exclusive_combinations", , drop = FALSE]

  expect_true(nrow(hit) > 0)
  expect_match(hit$message, "choice_label is empty", all = FALSE)
})

test_that("the pipeline's validator names the sheet the user wrote in", {
  skip_if_not(
    exists("ck_check_choice_combinations", mode = "function"),
    "analysis functions are not in the repository yet"
  )
  skip_if_not(
    "arg_name" %in% names(formals(ck_check_choice_combinations)),
    "this build of the pipeline has no arg_name"
  )

  # A mistyped label in the exclusive sheet must not produce a message about
  # count_combinations, or the user goes looking in the wrong place.
  sheets <- fixture_sheets()
  sheets$count_exclusive_combinations$choice_label[1] <- "Ecomonic reasons"

  problems <- validate_loa(fixture_workbook(sheets), fixture_dataset())
  hit <- problems[problems$severity == "error", , drop = FALSE]

  expect_true(any(grepl("count_exclusive_combinations", hit$message)))
  expect_false(any(grepl("^count_combinations is not usable", hit$message)))
})

test_that("the three settings that are not shared reach the pipeline", {
  sheets <- fixture_sheets()
  sheets$settings <- rbind(
    sheets$settings,
    data.frame(
      setting = c(
        "count_exclusive_combinations_heading",
        "count_exclusive_combinations_suffix",
        "count_exclusive_combinations_none_label"
      ),
      value = c("Picked only these", '" alone"', "Something else too"),
      stringsAsFactors = FALSE
    )
  )

  spec <- build_analysis_spec(fixture_workbook(sheets), fixture_dataset())
  args <- analysis_spec_args(fixture_dataset(), spec)

  expect_equal(args$count_exclusive_combinations_heading, "Picked only these")
  expect_equal(args$count_exclusive_combinations_none_label, "Something else too")
  # The leading space is the point: without it the row reads "Economicalone".
  expect_equal(args$count_exclusive_combinations_suffix, " alone")
})

test_that("a quoted setting keeps its edge whitespace, an unquoted one does not", {
  parsed <- loa_parse_settings(data.frame(
    setting = c("count_combinations_joiner", "count_combinations_heading"),
    value = c('" + "', "  Combination  "),
    stringsAsFactors = FALSE
  ))

  expect_equal(parsed$settings$count_combinations_joiner, " + ")
  expect_equal(parsed$settings$count_combinations_heading, "Combination")
})

test_that("the exclusive block reports on a smaller base than the ordinary one", {
  skip_if_not(
    exists("run_group_analysis_pipeline", mode = "function"),
    "analysis functions are not in the repository yet"
  )

  # A respondent who picked Economic AND an unlisted choice is in the ordinary
  # combination base and outside the exclusive one. This dataset has some.
  dataset <- fixture_dataset(n = 60)
  body <- seq_len(nrow(dataset))[-1]
  mixed <- body[seq(2, length(body), by = 6)]
  dataset[["Q78/Economic reasons"]][mixed] <- "Economic reasons"
  dataset[["Q78/Lack of services"]][mixed] <- "Lack of services"

  spec <- build_analysis_spec(fixture_workbook(), dataset)
  results <- suppressMessages(run_analysis_spec(dataset, spec, verbose = FALSE))

  long <- results$results_long
  overall <- long[is.na(long$group_var), , drop = FALSE]

  ordinary <- overall[overall$analysis_type == "combination_select_multiple", ]
  exclusive <- overall[overall$analysis_type == "exclusive_combination_select_multiple", ]

  expect_true(nrow(ordinary) > 0)
  expect_true(nrow(exclusive) > 0)

  # The property worth protecting: strictly fewer respondents, not merely
  # different rows.
  expect_lt(max(exclusive$n_total), max(ordinary$n_total))

  # Each set is still mutually exclusive and exhaustive on its own base.
  expect_equal(sum(exclusive$stat), 1, tolerance = 1e-8)
  expect_equal(sum(ordinary$stat), 1, tolerance = 1e-8)

  # And the pipeline counts who was dropped, which is what the app reports.
  expect_true(results$exclusive_combinations$n_mixed_dropped > 0)
})

test_that("the caveat names the question and the share, and is silent when nothing was dropped", {
  map <- data.frame(
    analysis_var = "Q78", n_in_base = 800, n_mixed_dropped = 200,
    stringsAsFactors = FALSE
  )
  note <- ak_exclusive_base_note(map)

  expect_length(note, 1L)
  expect_match(note, "Q78")
  expect_match(note, "200")
  expect_match(note, "20.0%")
  expect_match(note, "outside this base")

  map$n_mixed_dropped <- 0
  expect_length(ak_exclusive_base_note(map), 0L)
  expect_length(ak_exclusive_base_note(NULL), 0L)
  expect_length(ak_exclusive_base_note(map[0, ]), 0L)
})

test_that("the caveat panel renders only when there is something to say", {
  quiet <- ak_exclusive_base_panel(
    data.frame(analysis_var = "Q78", n_in_base = 10, n_mixed_dropped = 0)
  )
  expect_equal(trimws(paste(as.character(quiet), collapse = "")), "")

  loud <- as.character(ak_exclusive_base_panel(
    data.frame(analysis_var = "Q78", n_in_base = 800, n_mixed_dropped = 200)
  ))
  expect_match(loud, "status-warning", all = FALSE)
  expect_match(loud, "smaller denominator", all = FALSE)
  expect_match(loud, "Footnote this", all = FALSE)
})
