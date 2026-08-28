# Writing a completed run to a folder. The formatter is injectable, so most of
# this runs without building a real workbook; the last test builds one.

fake_results <- function(spec = NULL, extra = character(0)) {
  wide <- data.frame(
    analysis_type = c("prop_select_one", "count_select_multiple"),
    analysis_var = c("Respondent_Gender", "Q78"),
    analysis_var_value = c("Female", "Selected exactly 1 choice"),
    stat_Overall = c(0.5, 0.4),
    n_Overall = c(20, 16),
    n_total_Overall = c(40, 40),
    stringsAsFactors = FALSE
  )
  for (nm in extra) wide[[nm]] <- "Demographics"

  list(
    combined_results = wide,
    column_map = data.frame(
      column = "stat_Overall", group_variable = "Overall",
      stringsAsFactors = FALSE
    )
  )
}


test_that("the output filename records the dataset and the moment", {
  when <- as.POSIXct("2026-08-28 09:41:07", tz = "UTC")

  expect_equal(
    ak_output_filename("4Mi round 9.xlsx", when),
    "analysiskit_4Mi_round_9_20260828-094107.xlsx"
  )
  # A path separator is stripped by basename(), and anything else awkward is
  # replaced rather than carried into the filename.
  expect_equal(
    ak_output_filename("a/b:c*d.csv", when),
    "analysiskit_b_c_d_20260828-094107.xlsx"
  )
  expect_false(grepl("/", ak_output_filename("a/b.csv", when), fixed = TRUE))
  expect_match(ak_output_filename("", when), "^analysiskit_results_")
})

test_that("an existing file is never overwritten", {
  folder <- tempfile()
  dir.create(folder)
  file.create(file.path(folder, "out.xlsx"))

  expect_equal(basename(ak_unique_path(folder, "out.xlsx")), "out_1.xlsx")

  file.create(file.path(folder, "out_1.xlsx"))
  expect_equal(basename(ak_unique_path(folder, "out.xlsx")), "out_2.xlsx")
  expect_equal(basename(ak_unique_path(folder, "fresh.xlsx")), "fresh.xlsx")
})

test_that("an unusable folder is described rather than discovered at write time", {
  expect_match(ak_check_folder(NULL), "No destination folder")
  expect_match(ak_check_folder(""), "No destination folder")
  expect_match(ak_check_folder("/no/such/folder"), "does not exist")

  folder <- tempfile()
  dir.create(folder)
  expect_null(ak_check_folder(folder))

  Sys.chmod(folder, "500")
  # Tested empirically rather than by username: root, and some filesystems,
  # ignore the mode bits entirely.
  skip_if(
    file.access(folder, mode = 2) == 0,
    "this user can write to the folder regardless of its mode"
  )
  expect_match(ak_check_folder(folder), "cannot be written to")
})

test_that("export settings split the pipeline's value columns into stats and counts", {
  spec <- build_analysis_spec(fixture_workbook(), fixture_dataset())
  settings <- ak_export_settings(fake_results(), spec)

  expect_equal(settings$layout, "blocks")
  expect_equal(settings$value_columns, "stat")
  expect_equal(settings$total_columns, c("n", "n_total"))

  # The two must together account for every column in a block, or the formatter
  # cuts the blocks in the wrong place.
  pipeline_columns <- spec$settings$value_columns
  expect_setequal(
    c(settings$value_columns, settings$total_columns), pipeline_columns
  )
})

test_that("confidence interval columns are treated as statistics, not counts", {
  sheets <- fixture_sheets()
  sheets$settings$value[sheets$settings$setting == "value_columns"] <-
    "stat,stat_low,stat_upp,n,n_total"

  spec <- build_analysis_spec(fixture_workbook(sheets), fixture_dataset())
  settings <- ak_export_settings(fake_results(), spec)

  expect_equal(settings$value_columns, c("stat", "stat_low", "stat_upp"))
  expect_equal(settings$total_columns, c("n", "n_total"))
})

test_that("a workbook asking only for counts is refused with a usable message", {
  sheets <- fixture_sheets()
  sheets$settings$value[sheets$settings$setting == "value_columns"] <- "n,n_total"
  spec <- build_analysis_spec(fixture_workbook(sheets), fixture_dataset())

  expect_error(ak_export_settings(fake_results(), spec), "no statistic to report")
})

test_that("sheets are split by sector, and only when the table has one", {
  spec <- build_analysis_spec(fixture_workbook(), fixture_dataset())

  expect_equal(ak_export_settings(fake_results(extra = "sector"), spec)$split_by, "sector")
  # Splitting on a column the table does not have aborts the whole export, so
  # the absence has to fall back rather than be assumed.
  expect_equal(ak_export_settings(fake_results(), spec)$split_by, "none")
})

test_that("the blocks layout drops the separator rows and the row_type column", {
  wide <- fake_results()$combined_results
  wide$row_type <- c("data", "data")
  wide <- rbind(wide, wide[1, ])
  wide$row_type[3] <- "heading"
  wide$analysis_var_value[3] <- "Select multiple count"

  prepared <- ak_prepare_for_export(wide, "blocks")

  expect_false("row_type" %in% names(prepared))
  expect_equal(nrow(prepared), 2L)
  expect_false("Select multiple count" %in% prepared$analysis_var_value)
})

test_that("the matrix layout keeps the separator rows and moves row_type to the front", {
  wide <- fake_results()$combined_results
  wide$row_type <- c("data", "spacer")

  prepared <- ak_prepare_for_export(wide, "matrix")

  expect_equal(nrow(prepared), 2L)
  # It must sit before the first statistic column, or the formatter reads it as
  # one and reports an unrecognised column.
  expect_lt(
    which(names(prepared) == "row_type"),
    which(startsWith(names(prepared), "stat_"))[1]
  )
})

test_that("a table with no row_type column is passed through untouched", {
  wide <- fake_results()$combined_results
  expect_equal(ak_prepare_for_export(wide, "blocks"), wide)
  expect_equal(ak_prepare_for_export(wide, "matrix"), wide)
})

test_that("provenance records what produced the file", {
  spec <- build_analysis_spec(fixture_workbook(), fixture_dataset())
  lines <- ak_provenance(spec, "4mi_round9.xlsx", as.POSIXct("2026-08-28 09:41:07", tz = "UTC"))

  expect_match(lines, "4mi_round9.xlsx", all = FALSE)
  expect_match(lines, "loa.xlsx", all = FALSE)
  expect_match(lines, "Respondent_Gender", all = FALSE)
  expect_match(lines, "2026-08-28", all = FALSE)
})

test_that("export refuses a bad folder before calling the formatter", {
  spec <- build_analysis_spec(fixture_workbook(), fixture_dataset())
  called <- FALSE

  expect_error(
    ak_export_results(
      fake_results(), spec, "/no/such/folder", "d.xlsx",
      formatter = function(...) called <<- TRUE
    ),
    "does not exist"
  )
  expect_false(called)
})

test_that("export hands the formatter a path in the chosen folder", {
  spec <- build_analysis_spec(fixture_workbook(), fixture_dataset())
  folder <- tempfile()
  dir.create(folder)

  seen <- NULL
  path <- ak_export_results(
    fake_results(extra = "sector"), spec, folder, "4mi_round9.xlsx",
    formatter = function(...) {
      seen <<- list(...)
      file.create(seen$file_path)
    }
  )

  expect_equal(dirname(path), folder)
  expect_match(basename(path), "^analysiskit_4mi_round9_")
  expect_equal(seen$file_path, path)
  expect_false(seen$overwrite)
  expect_equal(seen$layout, "blocks")
  expect_match(seen$readme_text, "Produced by Analysis Kit", all = FALSE)
})

test_that("export fails loudly if the formatter writes nothing", {
  # Silently reporting a path that holds no file would be the worst outcome:
  # the user goes looking for a workbook that was never written.
  spec <- build_analysis_spec(fixture_workbook(), fixture_dataset())
  folder <- tempfile()
  dir.create(folder)

  expect_error(
    ak_export_results(
      fake_results(), spec, folder, "d.xlsx",
      formatter = function(...) invisible(NULL)
    ),
    "no file appeared"
  )
})

test_that("a real run produces a real workbook", {
  skip_if_not(
    exists("run_group_analysis_pipeline", mode = "function"),
    "analysis functions are not in the repository yet"
  )
  skip_if_not(
    exists("format_my_xlsx_variable_x_group", mode = "function"),
    "export functions are not in the repository yet"
  )
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("readxl")

  folder <- tempfile()
  dir.create(folder)

  dataset <- fixture_dataset(n = 40)
  spec <- build_analysis_spec(fixture_workbook(), dataset)
  results <- suppressMessages(run_analysis_spec(dataset, spec, verbose = FALSE))

  path <- expect_no_warning(
    suppressMessages(
      ak_export_results(results, spec, folder, "4mi_round9.xlsx")
    )
  )

  expect_true(file.exists(path))
  expect_gt(file.size(path), 5000)

  # One sheet per sector, plus the readme - and no stray sheet for the rows the
  # separator markers left without a sector.
  sheets <- readxl::excel_sheets(path)
  expect_setequal(sheets, c("readme", "Demographics", "Drivers"))

  expect_equal(length(list.files(folder)), 1L)
})
