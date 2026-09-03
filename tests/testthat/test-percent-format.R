# Percentages are published to the nearest whole number.
#
# The rounding is a *display* format, not a change to the stored value, and the
# distinction is the point of these tests: a reader sees 67%, while the cell
# still holds 0.668039538714992 so a column of them continues to sum and
# average correctly. A future change that rounded the values on the way in
# would pass a visual check and fail here.

format_fixture <- function() {
  data.frame(
    analysis_var = c("Q27", "Q27", "Q29"),
    analysis_var_value = c("Female", "Male", NA),
    analysis_type = c("prop_select_one", "prop_select_one", "mean"),
    sector = "Demographics",
    stat_Overall = c(0.668039538714992, 0.331960461285008, 31.4159265),
    n_Overall = c(668, 332, 1000),
    n_total_Overall = c(1000, 1000, 1000),
    stringsAsFactors = FALSE
  )
}

# Reads the number formats back out of the written file rather than trusting
# the arguments that went in. openxlsx writes custom formats into
# xl/styles.xml, so an unwanted two-decimal percentage anywhere in the workbook
# - including the readme sheet, which used to carry its own "0.0%" - shows up
# here.
workbook_formats <- function(path) {
  dir <- withr::local_tempdir()
  utils::unzip(path, exdir = dir)
  xml <- paste(
    readLines(file.path(dir, "xl", "styles.xml"), warn = FALSE),
    collapse = ""
  )
  codes <- regmatches(xml, gregexpr('formatCode="[^"]*"', xml))[[1]]
  unique(sub('^formatCode="(.*)"$', "\\1", codes))
}

write_fixture_workbook <- function(..., layout = "blocks") {
  path <- tempfile(fileext = ".xlsx")
  format_my_xlsx_variable_x_group(
    format_fixture(),
    file_path = path,
    layout = layout,
    value_columns = "stat",
    total_columns = c("n", "n_total"),
    split_by = "sector",
    verbose = FALSE,
    ...
  )
  path
}


test_that("ck_percent_format builds the Excel format for a digit count", {
  expect_equal(ck_percent_format(0), "0%")
  expect_equal(ck_percent_format(1), "0.0%")
  expect_equal(ck_percent_format(2), "0.00%")
  expect_equal(ck_percent_format(4), "0.0000%")
})


test_that("ck_percent_format refuses a digit count it cannot honour", {
  expect_error(ck_percent_format(-1), "non-negative")
  expect_error(ck_percent_format(c(1, 2)), "single")
  expect_error(ck_percent_format("two"), "non-negative")
})


test_that("the app asks for whole-number percentages", {
  spec <- build_analysis_spec(fixture_workbook(), fixture_dataset())
  results <- list(
    combined_results = format_fixture(),
    column_map = data.frame(
      column = "stat_Overall", group_variable = "Overall",
      stringsAsFactors = FALSE
    )
  )

  expect_equal(ak_export_settings(results, spec)$percent_digits, 0)
})


# The formatter's own defaults have to agree, because each sheet writer carries
# a default of its own. One that disagreed would only surface in whichever
# writer was not passed the value explicitly.
test_that("every layer defaults to whole-number percentages", {
  expect_equal(formals(format_my_xlsx_variable_x_group)$percent_digits, 0)
  expect_equal(eval(formals(ck_write_group_sheet)$pct_fmt), "0%")
  expect_equal(eval(formals(ck_write_block_sheet)$pct_fmt), "0%")
  expect_equal(eval(formals(ck_write_readme_sheet)$pct_fmt), "0%")
})


test_that("a written workbook carries no two-decimal percentage anywhere", {
  skip_if_not_installed("withr")

  for (layout in c("blocks", "matrix")) {
    formats <- workbook_formats(write_fixture_workbook(layout = layout))
    percent <- grep("%", formats, value = TRUE)

    expect_equal(percent, "0%", info = layout)
    expect_false(any(grepl("0.00%", formats, fixed = TRUE)), info = layout)
    expect_false(any(grepl("0.0%", formats, fixed = TRUE)), info = layout)
  }
})


test_that("the displayed rounding does not touch the stored value", {
  skip_if_not_installed("withr")

  path <- write_fixture_workbook()
  cells <- suppressWarnings(as.numeric(unlist(
    openxlsx::read.xlsx(path, sheet = "Demographics", colNames = FALSE)
  )))
  cells <- cells[!is.na(cells)]

  # 67% on screen, every digit still in the cell.
  expect_true(any(abs(cells - 0.668039538714992) < 1e-12))
})


test_that("percent_digits still allows decimals when a run asks for them", {
  skip_if_not_installed("withr")

  formats <- workbook_formats(write_fixture_workbook(percent_digits = 1))
  expect_equal(grep("%", formats, value = TRUE), "0.0%")
})
