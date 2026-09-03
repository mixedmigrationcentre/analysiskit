# The readme sheet's sample-composition table.
#
# The table lists every disaggregation group in one column-pair, so without a
# break it runs as a single wall of rows and the eye cannot tell where one
# disaggregation variable ends and the next begins. A blank row between blocks
# is the separator - and it has to be genuinely blank, not a styled row with no
# text in it, which still reads as part of the table.

test_that("ck_value_runs splits a vector into consecutive runs", {
  expect_equal(ck_value_runs(c("a", "a", "b", "b", "b", "c")), list(1:2, 3:5, 6L))
  expect_equal(ck_value_runs("a"), list(1L))
  expect_equal(ck_value_runs(character(0)), list())
})


test_that("ck_value_runs treats NA as a value rather than a break", {
  # rle() starts a new run at every NA, which would scatter blank rows through
  # the table wherever a grouping variable was unlabelled.
  expect_equal(ck_value_runs(c(NA, NA, "b")), list(1:2, 3L))
  expect_equal(ck_value_runs(c("a", NA, NA, "b")), list(1L, 2:3, 4L))
})


test_that("a variable appearing in two stretches is not stitched together", {
  # Consecutive runs, not a split by unique value: the composition table is in
  # block order, and merging separated stretches would misreport that order.
  expect_equal(ck_value_runs(c("a", "b", "a")), list(1L, 2L, 3L))
})


# Builds a workbook with two disaggregation variables and reads the readme
# sheet's XML back, because the question is about which rows exist and what is
# on them - something no in-memory check can answer.
readme_fixture <- function() {
  wide <- data.frame(
    analysis_var = rep(c("Q27", "Q31"), each = 2),
    analysis_var_value = c("Female", "Male", "Yes", "No"),
    analysis_type = "prop_select_one",
    stat_Overall = c(.6, .4, .4, .6),
    n_Overall = c(600, 400, 400, 600),
    n_total_Overall = 1000,
    stat_Gender_Female = c(1, 0, .5, .5),
    n_Gender_Female = c(668, 0, 334, 334),
    n_total_Gender_Female = 668,
    stat_Gender_Male = c(0, 1, .3, .7),
    n_Gender_Male = c(0, 332, 100, 232),
    n_total_Gender_Male = 332,
    stat_Region_North = c(.7, .3, .4, .6),
    n_Region_North = c(350, 150, 200, 300),
    n_total_Region_North = 500,
    stringsAsFactors = FALSE
  )
  column_map <- data.frame(
    column = c(
      "stat_Overall", "stat_Gender_Female", "stat_Gender_Male",
      "stat_Region_North"
    ),
    group_variable = c("Overall", "Gender", "Gender", "Region"),
    stringsAsFactors = FALSE
  )
  list(wide = wide, column_map = column_map)
}

write_readme_workbook <- function() {
  fx <- readme_fixture()
  path <- tempfile(fileext = ".xlsx")
  format_my_xlsx_variable_x_group(
    fx$wide,
    file_path = path,
    layout = "blocks",
    value_columns = "stat",
    total_columns = c("n", "n_total"),
    split_by = "none",
    column_map = fx$column_map,
    verbose = FALSE
  )
  path
}

# Row numbers that actually carry cells on the first sheet (the readme).
readme_occupied_rows <- function(path) {
  dir <- withr::local_tempdir()
  utils::unzip(path, exdir = dir)
  xml <- paste(
    readLines(file.path(dir, "xl", "worksheets", "sheet1.xml"), warn = FALSE),
    collapse = ""
  )
  as.integer(sub('.*r="([0-9]+)".*', "\\1", regmatches(
    xml, gregexpr('<row r="[0-9]+"', xml)
  )[[1]]))
}


test_that("the composition table leaves a blank row between disaggregations", {
  skip_if_not_installed("withr")

  path <- write_readme_workbook()
  rows <- readme_occupied_rows(path)

  contents <- openxlsx::read.xlsx(
    path, sheet = "readme", colNames = FALSE, skipEmptyRows = FALSE
  )
  first_col <- as.character(contents[[1]])

  gender <- which(first_col == "Gender")
  region <- which(first_col == "Region")
  expect_gt(length(gender), 0)
  expect_gt(length(region), 0)

  # Exactly one row between the last Gender row and the first Region row...
  gap <- seq(max(gender) + 1L, min(region) - 1L)
  expect_length(gap, 1)
  expect_true(all(is.na(unlist(contents[gap, ]))))

  # ...and it is a row that does not exist in the sheet at all: no text, and no
  # style either, so the table's fills and borders stop and restart around it.
  #
  # read.xlsx() indexes from the first non-empty row, not from row 1, so the
  # two coordinate systems have to be lined up before they can be compared.
  offset <- min(rows) - 1L
  expect_false((gap + offset) %in% rows)
})


test_that("the blank row is not doubled up before the next section", {
  skip_if_not_installed("withr")

  contents <- openxlsx::read.xlsx(
    write_readme_workbook(), sheet = "readme",
    colNames = FALSE, skipEmptyRows = FALSE
  )
  first_col <- as.character(contents[[1]])

  last_group <- max(which(first_col == "Region"))
  next_band <- min(which(grepl("^Sheets in this workbook", first_col)))

  # One gap, as before this change - the section band brings its own spacing
  # and a second blank row would open a hole.
  expect_equal(next_band - last_group, 2L)
})
