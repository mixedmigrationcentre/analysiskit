# Fixtures shared by the List of Analysis tests.
#
# The dataset mimics an MMC 4Mi ONA export: row 1 is the label row, choice
# labels live in the select_multiple child cells, and the grouping variables
# carry raw question codes rather than readable names.

fixture_dataset <- function(n = 6) {
  label_row <- data.frame(
    Q27 = "What is your gender?",
    Q31 = "Are you travelling with children?",
    Q29 = "How old are you?",
    Q78 = "Why did you leave?",
    `Q78/Economic reasons` = "Q78/Economic reasons",
    `Q78/Armed conflict, generalised violence, and insecurity` =
      "Q78/Armed conflict, generalised violence, and insecurity",
    `Q78/Lack of services` = "Q78/Lack of services",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  body <- data.frame(
    Q27 = rep(c("Female", "Male"), length.out = n),
    Q31 = rep(c("Yes", "No"), length.out = n),
    Q29 = as.character(seq(20, length.out = n)),
    Q78 = rep("Economic reasons", n),
    `Q78/Economic reasons` = rep(c("Economic reasons", NA), length.out = n),
    `Q78/Armed conflict, generalised violence, and insecurity` =
      rep(c(NA, "Armed conflict, generalised violence, and insecurity"), length.out = n),
    `Q78/Lack of services` = rep(NA_character_, n),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  out <- rbind(label_row, body)
  rownames(out) <- NULL
  out
}

fixture_sheets <- function(...) {
  sheets <- list(
    analysis = data.frame(
      analysis_type = c("prop_select_one", "prop_select_multiple", "mean"),
      analysis_var = c("Q27", "Q78", "Q29"),
      level = c(NA_real_, NA_real_, NA_real_),
      sector = c("Demographics", "Drivers", "Demographics"),
      stringsAsFactors = FALSE
    ),
    group_analysis = data.frame(
      raw_data_name = c("Q27", "Q31"),
      new_name = c("Respondent_Gender", "Travelling_with_children"),
      stringsAsFactors = FALSE
    ),
    count_selections = data.frame(
      analysis_var = "Q78", stringsAsFactors = FALSE
    ),
    count_combinations = data.frame(
      analysis_var = c("Q78", "Q78"),
      choice_label = c(
        "Economic reasons",
        "Armed conflict, generalised violence, and insecurity"
      ),
      display_name = c("Economic", "Conflict"),
      stringsAsFactors = FALSE
    ),
    settings = data.frame(
      setting = c("sm_separator", "value_columns", "extra_columns"),
      value = c("/", "stat,n,n_total", "sector"),
      stringsAsFactors = FALSE
    )
  )

  # Not modifyList(): it merges data frames column by column, so passing a
  # replacement sheet would splice into the fixture instead of replacing it.
  replacement <- list(...)
  for (nm in names(replacement)) {
    sheets[[nm]] <- replacement[[nm]]
  }
  sheets[!vapply(sheets, is.null, logical(1))]
}

# A workbook as read_loa_workbook() returns it, without touching the disk.
fixture_workbook <- function(sheets = fixture_sheets(), ...) {
  wb <- list(
    sheets = sheets,
    format = "XLSX",
    filename = "loa.xlsx",
    sheet_names = names(sheets),
    unknown_sheets = character(0),
    ignored_sheets = character(0)
  )
  overrides <- list(...)
  for (nm in names(overrides)) {
    wb[[nm]] <- overrides[[nm]]
  }
  wb
}

write_fixture_xlsx <- function(sheets, path) {
  writexl::write_xlsx(sheets, path)
  path
}
