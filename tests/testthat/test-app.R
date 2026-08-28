# The wiring itself: app.R driven through shiny::testServer(), which is the one
# place a live reactive session is genuinely needed. Everything these tests
# assert about behaviour - what blocks a run, what only warns - is decided by
# the pure functions tested elsewhere; here we check that the server asks them.

# testthat runs with the working directory set to tests/testthat, but the app
# directory is the project root. Walk up rather than hard-coding "../..", so the
# tests still find it if they are run from somewhere else.
find_app_dir <- function(start = ".", levels = 4L) {
  path <- normalizePath(start, mustWork = FALSE)
  for (i in seq_len(levels)) {
    if (file.exists(file.path(path, "app.R"))) return(path)
    path <- dirname(path)
  }
  NA_character_
}

app_dir <- find_app_dir()

skip_without_app <- function() {
  skip_if_not_installed("shiny")
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")
  skip_if(is.na(app_dir), "app.R not found")
}

# A renderUI output arrives as a list of html plus dependencies, so flatten it
# before matching against the text.
ui_text <- function(x) paste(as.character(unlist(x)), collapse = " ")

# shinyDirButton reports its choice as a root name plus the path components
# below it; this is the shape parseDirPath() reads back.
pick_folder <- function(path) {
  relative <- sub(paste0("^", path.expand("~"), "/?"), "", path)
  list(root = "Home", path = as.list(c("", strsplit(relative, "/", fixed = TRUE)[[1]])))
}

# A fresh, writable folder under Home, so the picker has something real to
# point at and each test gets its own output directory.
#
# Kept inside one dotted folder rather than scattered across Home: the app is
# run locally, so Home is what the folder picker shows its user, and a test run
# should not leave a dozen directories sitting in it.
test_destination_root <- function() {
  file.path(path.expand("~"), ".analysiskit-tests")
}

temp_destination <- function() {
  path <- file.path(
    test_destination_root(), paste0("run_", as.integer(runif(1, 1, 1e9)))
  )
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}

withr::defer(unlink(test_destination_root(), recursive = TRUE), teardown_env())

# fileInput hands the server a one-row data frame, so the fixtures must too.
upload <- function(path, name = basename(path)) {
  data.frame(
    name = name,
    size = file.size(path),
    type = "",
    datapath = path,
    stringsAsFactors = FALSE
  )
}

dataset_file <- function(dataset = fixture_dataset()) {
  path <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(dataset, path)
  path
}

loa_file <- function(sheets = fixture_sheets()) {
  path <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(sheets, path)
  path
}


test_that("the app loads and starts on the dataset step", {
  skip_without_app()

  shiny::testServer(app_dir, {
    expect_equal(step_states()[["dataset"]], "active")
    expect_equal(step_states()[["loa"]], "todo")
    expect_false(ak_can_run(step_states()))
    expect_match(ui_text(output$status), "Upload a CSV or XLSX dataset")
  })
})

test_that("a dataset upload is read, profiled and advances the tracker", {
  skip_without_app()

  shiny::testServer(app_dir, {
    session$setInputs(dataset = upload(dataset_file(), "4mi_export.xlsx"))

    expect_true(dataset_ok())
    expect_equal(step_states()[["dataset"]], "done")
    expect_equal(step_states()[["loa"]], "active")

    # The column profile and preview were dead code until now; assert they
    # actually render rather than trusting the UI wiring.
    expect_match(output$dataset_overview, "4mi_export.xlsx")
    expect_match(output$dataset_columns, "Q27")
    expect_match(output$dataset_preview, "Female")

    expect_match(ui_text(output$status), "Upload a List of Analysis")
  })
})

test_that("an unreadable dataset is reported without stopping the app", {
  skip_without_app()

  path <- tempfile(fileext = ".txt")
  writeLines("not a dataset", path)

  shiny::testServer(app_dir, {
    session$setInputs(dataset = upload(path, "notes.txt"))

    expect_false(dataset_ok())
    expect_equal(step_states()[["dataset"]], "error")
    expect_match(ui_text(output$status), "CSV or XLSX")
  })
})

test_that("the checks run as soon as both files are in, without a button", {
  skip_without_app()

  shiny::testServer(app_dir, {
    session$setInputs(dataset = upload(dataset_file()))
    session$setInputs(loa = upload(loa_file()))

    expect_true(loa_ok())
    expect_false(loa_has_errors(loa_problems()))
    expect_equal(step_states()[["checks"]], "done")
    expect_match(ui_text(output$status), "Every check passed")

    # The checks pass, but there is nowhere to save yet.
    expect_equal(step_states()[["destination"]], "active")
    expect_false(ak_can_run(step_states()))

    session$setInputs(folder = pick_folder(temp_destination()))
    expect_equal(step_states()[["destination"]], "done")
    expect_true(ak_can_run(step_states()))
  })
})

test_that("the variable check compares the workbook against the dataset just read", {
  skip_without_app()

  sheets <- fixture_sheets()
  sheets$analysis <- rbind(
    sheets$analysis,
    data.frame(
      analysis_type = "prop_select_one", analysis_var = "Q999",
      level = NA_real_, sector = "Other", stringsAsFactors = FALSE
    )
  )

  shiny::testServer(app_dir, {
    session$setInputs(dataset = upload(dataset_file()))
    session$setInputs(loa = upload(loa_file(sheets)))

    coverage <- variable_coverage()
    expect_true("Q999" %in% coverage$variable)
    expect_false(coverage$present[coverage$variable == "Q999"])

    expect_match(output$coverage_table, "Not in dataset")
    expect_match(ui_text(output$coverage_note), "not in")

    # A missing analysis variable costs a row, not correctness, so the run is
    # still offered.
    expect_equal(step_states()[["checks"]], "warning")
    session$setInputs(folder = pick_folder(temp_destination()))
    expect_true(ak_can_run(step_states()))
  })
})

test_that("the check reruns when a different dataset is uploaded", {
  skip_without_app()

  # The same workbook against a dataset from another round: the point of
  # running the check on both inputs rather than once at upload.
  other <- fixture_dataset()
  names(other) <- paste0("R2_", names(other))

  shiny::testServer(app_dir, {
    session$setInputs(dataset = upload(dataset_file()))
    session$setInputs(loa = upload(loa_file()))
    session$setInputs(folder = pick_folder(temp_destination()))
    expect_true(ak_can_run(step_states()))

    session$setInputs(dataset = upload(dataset_file(other)))

    expect_true(loa_has_errors(loa_problems()))
    expect_equal(step_states()[["checks"]], "error")
    expect_false(ak_can_run(step_states()))
    expect_match(ui_text(output$status), "must be fixed before running")
  })
})

test_that("a workbook with a fatal problem blocks the run and says where", {
  skip_without_app()

  sheets <- fixture_sheets()
  sheets$analysis$analysis_type[1] <- "prop_select_mutliple"

  shiny::testServer(app_dir, {
    session$setInputs(dataset = upload(dataset_file()))
    session$setInputs(loa = upload(loa_file(sheets)))

    expect_true(loa_has_errors(loa_problems()))
    expect_false(ak_can_run(step_states()))
    expect_match(output$loa_problems, "Must fix")
    expect_match(ui_text(output$check_badges), "problem to fix")
  })
})

test_that("an unreadable List of Analysis is reported, not thrown", {
  skip_without_app()

  path <- tempfile(fileext = ".docx")
  writeLines("x", path)

  shiny::testServer(app_dir, {
    session$setInputs(dataset = upload(dataset_file()))
    session$setInputs(loa = upload(path, "loa.docx"))

    expect_false(loa_ok())
    expect_equal(step_states()[["loa"]], "error")
    expect_match(ui_text(output$status), "CSV or XLSX")
  })
})

test_that("the sheet summary reflects what the workbook actually supplied", {
  skip_without_app()

  shiny::testServer(app_dir, {
    session$setInputs(dataset = upload(dataset_file()))
    session$setInputs(loa = upload(loa_file()))

    html <- output$loa_sheets
    expect_match(html, "exclude_choices")
    expect_match(html, "Not supplied - defaults apply")
  })
})

test_that("the run button reports honestly when the pipeline is absent", {
  skip_without_app()
  skip_if(
    exists("run_group_analysis_pipeline", mode = "function"),
    "the analysis functions are installed, so there is nothing to report"
  )

  shiny::testServer(app_dir, {
    session$setInputs(dataset = upload(dataset_file()))
    session$setInputs(loa = upload(loa_file()))
    expect_match(ui_text(output$run_hint), "functions/ folder")
    # The button is not offered when it cannot succeed.
    expect_match(ui_text(output$run_control), "disabled")

    session$setInputs(run = 1)

    # It must not claim to have run, and it must not crash the session.
    expect_null(analysis_results())
    expect_match(run_error(), "not available")
    expect_match(ui_text(output$results_status), "not available")
  })
})

test_that("a blocked workbook never reaches the pipeline at all", {
  skip_without_app()

  sheets <- fixture_sheets()
  sheets$analysis$analysis_type[1] <- "nonsense"

  shiny::testServer(app_dir, {
    session$setInputs(dataset = upload(dataset_file()))
    session$setInputs(loa = upload(loa_file(sheets)))
    session$setInputs(run = 1)

    expect_null(analysis_results())
    expect_false(ak_can_run(step_states()))
  })
})

test_that("uploading a file does not start an analysis by itself", {
  skip_without_app()

  destination <- temp_destination()

  shiny::testServer(app_dir, {
    session$setInputs(dataset = upload(dataset_file()))
    session$setInputs(loa = upload(loa_file()))
    session$setInputs(folder = pick_folder(destination))

    # Everything is valid and every check passes, and still nothing has run and
    # nothing has been written: expensive work, and files on disk, wait for the
    # button.
    expect_true(ak_can_run(step_states()))
    expect_null(analysis_results())
    expect_null(run_error())
    expect_null(saved_path())
    expect_equal(length(list.files(destination)), 0L)
    expect_equal(step_states()[["results"]], "active")
  })
})

test_that("no output is computed against an empty upload", {
  skip_without_app()

  # The regression this guards: reading the upload into a reactiveVal made
  # uploaded_dataset() return NULL rather than suspending, so every dataset
  # output ran against a dataset with no size, rows or columns.
  expect_no_warning(
    shiny::testServer(app_dir, {
      force(output$status)
      force(output$progress_tracker)
      force(output$run_control)
      force(output$run_hint)
      force(output$loa_status)
      force(output$results_status)
    })
  )
})

test_that("the status message reflects the file that was actually read", {
  skip_without_app()

  shiny::testServer(app_dir, {
    session$setInputs(dataset = upload(dataset_file(), "round_9.xlsx"))
    expect_match(ui_text(output$status), "Loaded round_9.xlsx")

    # Replacing the file must replace the message, not append to it.
    session$setInputs(dataset = upload(dataset_file(), "round_10.xlsx"))
    expect_match(ui_text(output$status), "Loaded round_10.xlsx")
    expect_no_match(ui_text(output$status), "round_9")
  })
})

test_that("clicking run produces results and saves a workbook to the chosen folder", {
  skip_without_app()
  skip_if_not(
    exists("run_group_analysis_pipeline", mode = "function"),
    "analysis functions are not in the repository yet"
  )
  skip_if_not_installed("openxlsx")

  destination <- temp_destination()

  shiny::testServer(app_dir, {
    session$setInputs(dataset = upload(dataset_file(fixture_dataset(40)), "round_9.xlsx"))
    session$setInputs(loa = upload(loa_file()))
    session$setInputs(folder = pick_folder(destination))
    expect_true(ak_can_run(step_states()))

    session$setInputs(run = 1)

    expect_null(run_error())
    expect_false(is.null(analysis_results()))
    expect_true(nrow(analysis_results()$combined_results) > 0)

    # The file is the deliverable; the reactive value is only how we found it.
    expect_false(is.null(saved_path()))
    expect_true(file.exists(saved_path()))
    expect_equal(dirname(saved_path()), destination)
    expect_equal(list.files(destination), basename(saved_path()))
    expect_match(basename(saved_path()), "^analysiskit_round_9_.*[.]xlsx$")

    expect_equal(step_states()[["results"]], "done")
    expect_match(ui_text(output$results_status), "Saved as")
    expect_match(output$results_summary, basename(saved_path()), fixed = TRUE)
  })
})

test_that("the run log keeps what the pipeline reported", {
  skip_without_app()
  skip_if_not(
    exists("run_group_analysis_pipeline", mode = "function"),
    "analysis functions are not in the repository yet"
  )

  shiny::testServer(app_dir, {
    session$setInputs(dataset = upload(dataset_file(fixture_dataset(40))))
    session$setInputs(loa = upload(loa_file()))
    session$setInputs(folder = pick_folder(temp_destination()))
    session$setInputs(run = 1)

    log <- run_log()
    expect_true(length(log) > 0)
    # The pipeline's own diagnostics, not our narration - these are what tell an
    # analyst which variables were skipped or which groups were set aside.
    expect_match(log, "ANALYSIS DONE", all = FALSE)
    expect_match(log, "^Saved to ", all = FALSE)
    # The "-->" prefix is console formatting and has no place in the interface.
    expect_false(any(startsWith(log, "-->")))
    expect_match(ui_text(output$run_log), "ANALYSIS DONE")
  })
})

test_that("a run is refused, and nothing is written, when the folder has gone", {
  skip_without_app()
  skip_if_not(
    exists("run_group_analysis_pipeline", mode = "function"),
    "analysis functions are not in the repository yet"
  )

  destination <- temp_destination()

  shiny::testServer(app_dir, {
    session$setInputs(dataset = upload(dataset_file()))
    session$setInputs(loa = upload(loa_file()))
    session$setInputs(folder = pick_folder(destination))

    # Chosen, then removed behind the app's back - the realistic version of a
    # network drive going away between choosing and running.
    unlink(destination, recursive = TRUE)
    session$setInputs(run = 1)

    expect_null(analysis_results())
    expect_null(saved_path())
    expect_match(run_error(), "does not exist")
  })
})

test_that("a blocked workbook writes nothing to the destination", {
  skip_without_app()

  sheets <- fixture_sheets()
  sheets$analysis$analysis_type[1] <- "nonsense"
  destination <- temp_destination()

  shiny::testServer(app_dir, {
    session$setInputs(dataset = upload(dataset_file()))
    session$setInputs(loa = upload(loa_file(sheets)))
    session$setInputs(folder = pick_folder(destination))
    session$setInputs(run = 1)

    expect_null(saved_path())
    expect_equal(length(list.files(destination)), 0L)
  })
})
