# The deployment bundle. Nothing here writes a manifest: these tests are about
# what would be sent to a server, which is worth being sure of before it is.

app_root <- function() {
  path <- normalizePath(".", mustWork = FALSE)
  for (i in 1:4) {
    if (file.exists(file.path(path, "app.R"))) return(path)
    path <- dirname(path)
  }
  NA_character_
}


test_that("the bundle carries everything the app needs at run time", {
  skip_if(is.na(app_root()), "app.R not found")
  files <- ak_manifest_files(app_root())

  expect_true("app.R" %in% files)
  expect_true(any(startsWith(files, "R/")))
  expect_true(any(startsWith(files, "functions/")))
  expect_true("www/styles.css" %in% files)
  # A deployed user has no access to the repository, so the template has to
  # travel with the app or the download link points at nothing.
  expect_true("docs/loa_template.xlsx" %in% files)

  expect_true(all(file.exists(file.path(app_root(), files))))
})

test_that("the bundle carries nothing that would leak data or waste a build", {
  skip_if(is.na(app_root()), "app.R not found")
  files <- ak_manifest_files(app_root())

  # .RData is an R session's saved workspace. If a dataset was loaded when it
  # was written, deploying it uploads respondent data to a hosted server.
  expect_false(any(grepl("^[.]RData$|[.]Rhistory$", basename(files))))
  expect_false(any(startsWith(files, "tests/")))
  expect_false(any(startsWith(files, ".git")))
  expect_false(any(startsWith(files, ".Rproj.user")))
  # Bundling the generator would record rsconnect as a dependency of the app.
  expect_false("R/generate_manifest.R" %in% files)
})

test_that("the bundle is a whitelist, so anything new is left out until named", {
  root <- tempfile()
  dir.create(file.path(root, "R"), recursive = TRUE)
  file.create(file.path(root, "app.R"))
  file.create(file.path(root, "R", "thing.R"))
  file.create(file.path(root, "R", "generate_manifest.R"))
  file.create(file.path(root, "secrets.csv"))
  file.create(file.path(root, ".RData"))

  files <- ak_manifest_files(root)

  expect_setequal(files, c("app.R", "R/thing.R"))
})

test_that("hard dependencies are read, and Suggests is not", {
  # testthat Suggests a good deal and Imports much less; if Suggests leaked in,
  # the pre-flight would demand packages nothing actually needs.
  deps <- ak_hard_dependencies("testthat")

  expect_true("cli" %in% deps)
  expect_false("R" %in% deps)
  expect_false("base" %in% deps)
  expect_type(deps, "character")

  expect_equal(ak_hard_dependencies("notarealpackage"), character(0))
})

test_that("a missing package is reported against whatever needs it", {
  problems <- ak_manifest_preflight(packages = "notarealpackage")

  expect_equal(nrow(problems), 1L)
  expect_equal(problems$package, "notarealpackage")
  expect_equal(problems$problem, "not installed")
  expect_equal(problems$needed_by, "the app")
})

test_that("a satisfied set of packages raises nothing", {
  expect_equal(nrow(ak_manifest_preflight(packages = "stats")), 0L)
})

test_that("the fixes distinguish CRAN from GitHub", {
  problems <- data.frame(
    package = c("srvyr", "analysistools", "cleaningtools"),
    problem = "not installed", needed_by = "the app",
    stringsAsFactors = FALSE
  )
  fixes <- ak_manifest_fixes(problems)

  expect_match(fixes, 'install.packages\\(c\\("srvyr"\\)\\)', all = FALSE)
  expect_match(fixes, 'install_github\\("impact-initiatives/analysistools"\\)', all = FALSE)
  expect_match(fixes, 'install_github\\("impact-initiatives/cleaningtools"\\)', all = FALSE)
  # A GitHub package must never appear in an install.packages() call - it is not
  # on CRAN, and the command would fail with a misleading message.
  expect_false(any(grepl('install.packages.*analysistools', fixes)))

  expect_equal(ak_manifest_fixes(problems[0, ]), character(0))
})

test_that("the packages to check come from a real scan of the bundled files", {
  skip_if(is.na(app_root()), "app.R not found")
  skip_if_not_installed("renv")

  packages <- ak_manifest_packages(app_root())

  expect_true(all(c("shiny", "shinyFiles", "openxlsx") %in% packages))
  # The pipeline calls these by name, which is why deploying needs them
  # installed even though a run may never reach them.
  expect_true(all(c("srvyr", "analysistools", "cleaningtools") %in% packages))
  # Nothing from the test suite, which is not in the bundle.
  expect_false("testthat" %in% packages)
})

test_that("nothing in R/ runs a deployment step when the app starts", {
  # shiny::runApp() auto-sources every file in R/ for an app directory. A file
  # there with a side effect runs on every launch - a manifest generator in R/
  # stopped the app before its interface appeared.
  skip_if(is.na(app_root()), "app.R not found")

  for (file in list.files(file.path(app_root(), "R"), pattern = "[.][Rr]$", full.names = TRUE)) {
    top_level <- as.list(parse(file))
    calls <- top_level[!vapply(
      top_level,
      function(e) is.call(e) && identical(as.character(e[[1]]), "<-"),
      logical(1)
    )]

    expect_equal(
      length(calls), 0L,
      info = paste0(
        basename(file), " runs something at top level. Files in R/ may only ",
        "define objects; a runnable script belongs in the project root."
      )
    )
  }
})

test_that("the runnable script is in the project root and generates nothing on load", {
  skip_if(is.na(app_root()), "app.R not found")

  script <- file.path(app_root(), "generate_manifest.R")
  expect_true(file.exists(script))
  expect_match(paste(readLines(script), collapse = "\n"), "ak_write_manifest()")

  # It is deployment tooling, so it must not travel with the app either.
  files <- ak_manifest_files(app_root())
  expect_false("generate_manifest.R" %in% files)
  expect_false("R/manifest.R" %in% files)
})
