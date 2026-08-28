# The startup package check. Every test here runs with install = FALSE: a test
# suite must never install anything into the library it is running in.

requirement <- function(package, need = "required", source = "CRAN",
                        repo = NA_character_, purpose = "testing") {
  data.frame(
    package = package, need = need, source = source,
    repo = repo, purpose = purpose, stringsAsFactors = FALSE
  )
}


test_that("the requirements name a package, a need, a source and a reason", {
  requirements <- ak_package_requirements()

  expect_setequal(
    names(requirements), c("package", "need", "source", "repo", "purpose")
  )
  expect_equal(anyDuplicated(requirements$package), 0L)
  expect_true(all(requirements$need %in% c("required", "optional")))
  expect_true(all(requirements$source %in% c("CRAN", "GitHub")))
  # Every purpose is shown to the user in the failure message, so none may be
  # blank.
  expect_true(all(nzchar(requirements$purpose)))
  # A GitHub package without a repo cannot produce an install command.
  github <- requirements$source == "GitHub"
  expect_true(all(!is.na(requirements$repo[github])))
  expect_true(all(is.na(requirements$repo[!github])))
})

test_that("the requirements cover everything the code actually calls", {
  requirements <- ak_package_requirements()

  # These are called by name in R/ and functions/. A package used but not
  # declared would fail at run time instead of at startup, which is the whole
  # point of the check.
  expect_true(
    all(
      c("shiny", "readxl", "shinyFiles", "dplyr", "tidyr", "stringr", "openxlsx")
        %in% requirements$package[requirements$need == "required"]
    )
  )
  # The survey engine and recreate_sm_parents are reached only when a workbook
  # asks for them, so none of these may be able to block startup.
  expect_setequal(
    requirements$package[requirements$need == "optional"],
    c("srvyr", "analysistools", "cleaningtools")
  )
})

test_that("install commands match where the package comes from", {
  expect_equal(
    ak_install_command(requirement("dplyr")),
    "install.packages('dplyr')"
  )
  expect_equal(
    ak_install_command(
      requirement("analysistools", source = "GitHub", repo = "impact-initiatives/analysistools")
    ),
    "remotes::install_github('impact-initiatives/analysistools')"
  )
})

test_that("an installed package is reported as available and left alone", {
  report <- ak_ensure_packages(requirement("stats"), install = FALSE, quiet = TRUE)

  expect_equal(report$status, "available")
  expect_equal(report$action, "none")
})

test_that("a missing required package stops with the command to fix it", {
  expect_error(
    ak_ensure_packages(
      requirement("notarealpackage", purpose = "nothing at all"),
      install = FALSE, quiet = TRUE
    ),
    "cannot start without"
  )

  err <- tryCatch(
    ak_ensure_packages(
      requirement("notarealpackage", purpose = "nothing at all"),
      install = FALSE, quiet = TRUE
    ),
    error = function(e) conditionMessage(e)
  )
  # The message has to carry the reason and the remedy, or it just tells the
  # user something is wrong without saying what to do.
  expect_match(err, "nothing at all")
  expect_match(err, "install.packages\\('notarealpackage'\\)")
})

test_that("a missing optional package is reported but never blocks startup", {
  report <- expect_no_error(
    ak_ensure_packages(
      requirement("notarealpackage", need = "optional"),
      install = FALSE, quiet = TRUE
    )
  )

  expect_equal(report$status, "missing")
  expect_equal(report$action, "install.packages('notarealpackage')")
})

test_that("a GitHub package is never installed implicitly, only reported", {
  # install = TRUE, and still nothing is installed: a GitHub install compiles a
  # dependency tree, which is more than a startup check should decide to do.
  report <- ak_ensure_packages(
    requirement(
      "notarealpackage", need = "optional", source = "GitHub",
      repo = "someone/notarealpackage"
    ),
    install = TRUE, quiet = TRUE
  )

  expect_equal(report$status, "missing")
  expect_equal(report$action, "remotes::install_github('someone/notarealpackage')")
})

test_that("the report covers every requirement, in order", {
  requirements <- rbind(
    requirement("stats"),
    requirement("notarealpackage", need = "optional")
  )
  report <- ak_ensure_packages(requirements, install = FALSE, quiet = TRUE)

  expect_equal(report$package, requirements$package)
  expect_equal(report$need, requirements$need)
  expect_setequal(names(report), c("package", "need", "status", "action"))
})

test_that("the check is quiet when it is asked to be", {
  expect_silent(
    ak_ensure_packages(
      requirement("notarealpackage", need = "optional"),
      install = FALSE, quiet = TRUE
    )
  )
  expect_message(
    ak_ensure_packages(
      requirement("notarealpackage", need = "optional"),
      install = FALSE, quiet = FALSE
    ),
    "Optional package"
  )
})

test_that("a CRAN mirror is set before any install is attempted", {
  # install.packages() in a non-interactive session with repos unset fails with
  # a mirror-selection error that says nothing about the real problem.
  old <- getOption("repos")
  on.exit(options(repos = old), add = TRUE)

  options(repos = c(CRAN = "@CRAN@"))
  ak_ensure_cran_mirror()
  expect_false(getOption("repos")[["CRAN"]] == "@CRAN@")
  expect_match(getOption("repos")[["CRAN"]], "^https://")

  # An existing choice is respected rather than replaced.
  options(repos = c(CRAN = "https://example.org/cran"))
  ak_ensure_cran_mirror()
  expect_equal(getOption("repos")[["CRAN"]], "https://example.org/cran")
})

test_that("only shiny is attached", {
  # Every other call in this project is namespace-qualified, so attaching more
  # would mask base functions - stats::filter and stats::lag above all - for no
  # benefit.
  expect_equal(ak_attach_packages(), "shiny")
  expect_true("package:shiny" %in% search())
  expect_false("package:dplyr" %in% search())
})

test_that("the real requirements are satisfied in this environment", {
  # Not a check of the code so much as of the machine: if this fails, the suite
  # itself is running somewhere the app could not start.
  report <- ak_ensure_packages(install = FALSE, quiet = TRUE)
  required <- report[report$need == "required", , drop = FALSE]

  expect_true(
    all(required$status == "available"),
    info = paste(
      "missing:",
      paste(required$package[required$status != "available"], collapse = ", ")
    )
  )
})

test_that("an optional CRAN package is reported, not installed, by default", {
  # The regression this guards: reaching for the network on every launch to
  # install something for a feature the user may never use. It also made the
  # app's startup depend on having a connection.
  report <- ak_ensure_packages(
    requirement("notarealpackage", need = "optional"),
    install = TRUE, quiet = TRUE
  )

  expect_equal(report$status, "missing")
  expect_equal(report$action, "install.packages('notarealpackage')")
})

test_that("the install policy is CRAN only, required by default, optional on request", {
  # Tested as a predicate rather than by attempting a real install: a test suite
  # that reaches for the network is slow, and fails on a machine that is offline
  # for reasons that have nothing to do with the code.
  cran_required <- requirement("dplyr")
  cran_optional <- requirement("srvyr", need = "optional")
  github <- requirement(
    "analysistools", need = "optional",
    source = "GitHub", repo = "impact-initiatives/analysistools"
  )

  expect_true(ak_should_install(cran_required))
  expect_false(ak_should_install(cran_optional))
  expect_true(ak_should_install(cran_optional, install_optional = TRUE))

  # GitHub is never installed automatically, whatever else is asked for.
  expect_false(ak_should_install(github))
  expect_false(ak_should_install(github, install_optional = TRUE))

  # install = FALSE turns the whole thing into a report.
  expect_false(ak_should_install(cran_required, install = FALSE))
})
