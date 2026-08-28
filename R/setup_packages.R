# =============================================================================
# Package requirements
# =============================================================================
#
# One declared list of what Analysis Kit needs, checked at startup so a missing
# package is a clear message on launch rather than an error halfway through a
# run.
#
# Two deliberate limits on what this does automatically:
#
#   * It installs from CRAN only. analysistools and cleaningtools live on
#     GitHub; pulling those implicitly means compiling a dependency tree the
#     user never asked for, so they are reported with the exact command
#     instead.
#   * It attaches only shiny. Every other call in this project is
#     namespace-qualified (dplyr::bind_rows, openxlsx::addStyle), so the rest
#     need to be installed and loadable, not on the search path. Attaching
#     dplyr would also mask stats::filter and stats::lag for no benefit.
# =============================================================================


#' What Analysis Kit Needs
#'
#' `need = "required"` means the app cannot start without it. `need =
#' "optional"` means one feature needs it: the survey engine, used only when an
#' analysis row sets `level`, and `recreate_sm_parents`.
#'
#' @return A data frame with `package`, `need`, `source`, `repo` and `purpose`.
#' @export
ak_package_requirements <- function() {
  pkg <- function(package, need, purpose, source = "CRAN", repo = NA_character_) {
    data.frame(
      package = package, need = need, source = source,
      repo = repo, purpose = purpose, stringsAsFactors = FALSE
    )
  }

  rbind(
    pkg("shiny", "required", "the application itself"),
    pkg("readxl", "required", "reading .xlsx datasets and List of Analysis workbooks"),
    pkg("shinyFiles", "required", "choosing the output folder"),
    pkg("dplyr", "required", "the analysis pipeline"),
    pkg("tidyr", "required", "reshaping results into the wide table"),
    pkg("stringr", "required", "question and choice labels"),
    pkg("openxlsx", "required", "writing the results workbook"),
    pkg(
      "srvyr", "optional",
      "the survey engine, used only when an analysis row sets a confidence level"
    ),
    pkg(
      "analysistools", "optional",
      "the survey engine, used only when an analysis row sets a confidence level",
      source = "GitHub", repo = "impact-initiatives/analysistools"
    ),
    pkg(
      "cleaningtools", "optional",
      "rebuilding select_multiple parent columns, when recreate_sm_parents is TRUE",
      source = "GitHub", repo = "impact-initiatives/cleaningtools"
    )
  )
}


#' Is a Package Installed and Loadable
#'
#' `requireNamespace()` rather than checking the library path: a package can be
#' present but broken, and this is the same test the code itself will make.
#'
#' @param package Package name.
#' @return `TRUE` when the package can be loaded.
#' @keywords internal
ak_package_available <- function(package) {
  requireNamespace(package, quietly = TRUE)
}


#' The Command That Would Install a Package
#'
#' Shown rather than run for GitHub packages, so nobody is surprised by a long
#' compile they did not ask for.
#'
#' @param requirement One row of \code{\link{ak_package_requirements}}.
#' @return A single command string.
#' @export
ak_install_command <- function(requirement) {
  if (identical(requirement$source, "GitHub")) {
    paste0("remotes::install_github('", requirement$repo, "')")
  } else {
    paste0("install.packages('", requirement$package, "')")
  }
}


#' Make Sure a CRAN Mirror Is Set
#'
#' A non-interactive R session often has `repos` unset (`"@CRAN@"`), and
#' `install.packages()` then fails with a mirror-selection error that says
#' nothing about the real problem.
#'
#' @param mirror The mirror to fall back to.
#' @return Invisibly, the mirror in use.
#' @keywords internal
ak_ensure_cran_mirror <- function(mirror = "https://cloud.r-project.org") {
  repos <- getOption("repos")

  if (is.null(repos) || is.na(repos["CRAN"]) || repos["CRAN"] == "@CRAN@") {
    repos["CRAN"] <- mirror
    options(repos = repos)
  }

  invisible(getOption("repos")[["CRAN"]])
}


#' Would This Requirement Be Installed Automatically
#'
#' The whole policy in one predicate, so it can be checked without touching the
#' network or the library: CRAN only, required by default, optional only when
#' asked for.
#'
#' @param requirement One row of \code{\link{ak_package_requirements}}.
#' @param install Logical. Install anything at all.
#' @param install_optional Logical. Extend that to optional packages.
#' @return `TRUE` when this package would be installed if it were missing.
#' @export
ak_should_install <- function(requirement, install = TRUE, install_optional = FALSE) {
  isTRUE(install) &&
    identical(requirement$source, "CRAN") &&
    (identical(requirement$need, "required") || isTRUE(install_optional))
}


#' Install What Is Missing, and Report What Cannot Be Installed
#'
#' Called once at startup. Installs missing CRAN packages, reports missing
#' GitHub ones, and stops only when something the app genuinely cannot run
#' without is still unavailable afterwards.
#'
#' Only **required** CRAN packages are installed. An optional one is reported
#' with its command and left alone: it is needed for a feature the user may
#' never touch, and reaching for the network on every launch to install
#' something nobody asked for is the same imposition as a silent GitHub build.
#' `install_optional = TRUE` opts into it.
#'
#' @param requirements Defaults to \code{\link{ak_package_requirements}}.
#' @param install Logical. Attempt to install missing required CRAN packages.
#'   `FALSE` reports without touching the library, which is what the tests use.
#' @param install_optional Logical. Also install missing optional CRAN
#'   packages. Default `FALSE`.
#' @param quiet Logical. Suppress the progress messages.
#' @return Invisibly, a data frame with `package`, `need`, `status` and
#'   `action`.
#' @export
ak_ensure_packages <- function(requirements = ak_package_requirements(),
                               install = TRUE,
                               install_optional = FALSE,
                               quiet = FALSE) {
  say <- function(...) if (!isTRUE(quiet)) message(...)

  status <- character(nrow(requirements))
  action <- character(nrow(requirements))

  will_install <- function(requirement) {
    ak_should_install(requirement, install, install_optional)
  }

  wanted <- vapply(
    seq_len(nrow(requirements)),
    function(i) {
      !ak_package_available(requirements$package[i]) &&
        will_install(requirements[i, , drop = FALSE])
    },
    logical(1)
  )
  if (any(wanted)) {
    ak_ensure_cran_mirror()
  }

  for (i in seq_len(nrow(requirements))) {
    requirement <- requirements[i, , drop = FALSE]
    package <- requirement$package

    if (ak_package_available(package)) {
      status[i] <- "available"
      action[i] <- "none"
      next
    }

    if (identical(requirement$source, "GitHub")) {
      # Never installed implicitly: a GitHub install compiles a dependency tree
      # and needs remotes, which is a bigger favour than anyone asked for.
      status[i] <- "missing"
      action[i] <- ak_install_command(requirement)
      next
    }

    if (!will_install(requirement)) {
      status[i] <- "missing"
      action[i] <- ak_install_command(requirement)
      next
    }

    say("Installing '", package, "' (", requirement$purpose, ")...")
    try(
      utils::install.packages(package, quiet = isTRUE(quiet)),
      silent = TRUE
    )

    if (ak_package_available(package)) {
      status[i] <- "installed"
      action[i] <- "none"
      say("  '", package, "' installed.")
    } else {
      status[i] <- "missing"
      action[i] <- ak_install_command(requirement)
    }
  }

  report <- data.frame(
    package = requirements$package,
    need = requirements$need,
    status = status,
    action = action,
    stringsAsFactors = FALSE
  )

  missing_required <- report$status == "missing" & report$need == "required"
  missing_optional <- report$status == "missing" & report$need == "optional"

  if (any(missing_optional)) {
    say(
      "Optional package(s) not installed. Analysis Kit runs without them; the ",
      "features that need them will say so if they are used:\n  ",
      paste(
        paste0(
          report$package[missing_optional], ": ", report$action[missing_optional]
        ),
        collapse = "\n  "
      )
    )
  }

  if (any(missing_required)) {
    stop(
      paste0(
        "Analysis Kit cannot start without these package(s):\n  ",
        paste(
          paste0(
            report$package[missing_required], " - ",
            requirements$purpose[missing_required], "\n    ",
            report$action[missing_required]
          ),
          collapse = "\n  "
        ),
        "\nInstall them and start the application again."
      ),
      call. = FALSE
    )
  }

  invisible(report)
}


#' Attach the Packages That Must Be on the Search Path
#'
#' Only shiny. Everything else in this project is called namespace-qualified,
#' so it needs to be installed rather than attached, and attaching more would
#' mask base functions for no gain.
#'
#' @return Invisibly, the packages attached.
#' @export
ak_attach_packages <- function() {
  attached <- "shiny"
  for (package in attached) {
    library(package, character.only = TRUE)
  }
  invisible(attached)
}
