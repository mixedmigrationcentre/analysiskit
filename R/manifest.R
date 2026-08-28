# =============================================================================
# manifest.json for Posit Cloud / Connect / shinyapps.io
# =============================================================================
#
# Functions only. The runnable script is generate_manifest.R in the project
# root, and it lives there for a reason: shiny::runApp() auto-sources every
# file in R/ when it starts an app directory, so anything here with a side
# effect runs on every launch. A manifest generator in R/ took the app down.
#
# Two things this does that a bare rsconnect::writeManifest(appDir = ".") does
# not, both of which matter:
#
# 1. It bundles a named list of files rather than the whole project directory.
#    The repository root holds .RData and .Rhistory - an R session's saved
#    workspace and command history. If a dataset was loaded when that workspace
#    was written, deploying the directory uploads respondent data to a hosted
#    server. It also keeps tests/ out, so a deployment does not install testthat
#    to run nothing.
#
# 2. It says what is missing before renv does. writeManifest() snapshots with
#    renv, and renv refuses to snapshot a library whose packages have missing
#    dependencies. Its message names one package at a time and does not say
#    what to do, so the checks below report the whole list at once with the
#    commands that fix it.
#
# Why the survey packages are involved at all: the analysis pipeline calls
# srvyr::, analysistools:: and cleaningtools:: by name. renv reads those calls,
# so all three have to be installed for the manifest to record them - even
# though a run only reaches them when a List of Analysis asks for a confidence
# level, or for recreate_sm_parents. Deploying pulls them in whether or not you
# use them.
# =============================================================================


#' The Files a Deployment Actually Needs
#'
#' A whitelist, not an exclusion list: anything new in the project is left out
#' until it is named here, which is the safe direction for a step that uploads
#' to a server.
#'
#' @param app_dir The project root.
#' @return A character vector of paths relative to `app_dir`.
#' @export
ak_manifest_files <- function(app_dir = ".") {
  relative <- function(pattern, path) {
    found <- list.files(
      file.path(app_dir, path), pattern = pattern, full.names = FALSE
    )
    if (length(found) == 0) character(0) else file.path(path, found)
  }

  files <- c(
    "app.R",
    # Deployment tooling is not part of the application. Bundling it would
    # also record rsconnect as a dependency of the app.
    setdiff(relative("[.][Rr]$", "R"), c("R/manifest.R", "R/generate_manifest.R")),
    relative("[.][Rr]$", "functions"),
    relative("[.](css|png|js|svg|ico)$", "www"),
    # The template is downloadable from the interface, so it has to travel with
    # the app - a deployed user has no access to the repository.
    "docs/loa_template.xlsx"
  )

  files[file.exists(file.path(app_dir, files))]
}


#' The Hard Dependencies a Package Declares
#'
#' Depends, Imports and LinkingTo - the ones that must be installed for the
#' package to work. Suggests is deliberately left out.
#'
#' @param package An installed package name.
#' @return A character vector of package names.
#' @keywords internal
ak_hard_dependencies <- function(package) {
  # packageDescription() warns rather than errors for a package that is not
  # there, and the caller already handles "not installed" separately.
  described <- suppressWarnings(tryCatch(
    utils::packageDescription(package, fields = c("Depends", "Imports", "LinkingTo")),
    error = function(e) NULL
  ))
  if (is.null(described) || all(is.na(described))) return(character(0))

  declared <- unlist(described, use.names = FALSE)
  declared <- declared[!is.na(declared)]
  if (length(declared) == 0) return(character(0))

  parts <- unlist(strsplit(paste(declared, collapse = ","), ",", fixed = TRUE))
  parts <- trimws(sub("\\(.*", "", parts))
  setdiff(parts[nzchar(parts)], c("R", "base", "utils", "stats", "methods",
                                  "graphics", "grDevices", "tools", "parallel",
                                  "splines", "grid", "compiler", "datasets"))
}


#' Packages the Deployment Will Need
#'
#' Taken from renv's own scan of the bundled files where renv is available, so
#' the answer matches what `writeManifest()` will look for. Falls back to the
#' declared requirements otherwise.
#'
#' @param app_dir The project root.
#' @param files The files to scan. Defaults to \code{\link{ak_manifest_files}}.
#' @return A character vector of package names.
#' @export
ak_manifest_packages <- function(app_dir = ".", files = ak_manifest_files(app_dir)) {
  if (requireNamespace("renv", quietly = TRUE)) {
    found <- tryCatch(
      renv::dependencies(file.path(app_dir, files), quiet = TRUE)$Package,
      error = function(e) NULL
    )
    if (length(found) > 0) {
      return(sort(unique(found)))
    }
  }

  sort(unique(ak_package_requirements()$package))
}


#' What Would Stop the Manifest Being Written
#'
#' Walks the dependency closure of everything the deployment needs and reports
#' both kinds of problem in one list: a package the app calls that is not
#' installed, and a package that is installed but whose own dependencies are
#' not.
#'
#' @param app_dir The project root.
#' @param packages Packages to check. Defaults to
#'   \code{\link{ak_manifest_packages}}.
#' @return A data frame with `package`, `problem` and `needed_by`.
#' @export
ak_manifest_preflight <- function(app_dir = ".",
                                  packages = ak_manifest_packages(app_dir)) {
  problems <- list()
  note <- function(package, problem, needed_by) {
    problems[[length(problems) + 1]] <<- data.frame(
      package = package, problem = problem, needed_by = needed_by,
      stringsAsFactors = FALSE
    )
  }

  seen <- character(0)
  queue <- packages

  while (length(queue) > 0) {
    package <- queue[1]
    queue <- queue[-1]
    if (package %in% seen) next
    seen <- c(seen, package)

    if (!requireNamespace(package, quietly = TRUE)) {
      # Reported against whoever asked for it, so the user can tell an app
      # dependency from something dragged in three levels down.
      note(package, "not installed", if (package %in% packages) "the app" else "another package")
      next
    }

    queue <- c(queue, setdiff(ak_hard_dependencies(package), seen))
  }

  # Attribute each missing package to the installed package that needs it.
  for (i in seq_along(problems)) {
    package <- problems[[i]]$package
    needed_by <- seen[vapply(
      seen,
      function(p) {
        requireNamespace(p, quietly = TRUE) && package %in% ak_hard_dependencies(p)
      },
      logical(1),
      USE.NAMES = FALSE
    )]
    if (length(needed_by) > 0) {
      problems[[i]]$needed_by <- paste(needed_by, collapse = ", ")
    }
  }

  if (length(problems) == 0) {
    return(data.frame(
      package = character(0), problem = character(0),
      needed_by = character(0), stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, problems)
}


#' How to Install What Is Missing
#'
#' @param problems The output of \code{\link{ak_manifest_preflight}}.
#' @return A character vector of commands.
#' @keywords internal
ak_manifest_fixes <- function(problems) {
  if (nrow(problems) == 0) return(character(0))

  github <- c(
    analysistools = "impact-initiatives/analysistools",
    cleaningtools = "impact-initiatives/cleaningtools"
  )

  from_github <- problems$package[problems$package %in% names(github)]
  from_cran <- setdiff(problems$package, from_github)

  c(
    if (length(from_cran) > 0) {
      paste0(
        "install.packages(c(",
        paste0('"', sort(from_cran), '"', collapse = ", "),
        "))"
      )
    },
    if (length(from_github) > 0) {
      paste0('remotes::install_github("', github[sort(from_github)], '")')
    }
  )
}


#' Write manifest.json
#'
#' @param app_dir The project root.
#' @param quiet Logical. Suppress progress messages.
#' @return Invisibly, the path to the manifest.
#' @export
ak_write_manifest <- function(app_dir = ".", quiet = FALSE) {
  say <- function(...) if (!isTRUE(quiet)) message(...)

  if (!requireNamespace("rsconnect", quietly = TRUE)) {
    stop(
      "Writing a manifest needs the 'rsconnect' package. Install it with install.packages('rsconnect').",
      call. = FALSE
    )
  }

  files <- ak_manifest_files(app_dir)
  say("Bundling ", length(files), " file(s).")

  problems <- ak_manifest_preflight(app_dir)

  if (nrow(problems) > 0) {
    stop(
      paste0(
        "manifest.json cannot be written until these package(s) are installed:\n  ",
        paste0(
          problems$package, " (", problems$problem, "; needed by ",
          problems$needed_by, ")",
          collapse = "\n  "
        ),
        "\n\nRun:\n  ",
        paste(ak_manifest_fixes(problems), collapse = "\n  "),
        "\n\nThe analysis pipeline calls srvyr::, analysistools:: and ",
        "cleaningtools:: by name, so renv records them even though a run only ",
        "reaches them when a List of Analysis asks for a confidence level."
      ),
      call. = FALSE
    )
  }

  # No library(rsconnect): it attaches serverInfo(), which masks shiny's.
  rsconnect::writeManifest(appDir = app_dir, appFiles = files, quiet = quiet)

  manifest <- file.path(app_dir, "manifest.json")
  if (!file.exists(manifest)) {
    stop("writeManifest() reported success but no manifest.json appeared.", call. = FALSE)
  }

  say("Wrote ", manifest, ".")
  invisible(manifest)
}


