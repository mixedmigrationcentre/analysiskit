# =============================================================================
# Where is this running, and what does that change
# =============================================================================
#
# Analysis Kit behaves differently depending on whose computer it is on, and
# the difference is not cosmetic:
#
#   * Run locally, the folder picker browses the analyst's own filesystem and
#     the results workbook is written straight into a folder they keep.
#   * Served - Posit Cloud, shinyapps.io, Posit Connect, Shiny Server - that
#     same picker browses the *server's* disk. A file written there is not on
#     the analyst's computer and disappears when the container recycles. The
#     deliverable has to reach them as a download instead.
#
# Guessing wrong in the safe direction matters more than guessing right: an
# unnecessary download button is a minor annoyance, while a folder picker that
# writes into a container the user cannot reach loses their work silently. So
# the download is always offered, and the folder picker only appears when this
# looks like a local session.
# =============================================================================


#' Is Analysis Kit Being Served Rather Than Run Locally
#'
#' Detection is a set of signals rather than one test, because no single
#' variable covers Posit Connect, shinyapps.io and Shiny Server. Both an option
#' and an environment variable override it, so a deployment this does not
#' recognise can still be told what it is.
#'
#' @return `TRUE` when the app appears to be served.
#' @export
ak_is_server <- function() {
  override <- getOption("analysiskit.server", NULL)
  if (!is.null(override)) {
    return(isTRUE(override))
  }

  from_env <- Sys.getenv("ANALYSISKIT_SERVER", "")
  if (nzchar(from_env)) {
    return(tolower(from_env) %in% c("1", "true", "yes"))
  }

  any(c(
    nzchar(Sys.getenv("SHINY_PORT")),                        # Shiny Server, shinyapps.io
    nzchar(Sys.getenv("SHINY_SERVER_VERSION")),              # Shiny Server
    identical(Sys.getenv("R_CONFIG_ACTIVE"), "rsconnect"),   # shinyapps.io
    nzchar(Sys.getenv("CONNECT_SERVER")),                    # Posit Connect
    nzchar(Sys.getenv("RSTUDIO_CONNECT_HASTE")),             # Posit Connect
    dir.exists("/opt/shiny-server")
  ))
}


#' Where a Served Run Puts Its Workbook
#'
#' A per-session temporary folder. The file is built there and then handed over
#' as a download; nothing is left in a place the user might mistake for
#' storage.
#'
#' @param create Logical. Create the folder if it is not there.
#' @return The folder path.
#' @export
ak_session_output_dir <- function(create = TRUE) {
  path <- file.path(tempdir(), "analysiskit-output")
  if (isTRUE(create) && !dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  path
}


#' How the Destination Should Behave Here
#'
#' One place that answers everything the interface needs to know: does the user
#' pick a folder, is the destination already settled, what is this step called,
#' and what does it say.
#'
#' The wording carries more weight than it looks. Served, the folder picker is
#' absent by design - but a step headed "Output folder" with no control under
#' it reads as a feature that failed to load rather than one that does not
#' apply. So the served build names the step after what actually happens,
#' states the download as a settled fact rather than a neutral aside, and says
#' where the button will appear.
#'
#' @param server Logical. Defaults to \code{\link{ak_is_server}}.
#' @return A list with `pick_folder`, `settled`, `label`, `step_label`,
#'   `status_type` and `explanation`.
#' @export
ak_destination_mode <- function(server = ak_is_server()) {
  if (isTRUE(server)) {
    list(
      pick_folder = FALSE,
      settled = TRUE,
      label = "How you get the results",
      step_label = "Delivery",
      # "success", not "neutral": nothing here is pending and nothing is
      # missing. The step is already complete, and it should look it.
      status_type = "success",
      explanation = paste0(
        "Ready - the workbook comes to you as a download. Analysis Kit is ",
        "running on a server, so it has no folder of yours to save into. When ",
        "the run finishes, a Download button appears on the Results tab; save ",
        "the file wherever you like from there."
      )
    )
  } else {
    list(
      pick_folder = TRUE,
      settled = FALSE,
      label = "Output folder",
      step_label = "Destination",
      status_type = "neutral",
      explanation = paste0(
        "Choose the folder to save the results workbook into. It is written ",
        "there as soon as the run finishes."
      )
    )
  }
}
