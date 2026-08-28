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
#' One place that answers all three questions the interface needs: does the
#' user pick a folder, is the destination already settled, and what should the
#' step say.
#'
#' @param server Logical. Defaults to \code{\link{ak_is_server}}.
#' @return A list with `pick_folder`, `settled` and `explanation`.
#' @export
ak_destination_mode <- function(server = ak_is_server()) {
  if (isTRUE(server)) {
    list(
      pick_folder = FALSE,
      settled = TRUE,
      explanation = paste0(
        "Analysis Kit is running on a server, so there is no folder of yours ",
        "to save into. The results workbook is built here and offered as a ",
        "download when the run finishes."
      )
    )
  } else {
    list(
      pick_folder = TRUE,
      settled = FALSE,
      explanation = paste0(
        "Choose the folder to save the results workbook into. It is written ",
        "there as soon as the run finishes."
      )
    )
  }
}
