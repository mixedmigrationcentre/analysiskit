# =============================================================================
# Presentation helpers for the Analysis Kit interface
# =============================================================================
#
# Two kinds of function live here, and the split is deliberate:
#
#   * ak_step_states(), ak_problem_counts(), ak_problems_display() and
#     ak_sheet_summary() are pure. They take facts and return facts, so the
#     rules behind the interface are testable without starting a Shiny session.
#   * ak_step_tracker(), ak_badges() and ak_status() turn those facts into
#     tags. They hold no logic beyond arranging what they are given.
#
# No analysis and no validation happens here.
# =============================================================================


#' The Steps of the Workflow
#' @return A named character vector: step id to label.
#' @keywords internal
ak_steps <- function() {
  c(
    dataset = "Dataset",
    loa = "List of Analysis",
    checks = "Checks",
    destination = "Destination",
    results = "Results"
  )
}


#' Work Out the State of Each Step
#'
#' The single rule behind the tracker, kept out of the server so it can be
#' tested directly.
#'
#' @param dataset_loaded,dataset_failed State of the dataset upload.
#' @param loa_loaded,loa_failed State of the List of Analysis upload.
#' @param problems The problems table, or `NULL` when the checks have not run.
#' @param destination_chosen,destination_failed State of the output folder.
#' @param results_ready Logical. Results exist.
#' @param running Logical. An analysis is in progress.
#' @return A named character vector of `"todo"`, `"active"`, `"done"`,
#'   `"warning"` or `"error"`, one per step.
#' @export
ak_step_states <- function(dataset_loaded = FALSE,
                           dataset_failed = FALSE,
                           loa_loaded = FALSE,
                           loa_failed = FALSE,
                           problems = NULL,
                           destination_chosen = FALSE,
                           destination_failed = FALSE,
                           results_ready = FALSE,
                           running = FALSE) {
  states <- c(
    dataset = "todo", loa = "todo", checks = "todo",
    destination = "todo", results = "todo"
  )

  states[["dataset"]] <- if (dataset_failed) {
    "error"
  } else if (dataset_loaded) {
    "done"
  } else {
    "active"
  }

  if (dataset_loaded) {
    states[["loa"]] <- if (loa_failed) {
      "error"
    } else if (loa_loaded) {
      "done"
    } else {
      "active"
    }
  }

  if (dataset_loaded && loa_loaded && !is.null(problems)) {
    states[["checks"]] <- if (loa_has_errors(problems)) {
      "error"
    } else if (nrow(problems) > 0) {
      "warning"
    } else {
      "done"
    }
  }

  # The folder is only asked for once there is something worth saving: offering
  # it before the checks pass invites the user to answer a question that may
  # turn out not to matter.
  if (states[["checks"]] %in% c("done", "warning")) {
    states[["destination"]] <- if (destination_failed) {
      "error"
    } else if (destination_chosen) {
      "done"
    } else {
      "active"
    }
  }

  states[["results"]] <- if (running) {
    "active"
  } else if (results_ready) {
    "done"
  } else if (identical(states[["destination"]], "done")) {
    "active"
  } else {
    "todo"
  }

  states
}


#' Can the Analysis Be Run
#'
#' Both files in, no fatal check, and somewhere to put the result. The
#' destination is part of the gate because a run that cannot be saved wastes
#' the wait.
#'
#' @param states A vector from \code{\link{ak_step_states}}.
#' @return `TRUE` when the run should be offered.
#' @export
ak_can_run <- function(states) {
  identical(unname(states[["dataset"]]), "done") &&
    identical(unname(states[["loa"]]), "done") &&
    unname(states[["checks"]]) %in% c("done", "warning") &&
    identical(unname(states[["destination"]]), "done")
}


#' Render the Step Tracker
#'
#' A horizontal row of steps showing where the user is, which is the one piece
#' of the interface that is always on screen.
#'
#' @param states A vector from \code{\link{ak_step_states}}.
#' @return A Shiny tag.
#' @export
ak_step_tracker <- function(states) {
  steps <- ak_steps()

  symbol <- c(
    todo = "○", active = "●", done = "✓",
    warning = "!", error = "×"
  )

  items <- lapply(names(steps), function(id) {
    state <- states[[id]]
    shiny::tags$li(
      class = paste0("ak-step ak-step-", state),
      # The shape carries the state as well as the colour, so the tracker is
      # still readable without relying on colour alone.
      shiny::tags$span(class = "ak-step-mark", symbol[[state]]),
      shiny::tags$span(class = "ak-step-label", steps[[id]])
    )
  })

  shiny::tags$ol(
    class = "ak-steps",
    role = "list",
    `aria-label` = "Workflow progress",
    items
  )
}


#' Count Problems by Severity
#' @param problems A problems data frame, or `NULL`.
#' @return A named integer vector with `error` and `warning`.
#' @export
ak_problem_counts <- function(problems) {
  out <- c(error = 0L, warning = 0L)
  if (is.null(problems) || nrow(problems) == 0) {
    return(out)
  }
  tab <- table(problems$severity)
  for (nm in intersect(names(tab), names(out))) {
    out[[nm]] <- as.integer(tab[[nm]])
  }
  out
}


#' Format the Problems Table for Display
#'
#' Errors first, then warnings, each group in workbook order. `Where` collapses
#' the sheet and row into the one string a user needs to find the cell.
#'
#' @param problems A problems data frame, or `NULL`.
#' @return A data frame with `Severity`, `Where` and `What to fix`.
#' @export
ak_problems_display <- function(problems) {
  empty <- data.frame(
    Severity = character(0), Where = character(0),
    `What to fix` = character(0),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  if (is.null(problems) || nrow(problems) == 0) {
    return(empty)
  }

  ordered <- problems[order(problems$severity != "error"), , drop = FALSE]

  data.frame(
    Severity = ifelse(ordered$severity == "error", "Must fix", "Warning"),
    Where = paste0(
      ordered$sheet,
      ifelse(is.na(ordered$row), "", paste0(" · row ", ordered$row))
    ),
    `What to fix` = ordered$message,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}


#' Summarise Which Sheets a Workbook Supplied
#' @param workbook The list from `read_loa_workbook()`.
#' @return A data frame with `Sheet`, `Rows` and `Status`.
#' @export
ak_sheet_summary <- function(workbook) {
  known <- loa_known_sheets()
  sheets <- workbook$sheets %||% list()

  data.frame(
    Sheet = known,
    Rows = vapply(
      known,
      function(nm) if (is.null(sheets[[nm]])) 0L else nrow(sheets[[nm]]),
      integer(1),
      USE.NAMES = FALSE
    ),
    Status = vapply(
      known,
      function(nm) {
        if (is.null(sheets[[nm]])) {
          if (nm == "analysis") "Missing - required" else "Not supplied - defaults apply"
        } else if (nrow(sheets[[nm]]) == 0) {
          "Empty - defaults apply"
        } else {
          "Read"
        }
      },
      character(1),
      USE.NAMES = FALSE
    ),
    stringsAsFactors = FALSE
  )
}


#' Render a Row of Severity Badges
#' @param counts A vector from \code{\link{ak_problem_counts}}.
#' @return A Shiny tag.
#' @export
ak_badges <- function(counts) {
  plural <- function(n, one, many) paste(n, if (n == 1L) one else many)

  if (counts[["error"]] == 0L && counts[["warning"]] == 0L) {
    return(shiny::tags$div(
      class = "ak-badges",
      shiny::tags$span(class = "ak-badge ak-badge-ok", "All checks passed")
    ))
  }

  shiny::tags$div(
    class = "ak-badges",
    if (counts[["error"]] > 0L) {
      shiny::tags$span(
        class = "ak-badge ak-badge-error",
        plural(counts[["error"]], "problem to fix", "problems to fix")
      )
    },
    if (counts[["warning"]] > 0L) {
      shiny::tags$span(
        class = "ak-badge ak-badge-warning",
        plural(counts[["warning"]], "warning", "warnings")
      )
    }
  )
}


#' Render a Status Message
#' @param text The message.
#' @param type `"neutral"`, `"success"`, `"warning"` or `"error"`.
#' @return A Shiny tag.
#' @export
ak_status <- function(text, type = "neutral") {
  shiny::tags$div(
    class = paste0(
      "status-message",
      switch(
        type,
        success = " status-success",
        warning = " status-warning",
        error = " status-error",
        ""
      )
    ),
    text
  )
}


#' Render a Section With a Heading
#' @param title Section heading.
#' @param ... Section contents.
#' @return A Shiny tag.
#' @export
ak_section <- function(title, ...) {
  shiny::tags$div(
    class = "dataset-section",
    shiny::tags$h3(title),
    ...
  )
}
