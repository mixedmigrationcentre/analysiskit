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
#'
#' The fourth step is the one that changes name with the deployment: locally
#' the user picks a destination, while served there is nothing to pick and the
#' step is about how the file reaches them. `ak_destination_mode()` supplies
#' the label, so the tracker never announces a choice the user is not being
#' offered.
#'
#' @param destination_label Label for the fourth step.
#' @return A named character vector: step id to label.
#' @keywords internal
ak_steps <- function(destination_label = "Destination") {
  c(
    dataset = "Dataset",
    loa = "List of Analysis",
    checks = "Checks",
    destination = destination_label,
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
#' @param destination_label Label for the destination step, from
#'   \code{\link{ak_destination_mode}}.
#' @return A Shiny tag.
#' @export
ak_step_tracker <- function(states, destination_label = "Destination") {
  steps <- ak_steps(destination_label)

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


#' The Exclusive-Combination Base, Spelled Out
#'
#' `count_exclusive_combinations` rows sit on a smaller denominator than every
#' other table in the output: a respondent who selected a listed choice
#' *together with* an unlisted one belongs to none of the categories and leaves
#' the base. That is the intended meaning of "only", but it is invisible in the
#' finished workbook - the percentages simply look like every other percentage.
#'
#' So the number is reported per question, as a share of everyone who answered,
#' in the place someone sees before they export the file.
#'
#' @param exclusive_map The `exclusive_combinations` element of a pipeline
#'   result: `analysis_var`, `n_in_base` and `n_mixed_dropped`.
#' @return A character vector of sentences, or `character(0)` when no
#'   respondent was dropped.
#' @export
ak_exclusive_base_note <- function(exclusive_map) {
  if (is.null(exclusive_map) || nrow(exclusive_map) == 0) {
    return(character(0))
  }
  if (!all(c("analysis_var", "n_mixed_dropped") %in% names(exclusive_map))) {
    return(character(0))
  }

  dropped <- as.numeric(exclusive_map$n_mixed_dropped)
  dropped[is.na(dropped)] <- 0
  in_base <- if ("n_in_base" %in% names(exclusive_map)) {
    as.numeric(exclusive_map$n_in_base)
  } else {
    rep(NA_real_, nrow(exclusive_map))
  }

  keep <- dropped > 0
  if (!any(keep)) {
    return(character(0))
  }

  answered <- dropped + in_base
  share <- ifelse(is.finite(answered) & answered > 0, 100 * dropped / answered, NA_real_)

  paste0(
    exclusive_map$analysis_var[keep], ": ",
    format(dropped[keep], big.mark = ",", trim = TRUE),
    " respondent(s)",
    ifelse(
      is.na(share[keep]),
      "",
      paste0(" (", format(round(share[keep], 1), nsmall = 1, trim = TRUE), "% of those who answered)")
    ),
    " selected one of these choices together with an unlisted one, and are outside this base."
  )
}


#' Render the Exclusive-Combination Caveat
#' @param exclusive_map The `exclusive_combinations` element of a result.
#' @return A Shiny tag; empty when nothing was dropped.
#' @export
ak_exclusive_base_panel <- function(exclusive_map) {
  notes <- ak_exclusive_base_note(exclusive_map)
  if (length(notes) == 0) {
    return(shiny::tagList())
  }

  shiny::tags$div(
    class = "status-message status-warning",
    shiny::tags$strong(
      "These rows use a smaller denominator than the rest of the workbook."
    ),
    shiny::tags$ul(
      class = "ak-caveat",
      lapply(notes, shiny::tags$li)
    ),
    "Footnote this wherever the exclusive percentages are published."
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
