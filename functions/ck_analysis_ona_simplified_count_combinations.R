# =============================================================================
# ONA-flavoured grouped analysis pipeline  --  MMC 4Mi cleaning kit
# SIMPLIFIED BUILD.  Same public functions, same arguments, same outputs.
# =============================================================================
#
# What changed relative to ck_analysis_ona.R
# ------------------------------------------
# Nothing that reaches the output. Every exported name, argument, default and
# returned column is unchanged. The reductions are:
#   * the four result-frame constructions in ck_fast_analysis() now go through
#     one ck_res_df() helper
#   * the repeated ck_gsum() calls per analysis are one rowsum() over a matrix
#     (ck_group_sums()); ck_gsum() is kept, unchanged, as the public one-vector
#     version
#   * long rationale essays moved out of roxygen into this header
#   * repeated warning/message boilerplate collapsed
# Two things that LOOK mergeable and are deliberately not:
#   * ck_order_count_blocks() and ck_insert_count_separators() run either side
#     of the column_map build, so row_type is never read as a statistic column
#   * ck_var_is_available() and ck_loa_is_available() are both called directly
#     by the test scripts
#
# Design decisions (unchanged)
# ----------------------------
# * The ONA export carries question labels in row 1 and choice LABELS (not XML
#   names) in the cells. There is no XLSForm to read, so no tool_survey /
#   tool_choices argument exists anywhere in this file.
# * presentresults::create_label_dictionary() and
#   add_label_columns_to_results_table() are not used: they need a Kobo survey +
#   choices sheet, and the choices half of the dictionary has nothing to do when
#   the cells already hold labels. ck_relabel_questions() replaces them.
# * analysistools::create_analysis() is kept as the *variance* engine only. The
#   point estimate of a weighted mean, proportion or ratio is sum(w*x)/sum(w)
#   whatever the strata are - verified against survey::svyby to 3e-17 for
#   proportions, 7e-15 for means, exactly 0 for ratios, and identically 0
#   between a stratified and an unstratified design. So when the LOA `level`
#   cell is empty no interval was asked for, there is nothing for survey to do,
#   and ck_fast_analysis() tabulates in one pass instead. Measured on a
#   6,000 x 210 export: svyby 200-level group, 10 questions = 9.43 s; weighted
#   tabulation = 0.05 s.
# * Weights and strata are both optional. With neither, the dataset is analysed
#   as it is (unweighted SRS).
# * sm_separator defaults to "/" (ONA export style).
# =============================================================================


# -----------------------------------------------------------------------------
# 0. Utilities
# -----------------------------------------------------------------------------

#' Require a Suggested Package
#'
#' @param pkg Package name.
#' @param repo Optional GitHub org/repo used in the install hint.
#' @keywords internal
ck_require_pkg <- function(pkg, repo = NULL) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    hint <- if (is.null(repo)) {
      paste0("install.packages('", pkg, "')")
    } else {
      paste0("remotes::install_github('", repo, "')")
    }
    stop(
      paste0("Package '", pkg, "' is required for this function. Install with: ", hint),
      call. = FALSE
    )
  }
  invisible(TRUE)
}


#' Emit a Pipeline Progress Message
#'
#' Uses \code{message()} rather than \code{cat()} so callers can silence the
#' pipeline with \code{suppressMessages()}.
#'
#' @param ... Pasted into a single message.
#' @param verbose Logical. If \code{FALSE} nothing is emitted.
#' @keywords internal
ck_note <- function(..., verbose = TRUE) {
  if (isTRUE(verbose)) message(paste0("--> ", ...))
  invisible(NULL)
}


#' Raise a Warning Without a Call Trace
#' @param ... Pasted into the warning text.
#' @keywords internal
ck_warn <- function(...) {
  warning(paste0(...), call. = FALSE)
  invisible(NULL)
}


#' Default Analysis Type Labels
#'
#' @return A dataframe with \code{analysis_type} and \code{label_analysis_type}.
#' @keywords internal
ck_analysis_type_labels <- function() {
  data.frame(
    analysis_type = c(
      "prop_select_one", "prop_select_multiple", "count_select_multiple",
      "combination_select_multiple", "mean", "median", "ratio"
    ),
    label_analysis_type = c(
      "Proportion (single choice)", "Proportion (multiple choice)",
      "Number of choices selected", "Combination of choices selected",
      "Mean", "Median", "Ratio"
    ),
    stringsAsFactors = FALSE
  )
}


# -----------------------------------------------------------------------------
# 1. Labels from the ONA label row
# -----------------------------------------------------------------------------

#' Split the ONA Label Row Off a Dataset
#'
#' Row 1 of an ONA export is label text. Left in place it is counted as a
#' respondent: it adds a bogus category to every proportion and becomes
#' \code{NA} for every mean / median.
#'
#' @param dataset The raw ONA export.
#' @param skip_label_row Logical. Remove row 1 and return it. Default
#'   \code{TRUE}.
#' @return A list with \code{dataset} and \code{label_row} (or \code{NULL}).
#' @export
ck_split_label_row <- function(dataset, skip_label_row = TRUE) {
  label_row <- NULL

  if (isTRUE(skip_label_row) && nrow(dataset) >= 1) {
    label_row <- dataset[1, , drop = FALSE]
    dataset <- dataset[-1, , drop = FALSE]
  }

  list(dataset = dataset, label_row = label_row)
}


#' Build a Question Label Lookup from an ONA Label Row
#'
#' In MMC 4Mi exports the label row for a select_multiple *child* column simply
#' repeats the column name (column \code{"QN9/Detention"} has label
#' \code{"QN9/Detention"}), which carries no information - the choice label is
#' already in the column name. Those self-referencing entries are dropped so
#' downstream code falls back to the column name, which is the better label.
#'
#' @param label_row A one-row dataframe: names are machine names, values are
#'   labels.
#' @param drop_self_labels Logical. Drop entries whose label equals the column
#'   name. Default \code{TRUE}.
#' @return A named character vector.
#' @export
ck_build_label_lookup <- function(label_row, drop_self_labels = TRUE) {
  if (is.null(label_row)) {
    return(character(0))
  }
  if (!is.data.frame(label_row)) {
    stop("label_row must be a one-row dataframe taken from the ONA export.")
  }
  if (nrow(label_row) < 1) {
    return(character(0))
  }

  label_names <- colnames(label_row)
  labels <- as.character(unlist(label_row[1, , drop = TRUE], use.names = FALSE))

  # str_squish() drops names, so squish first and re-attach the names
  labels <- stringr::str_squish(labels)
  names(labels) <- label_names

  labels <- labels[!is.na(labels) & labels != "" & labels != "NA"]

  if (isTRUE(drop_self_labels) && length(labels) > 0) {
    labels <- labels[labels != names(labels)]
  }

  labels
}


#' Select Multiple Child Columns and Their Choice Suffixes
#'
#' The suffix is taken by removing the \emph{parent prefix only}, never by
#' splitting on the separator: 4Mi choice labels contain the separator
#' themselves (e.g. \code{"QN9/Interception at sea/pull-back to point of
#' embarkation"}), so a split would truncate them.
#'
#' @param parent The select_multiple parent variable name.
#' @param data_names Column names of the dataset.
#' @param sm_separator Separator between parent and choice. Default \code{"/"}.
#' @return A named character vector: names are child columns, values are the
#'   choice suffixes.
#' @export
ck_sm_child_suffixes <- function(parent, data_names, sm_separator = "/") {
  if (is.na(parent)) {
    return(character(0))
  }

  prefix <- paste0(parent, sm_separator)
  child_cols <- data_names[startsWith(data_names, prefix)]

  if (length(child_cols) == 0) {
    return(character(0))
  }

  suffixes <- substr(child_cols, nchar(prefix) + 1, nchar(child_cols))
  names(suffixes) <- child_cols

  suffixes
}


#' Longest Common Prefix of a Character Vector
#'
#' @param x A character vector.
#' @return The longest common leading substring, or \code{NA}.
#' @keywords internal
ck_common_prefix <- function(x) {
  x <- x[!is.na(x) & x != ""]

  if (length(x) == 0) {
    return(NA_character_)
  }
  if (length(x) == 1) {
    return(x[1])
  }

  max_shared <- min(nchar(x))
  shared <- 0L

  # substr() is vectorised over x, so one character position per iteration is
  # enough - no need to split every string into characters first.
  while (shared < max_shared &&
         length(unique(substr(x, shared + 1L, shared + 1L))) == 1L) {
    shared <- shared + 1L
  }

  if (shared == 0L) NA_character_ else substr(x[1], 1L, shared)
}


#' Derive a Select Multiple Parent Label from its Child Column Labels
#'
#' \code{"Why did you leave?/Conflict"} and \code{"Why did you leave?/Economic"}
#' give \code{"Why did you leave?"}.
#'
#' @param parent The select_multiple parent variable name.
#' @param label_lookup A named vector from \code{\link{ck_build_label_lookup}}.
#' @param sm_separator Select_multiple separator. Default \code{"/"}.
#' @return The derived parent label, or \code{NA}.
#' @keywords internal
ck_parent_label_from_children <- function(parent, label_lookup, sm_separator = "/") {
  if (length(label_lookup) == 0 || is.na(parent)) {
    return(NA_character_)
  }

  child_names <- names(label_lookup)[
    startsWith(names(label_lookup), paste0(parent, sm_separator))
  ]

  if (length(child_names) == 0) {
    return(NA_character_)
  }

  shared_prefix <- ck_common_prefix(unname(label_lookup[child_names]))
  if (is.na(shared_prefix)) {
    return(NA_character_)
  }

  positions <- gregexpr(sm_separator, shared_prefix, fixed = TRUE)[[1]]

  if (positions[1] != -1) {
    parent_label <- trimws(substr(shared_prefix, 1, positions[length(positions)] - 1))
  } else {
    # The label row may use a different delimiter from the column names (columns
    # split on "." but labels written with "/"). Only trust the shared prefix
    # when it actually ends on a delimiter, so that merely shared wording
    # ("Option ") is not mistaken for a question label.
    if (!stringr::str_detect(shared_prefix, "[/.:|>\\-]\\s*$")) {
      return(NA_character_)
    }
    parent_label <- trimws(stringr::str_remove(shared_prefix, "[\\s/.:|>\\-]+$"))
  }

  # When the label row only repeats the column names, the shared prefix
  # collapses back to the parent code itself, which is not a label.
  if (is.na(parent_label) || parent_label == "" || identical(parent_label, parent)) {
    return(NA_character_)
  }

  parent_label
}


#' Relabel Question Names in an Analysis Table
#'
#' Adds \code{label_analysis_var} from the ONA label row. Only the *questions*
#' are relabelled: choice values are already labels in an ONA export. For
#' select_multiple, \code{analysis_var_value} holds the child suffix, so the
#' child column label is used where available and the repeated parent prefix is
#' stripped.
#'
#' @param results_table A dataframe containing at least \code{analysis_var}.
#' @param label_lookup A named vector from \code{\link{ck_build_label_lookup}}.
#' @param sm_separator Separator between parent and choice. Default \code{"/"}.
#' @param label_choices Logical. Relabel select_multiple choice values. Default
#'   \code{TRUE}.
#' @return \code{results_table} with \code{label_analysis_var} added.
#' @export
ck_relabel_questions <- function(results_table,
                                 label_lookup,
                                 sm_separator = "/",
                                 label_choices = TRUE) {
  if (!"analysis_var" %in% names(results_table)) {
    stop("results_table must contain an analysis_var column.")
  }

  if (length(label_lookup) == 0) {
    results_table$label_analysis_var <- results_table$analysis_var
    return(results_table)
  }

  analysis_var <- as.character(results_table$analysis_var)
  question_label <- unname(label_lookup[analysis_var])

  # Select_multiple parents are often absent from the label row (only their
  # child columns are exported), so recover the parent label from the children.
  for (v in unique(analysis_var[is.na(question_label)])) {
    derived <- ck_parent_label_from_children(v, label_lookup, sm_separator)
    if (!is.na(derived)) {
      question_label[analysis_var == v] <- derived
    }
  }

  question_label <- ifelse(is.na(question_label), analysis_var, question_label)

  if (isTRUE(label_choices) && "analysis_var_value" %in% names(results_table)) {
    var_value <- as.character(results_table$analysis_var_value)
    child_label <- unname(label_lookup[paste0(analysis_var, sm_separator, var_value)])

    # Strip a repeated prefix - first the parent code ("QN9/Detention" ->
    # "Detention"), then the parent question label ("Which countries did you
    # transit?/Sudan" -> "Sudan"). substr() is used rather than a regex so that
    # choice labels containing "(", ")" or "." are safe. Only the second pass
    # tidies leading delimiters and whitespace, because the label row may write
    # the delimiter differently from the column names; the code prefix is an
    # exact column-name prefix and needs no tidying.
    strip_prefix <- function(label, prefix, tidy) {
      hit <- !is.na(label) &
        substr(label, 1, nchar(prefix)) == prefix &
        nchar(label) > nchar(prefix)

      if (any(hit)) {
        trimmed <- substr(label[hit], nchar(prefix[hit]) + 1, nchar(label[hit]))
        if (isTRUE(tidy)) {
          trimmed <- stringr::str_squish(
            stringr::str_remove(trimmed, "^[\\s/.:|>\\-]+")
          )
        }
        label[hit] <- trimmed
      }
      label
    }

    child_label <- strip_prefix(
      child_label, paste0(analysis_var, sm_separator), tidy = FALSE
    )
    child_label <- strip_prefix(child_label, question_label, tidy = TRUE)

    results_table$analysis_var_value <- ifelse(
      is.na(child_label) | child_label == "", var_value, child_label
    )
  }

  results_table$label_analysis_var <- question_label

  dplyr::relocate(results_table, "label_analysis_var", .after = "analysis_var")
}


# -----------------------------------------------------------------------------
# 2. Preparing the ONA export for analysis
# -----------------------------------------------------------------------------

#' Turn Blank Strings into NA
#'
#' ONA uses empty strings for skipped questions. Left as they are, "" becomes a
#' legitimate response category and inflates every denominator.
#'
#' @param dataset A dataframe.
#' @return The dataframe with "" replaced by \code{NA} in character columns.
#' @export
ck_blank_to_na <- function(dataset) {
  for (v in names(dataset)) {
    if (is.character(dataset[[v]])) {
      x <- dataset[[v]]
      x[!is.na(x) & trimws(x) == ""] <- NA_character_
      dataset[[v]] <- x
    }
  }
  dataset
}


#' Coerce Analysis Variables to Numeric, Reporting Losses
#'
#' Everything below the ONA label row reads as text. Unlike a bare
#' \code{as.numeric()}, this reports how many non-blank values failed to parse.
#'
#' @param dataset A dataframe.
#' @param vars Character vector of columns to coerce.
#' @param verbose Logical. Report coercion losses. Default \code{TRUE}.
#' @return The dataframe with \code{vars} coerced to numeric.
#' @export
ck_coerce_numeric <- function(dataset, vars, verbose = TRUE) {
  vars <- intersect(unique(vars[!is.na(vars)]), names(dataset))

  for (v in vars) {
    if (is.numeric(dataset[[v]])) next

    old_chr <- trimws(as.character(dataset[[v]]))
    new <- suppressWarnings(as.numeric(old_chr))
    lost <- sum(!is.na(old_chr) & old_chr != "" & is.na(new))

    if (lost > 0) {
      ck_note(
        "'", v, "': ", lost,
        " non-numeric value(s) became NA when coercing to numeric",
        verbose = verbose
      )
    }

    dataset[[v]] <- new
  }

  dataset
}


#' Who Selected Which Choice
#'
#' The single place that decides whether a select_multiple cell counts as a
#' selection, so "selected" can never mean two things in one run. Two export
#' styles are detected per column:
#'
#' \describe{
#'   \item{\code{"label"}}{MMC 4Mi ONA style - the cell holds the choice label
#'     when selected and is blank otherwise, so any non-blank value is a
#'     selection. This is the only safe rule for these exports: real choice
#'     labels include "None", "No" and "Refused", and treating those strings as
#'     negatives would zero out genuine answers.}
#'   \item{\code{"dummy"}}{The cell holds 0/1 or TRUE/FALSE, so only 1 / TRUE
#'     is a selection.}
#' }
#'
#' Detection is conservative: a column is a dummy only when \emph{every}
#' non-blank value in it is one of 0, 1, TRUE, FALSE. No masking is applied.
#'
#' @param dataset A dataframe.
#' @param parent The select_multiple parent variable name.
#' @param sm_separator Separator between parent and choice. Default \code{"/"}.
#' @param sm_child_style \code{"auto"} (default), \code{"label"} or
#'   \code{"dummy"}.
#' @param exclude_choices Optional choice labels to leave out entirely.
#' @param ignore_case Logical. Match \code{exclude_choices} case-insensitively.
#' @return \code{NULL} if the parent has no children, otherwise a list with
#'   \code{selected} (logical matrix), \code{columns}, \code{suffixes} and
#'   \code{styles}.
#' @export
ck_sm_selection_matrix <- function(dataset,
                                   parent,
                                   sm_separator = "/",
                                   sm_child_style = c("auto", "label", "dummy"),
                                   exclude_choices = NULL,
                                   ignore_case = TRUE) {
  sm_child_style <- match.arg(sm_child_style)

  suffixes <- ck_sm_child_suffixes(parent, names(dataset), sm_separator)
  if (length(suffixes) == 0) return(NULL)

  if (length(exclude_choices) > 0) {
    norm <- function(z) {
      z <- trimws(as.character(z))
      if (isTRUE(ignore_case)) tolower(z) else z
    }
    suffixes <- suffixes[!norm(unname(suffixes)) %in% norm(exclude_choices)]
    if (length(suffixes) == 0) return(NULL)
  }

  cols <- names(suffixes)
  dummy_tokens <- c("0", "1", "true", "false")

  selected <- matrix(
    FALSE,
    nrow = nrow(dataset), ncol = length(cols),
    dimnames = list(NULL, cols)
  )
  styles <- character(length(cols))

  for (j in seq_along(cols)) {
    v <- trimws(as.character(dataset[[cols[j]]]))
    is_filled <- !is.na(v) & v != ""

    style <- sm_child_style
    if (style == "auto") {
      observed <- unique(tolower(v[is_filled]))
      style <- if (length(observed) > 0 && all(observed %in% dummy_tokens)) {
        "dummy"
      } else {
        "label"
      }
    }
    styles[j] <- style

    selected[, j] <- if (style == "dummy") {
      is_filled & tolower(v) %in% c("1", "true")
    } else {
      is_filled
    }
  }

  list(
    selected = selected,
    columns = cols,
    suffixes = unname(suffixes),
    styles = styles
  )
}


#' Check That Every Variable Passed to count_selections Is a Select Multiple
#'
#' Counting how many choices a respondent picked is meaningless for a single
#' select, so this fails loudly rather than silently returning a column of ones.
#'
#' @param count_selections Character vector of variable names to check.
#' @param dataset The dataset (used for the child columns).
#' @param loa The list of analyses (used for declared types).
#' @param sm_separator Separator between parent and choice. Default \code{"/"}.
#' @return The validated variable names, invisibly. Errors otherwise.
#' @export
ck_check_count_selections <- function(count_selections,
                                      dataset,
                                      loa = NULL,
                                      sm_separator = "/") {
  if (length(count_selections) == 0) {
    return(invisible(character(0)))
  }

  vars <- unique(trimws(as.character(count_selections)))
  vars <- vars[!is.na(vars) & vars != ""]

  declared <- character(0)
  if (!is.null(loa) && all(c("analysis_type", "analysis_var") %in% names(loa))) {
    keep <- !is.na(loa$analysis_type) &
      tolower(loa$analysis_type) %in% c("prop_select_one", "mean", "median", "ratio")
    declared <- stats::setNames(
      tolower(as.character(loa$analysis_type))[keep],
      as.character(loa$analysis_var)[keep]
    )
  }

  problems <- character(0)

  for (v in vars) {
    if (v %in% names(declared)) {
      problems <- c(problems, paste0(
        "'", v, "' is declared as ", declared[[v]],
        " in the LOA, which is not a select_multiple"
      ))
    } else if (length(ck_sm_child_suffixes(v, names(dataset), sm_separator)) == 0) {
      problems <- c(problems, paste0(
        "'", v, "' has no '", v, sm_separator,
        "...' child columns in the export, so it is not a select_multiple",
        if (v %in% names(dataset)) {
          " (it looks like a single select or a plain variable)"
        } else {
          " (and the column itself is absent)"
        }
      ))
    }
  }

  if (length(problems) > 0) {
    stop(
      paste0(
        "count_selections only applies to select_multiple questions.\n  - ",
        paste(problems, collapse = "\n  - ")
      ),
      call. = FALSE
    )
  }

  invisible(vars)
}


#' Match Choice Labels Against the Real Child Columns
#'
#' The choice labels handed to `count_combinations` are matched against the
#' actual select_multiple child column suffixes, after trimming and (by default)
#' case-insensitively. 4Mi labels are long free text - "Armed conflict,
#' generalised violence, and insecurity" - so a silent miss would produce a
#' table that looks fine and is wrong. This returns the empty set for a label
#' that matches nothing, and the caller is expected to fail on it.
#'
#' @param parent The select_multiple parent variable name.
#' @param wanted Character vector of choice labels to find.
#' @param data_names Column names of the dataset.
#' @param sm_separator Separator between parent and choice. Default \code{"/"}.
#' @param ignore_case Logical. Match case-insensitively. Default \code{TRUE}.
#'
#' @return A list the same length as \code{wanted}, each element the child
#'   column name(s) matching that label.
#' @keywords internal
ck_resolve_choices <- function(parent,
                               wanted,
                               data_names,
                               sm_separator = "/",
                               ignore_case = TRUE) {
  suffixes <- ck_sm_child_suffixes(parent, data_names, sm_separator)

  norm <- function(z) {
    z <- trimws(as.character(z))
    if (isTRUE(ignore_case)) tolower(z) else z
  }
  available <- norm(unname(suffixes))

  lapply(wanted, function(w) names(suffixes)[available == norm(w)])
}


#' Check a count_combinations Specification
#'
#' Validates the whole specification before any work is done, so a mistyped
#' choice label is the first thing the user sees rather than a wrong table.
#' Checks the structure, that every question really is a select_multiple, that
#' every choice label resolves to a child column, and that no focus choice is
#' also in \code{exclude_choices} (which would be contradictory: one argument
#' asks to report on a choice, the other to drop everyone who picked it).
#'
#' @param combinations The \code{count_combinations} specification.
#' @param dataset The dataset (used for the child columns).
#' @param loa The list of analyses (used for declared types).
#' @param sm_separator Separator between parent and choice. Default \code{"/"}.
#' @param ignore_case Logical. Match labels case-insensitively.
#' @param exclude_choices The run's \code{exclude_choices}, if any.
#' @param max_choices Refuse more than this many focus choices per question, so
#'   a long list cannot silently produce hundreds of rows. Default \code{6}
#'   (64 combinations).
#'
#' @return The normalised specification, invisibly: a named list of named
#'   character vectors, names being the display labels. Errors otherwise.
#' @export
ck_check_choice_combinations <- function(combinations,
                                         dataset,
                                         loa = NULL,
                                         sm_separator = "/",
                                         ignore_case = TRUE,
                                         exclude_choices = NULL,
                                         max_choices = 6) {
  if (length(combinations) == 0) {
    return(invisible(list()))
  }

  if (!is.list(combinations) ||
      is.null(names(combinations)) ||
      any(is.na(names(combinations))) ||
      any(!nzchar(names(combinations)))) {
    stop(
      paste0(
        "count_combinations must be a named list - one element per question, ",
        "named after the select_multiple parent. For example:\n",
        "  count_combinations = list(\n",
        "    Q78 = c(\n",
        "      Economic = \"Economic reasons\",\n",
        "      Conflict = \"Armed conflict, generalised violence, and insecurity\"\n",
        "    )\n",
        "  )"
      ),
      call. = FALSE
    )
  }

  norm <- function(z) {
    z <- trimws(as.character(z))
    if (isTRUE(ignore_case)) tolower(z) else z
  }
  excluded <- norm(exclude_choices)

  declared <- character(0)
  if (!is.null(loa) && all(c("analysis_type", "analysis_var") %in% names(loa))) {
    keep <- !is.na(loa$analysis_type) &
      tolower(loa$analysis_type) %in% c("prop_select_one", "mean", "median", "ratio")
    declared <- stats::setNames(
      tolower(as.character(loa$analysis_type))[keep],
      as.character(loa$analysis_var)[keep]
    )
  }

  problems <- character(0)
  out <- list()

  for (p in names(combinations)) {
    wanted <- combinations[[p]]
    display <- names(wanted)
    wanted <- trimws(as.character(wanted))

    keep_w <- !is.na(wanted) & wanted != ""
    wanted <- wanted[keep_w]
    if (!is.null(display)) display <- display[keep_w]

    # An unnamed entry falls back to the full ONA label as its display name.
    if (is.null(display)) {
      display <- wanted
    } else {
      display[is.na(display) | !nzchar(display)] <- wanted[is.na(display) | !nzchar(display)]
    }

    if (length(wanted) == 0) {
      problems <- c(problems, paste0("'", p, "' has no choice labels to combine"))
      next
    }
    if (length(wanted) > max_choices) {
      problems <- c(problems, paste0(
        "'", p, "' lists ", length(wanted), " choices, which would give 2^",
        length(wanted), " = ", 2^length(wanted),
        " combination rows. The limit is ", max_choices,
        " - raise max_choices only if you really want that many rows"
      ))
      next
    }

    suffixes <- unname(ck_sm_child_suffixes(p, names(dataset), sm_separator))

    if (p %in% names(declared)) {
      problems <- c(problems, paste0(
        "'", p, "' is declared as ", declared[[p]],
        " in the LOA, which is not a select_multiple"
      ))
      next
    }
    if (length(suffixes) == 0) {
      problems <- c(problems, paste0(
        "'", p, "' has no '", p, sm_separator,
        "...' child columns in the export, so it is not a select_multiple"
      ))
      next
    }

    hits <- ck_resolve_choices(p, wanted, names(dataset), sm_separator, ignore_case)
    missed <- wanted[vapply(hits, length, integer(1)) == 0]

    if (length(missed) > 0) {
      # Offer the labels that share an opening, then fall back to listing what
      # is actually there - the usual cause is a comma or an apostrophe.
      near <- unlist(lapply(missed, function(w) {
        stem <- substr(norm(w), 1, 10)
        suffixes[startsWith(norm(suffixes), stem)]
      }), use.names = FALSE)
      near <- unique(near)

      problems <- c(problems, paste0(
        "'", p, "': choice label(s) not found in the export: ",
        paste0("\"", missed, "\"", collapse = ", "), ".",
        if (length(near) > 0) {
          paste0(" Did you mean: ", paste0("\"", near, "\"", collapse = ", "), "?")
        } else {
          paste0(
            " Available choices are: ",
            paste0("\"", utils::head(suffixes, 20), "\"", collapse = ", "),
            if (length(suffixes) > 20) ", ..." else ""
          )
        }
      ))
      next
    }

    clash <- wanted[norm(wanted) %in% excluded]
    if (length(clash) > 0) {
      problems <- c(problems, paste0(
        "'", p, "': ", paste0("\"", clash, "\"", collapse = ", "),
        " appears in both count_combinations and exclude_choices. ",
        "exclude_choices drops every respondent who picked it, so there would ",
        "be nobody left to report the combination for. Remove it from one of them"
      ))
      next
    }

    if (any(duplicated(norm(wanted)))) {
      problems <- c(problems, paste0(
        "'", p, "': the same choice is listed more than once"
      ))
      next
    }

    if (any(duplicated(display))) {
      problems <- c(problems, paste0(
        "'", p, "': two choices share the same display name (",
        paste(unique(display[duplicated(display)]), collapse = ", "),
        "). Row labels are built from these, so they must be distinct"
      ))
      next
    }

    out[[p]] <- stats::setNames(wanted, display)
  }

  if (length(problems) > 0) {
    stop(
      paste0("count_combinations is not usable.\n  - ", paste(problems, collapse = "\n  - ")),
      call. = FALSE
    )
  }

  invisible(out)
}


#' Report Which Combination of Choices Each Respondent Selected
#'
#' For each question in \code{combinations}, adds a derived categorical column
#' recording which of the *chosen* choices that respondent selected, ignoring
#' everything else they selected. With k choices of interest every respondent
#' falls into exactly one of 2^k groups, so the categories are mutually
#' exclusive and exhaustive and the percentages add to 100\%.
#'
#' For \code{c(Economic = "Economic reasons", Conflict = "Armed conflict, ...")}
#' that is four rows: \emph{Economic + Conflict}, \emph{Economic} (selected
#' Economic and not Conflict, whatever else they selected), \emph{Conflict}, and
#' \emph{None of these}.
#'
#' Because the result is an ordinary categorical column it then flows through
#' the normal analysis - overall and across every grouping variable - with no
#' special casing downstream.
#'
#' \strong{Denominator.} Only respondents who answered the question are counted:
#' the parent column is non-blank, or at least one child is selected. Anyone who
#' was never asked, or asked and left it blank, is \code{NA} and drops out - so
#' \emph{None of these} means "answered, but picked none of the listed choices",
#' not "did not answer". When \code{exclude_choices} is in play, respondents who
#' picked an excluded choice also drop out, which keeps this base identical to
#' the one behind the question's own choice percentages.
#'
#' Run this \emph{before} \code{\link{ck_sm_children_to_binary}}: the pattern is
#' taken from the raw selections, before the not-asked mask is applied.
#'
#' @param dataset A dataframe (label row already removed).
#' @param combinations A named list: names are select_multiple parents, values
#'   are the choice labels of interest. Name the choices to get short display
#'   labels (\code{c(Economic = "Economic reasons")}); leave them unnamed and the
#'   full ONA label is used.
#' @param sm_separator Separator between parent and choice. Default \code{"/"}.
#' @param sm_child_style \code{"auto"} (default), \code{"label"}, \code{"dummy"}.
#' @param exclude_choices Optional choice labels whose pickers leave the base.
#' @param ignore_case Logical. Match labels case-insensitively. Default
#'   \code{TRUE}.
#' @param none_label Row label for respondents who selected none of the listed
#'   choices. Default \code{"None of these"}.
#' @param joiner Placed between the display labels of a multi-choice
#'   combination. Default \code{" + "}.
#' @param order Row order. \code{"descending"} (default) puts the largest
#'   combinations first, so the "both" row leads and \code{none_label} closes.
#'   \code{"ascending"} reverses it.
#' @param suffix Appended to the variable name to make the derived column name.
#' @param verbose Logical. Default \code{TRUE}.
#'
#' @return A list with \code{dataset} (the derived columns added) and \code{map}
#'   (a dataframe of \code{analysis_var}, \code{combination_column},
#'   \code{n_choices}, \code{n_combinations} and \code{n_in_base}).
#' @export
ck_add_choice_combinations <- function(dataset,
                                       combinations,
                                       sm_separator = "/",
                                       sm_child_style = c("auto", "label", "dummy"),
                                       exclude_choices = NULL,
                                       ignore_case = TRUE,
                                       none_label = "None of these",
                                       joiner = " + ",
                                       order = c("descending", "ascending"),
                                       suffix = "_choice_combination",
                                       verbose = TRUE) {
  sm_child_style <- match.arg(sm_child_style)
  order <- match.arg(order)

  empty_map <- data.frame(
    analysis_var = character(0),
    combination_column = character(0),
    n_choices = integer(0),
    n_combinations = integer(0),
    n_in_base = integer(0),
    stringsAsFactors = FALSE
  )

  if (length(combinations) == 0) {
    return(list(dataset = dataset, map = empty_map))
  }

  norm <- function(z) {
    z <- trimws(as.character(z))
    if (isTRUE(ignore_case)) tolower(z) else z
  }
  excluded <- norm(exclude_choices)

  map_rows <- list()

  for (p in names(combinations)) {
    wanted <- as.character(combinations[[p]])
    display <- names(combinations[[p]])
    if (is.null(display)) display <- wanted
    display[is.na(display) | !nzchar(display)] <- wanted[is.na(display) | !nzchar(display)]

    k <- length(wanted)
    if (k == 0) next

    # The full child matrix, with no exclusions applied: which choices are of
    # interest is decided below, and the base needs to know about the excluded
    # ones too.
    m <- ck_sm_selection_matrix(dataset, p, sm_separator, sm_child_style)
    if (is.null(m)) {
      ck_warn("'", p, "' has no select_multiple child columns. Skipped.")
      next
    }

    hits <- ck_resolve_choices(p, wanted, names(dataset), sm_separator, ignore_case)
    if (any(vapply(hits, length, integer(1)) == 0)) {
      ck_warn(
        "'", p, "': ", sum(vapply(hits, length, integer(1)) == 0),
        " choice label(s) matched no child column. Skipped - run ",
        "ck_check_choice_combinations() to see which."
      )
      next
    }

    # --- who is in the base --------------------------------------------------
    selected_any <- rowSums(m$selected) > 0

    if (p %in% names(dataset)) {
      parent_chr <- trimws(as.character(dataset[[p]]))
      in_base <- (!is.na(parent_chr) & parent_chr != "") | selected_any
    } else {
      in_base <- selected_any
      ck_warn(
        "Parent column '", p, "' is not in the export, so a respondent counts as ",
        "having answered only if one of their child columns is filled. Anyone ",
        "asked who selected nothing cannot be told apart from anyone never asked."
      )
    }

    # Match ck_exclude_choices: a respondent who picked an excluded choice
    # leaves the question's denominator entirely, so they leave this base too.
    n_excluded_out <- 0L
    if (length(excluded) > 0) {
      excl_cols <- m$columns[norm(m$suffixes) %in% excluded]
      if (length(excl_cols) > 0) {
        picked_excluded <- rowSums(m$selected[, excl_cols, drop = FALSE]) > 0
        n_excluded_out <- sum(in_base & picked_excluded)
        in_base <- in_base & !picked_excluded
      }
    }

    # --- which of the focus choices each respondent selected -----------------
    flags <- matrix(FALSE, nrow = nrow(dataset), ncol = k)
    for (j in seq_len(k)) {
      flags[, j] <- rowSums(m$selected[, hits[[j]], drop = FALSE]) > 0
    }

    # Every subset of the k focus choices, largest first, then in the order the
    # choices were given. combn() is lexicographic, so this is deterministic.
    sets <- unlist(
      lapply(seq(k, 0), function(size) {
        if (size == 0) list(integer(0)) else utils::combn(k, size, simplify = FALSE)
      }),
      recursive = FALSE
    )
    if (order == "ascending") sets <- rev(sets)

    set_labels <- vapply(
      sets,
      function(idx) {
        if (length(idx) == 0) none_label else paste(display[idx], collapse = joiner)
      },
      character(1)
    )

    if (any(duplicated(set_labels))) {
      stop(
        paste0(
          "'", p, "': two combination rows would carry the same label (",
          paste(unique(set_labels[duplicated(set_labels)]), collapse = "; "),
          "). Give the choices distinct short names, e.g. ",
          "c(Economic = \"...\", Conflict = \"...\"), or change joiner."
        ),
        call. = FALSE
      )
    }

    # A bit weight per focus choice turns the selection pattern into one integer
    # per respondent, which indexes straight into the label vector.
    weights <- 2^(seq_len(k) - 1)
    respondent_key <- as.vector(flags %*% weights)
    set_key <- vapply(sets, function(idx) sum(weights[idx]), numeric(1))

    value <- set_labels[match(respondent_key, set_key)]
    value[!in_base] <- NA_character_

    combination_col <- paste0(p, suffix)
    # A factor keeps the rows in the intended order and keeps an empty
    # combination visible as a zero instead of dropping the row entirely.
    dataset[[combination_col]] <- factor(value, levels = set_labels)

    map_rows[[length(map_rows) + 1]] <- data.frame(
      analysis_var = p,
      combination_column = combination_col,
      n_choices = k,
      n_combinations = length(sets),
      n_in_base = sum(in_base),
      stringsAsFactors = FALSE
    )

    ck_note(
      "'", p, "': ", k, " choice(s) -> ", length(sets), " combination(s) over ",
      sum(in_base), " respondent(s) who answered",
      if (n_excluded_out > 0) {
        paste0(" (", n_excluded_out, " dropped by exclude_choices)")
      } else {
        ""
      },
      verbose = verbose
    )
  }

  list(
    dataset = dataset,
    map = if (length(map_rows) > 0) dplyr::bind_rows(map_rows) else empty_map
  )
}


#' Combine the Derived-Column Maps into One
#'
#' The selection counts and the choice combinations are both derived categorical
#' columns that ride through the LOA as plain proportions and are renamed
#' afterwards. Everything downstream - the LOA append, the rename, the block
#' positioning, the separator rows - works off this single table so neither
#' feature needs its own branch.
#'
#' @param count_map The \code{map} from \code{\link{ck_add_selection_counts}}.
#' @param combination_map The \code{map} from
#'   \code{\link{ck_add_choice_combinations}}.
#'
#' @return A dataframe of \code{analysis_var}, \code{derived_column} and
#'   \code{analysis_type}.
#' @keywords internal
ck_derived_map <- function(count_map = NULL, combination_map = NULL) {
  parts <- list()

  if (!is.null(count_map) && nrow(count_map) > 0) {
    parts[[length(parts) + 1]] <- data.frame(
      analysis_var = count_map$analysis_var,
      derived_column = count_map$count_column,
      analysis_type = "count_select_multiple",
      stringsAsFactors = FALSE
    )
  }

  if (!is.null(combination_map) && nrow(combination_map) > 0) {
    parts[[length(parts) + 1]] <- data.frame(
      analysis_var = combination_map$analysis_var,
      derived_column = combination_map$combination_column,
      analysis_type = "combination_select_multiple",
      stringsAsFactors = FALSE
    )
  }

  if (length(parts) == 0) {
    return(data.frame(
      analysis_var = character(0),
      derived_column = character(0),
      analysis_type = character(0),
      stringsAsFactors = FALSE
    ))
  }

  dplyr::bind_rows(parts)
}


#' Count How Many Choices Each Respondent Selected
#'
#' Adds a derived categorical column per question recording how many choices
#' that respondent picked: none, exactly one, or more than one. Because it is an
#' ordinary categorical column it then flows through the normal analysis -
#' overall and across every grouping variable - with no special casing
#' downstream.
#'
#' Run this \emph{before} \code{\link{ck_sm_children_to_binary}}: the counts are
#' taken from the raw selection pattern, so that "no choice selected" is a real
#' category rather than a group the not-asked mask has already removed.
#'
#' \strong{Read the "no choice selected" figure carefully.} It counts
#' respondents with nothing recorded, which lumps together those never asked
#' the question (a relevance condition sent them past it) and those asked who
#' left it blank. If the question was asked of everyone the two are the same
#' thing; if it was conditional, this is "not answered", not "declined to
#' answer". A filled parent with no child selected is a genuine inconsistency
#' and is warned about.
#'
#' @param dataset A dataframe (label row already removed).
#' @param count_selections Character vector of select_multiple parents.
#' @param sm_separator Separator between parent and choice. Default \code{"/"}.
#' @param sm_child_style \code{"auto"} (default), \code{"label"}, \code{"dummy"}.
#' @param exclude_choices Optional choice labels not to count towards the total,
#'   so that e.g. "Refused" does not read as "selected one choice".
#' @param ignore_case Logical. Match \code{exclude_choices} case-insensitively.
#' @param mode \code{"grouped"} (default) gives none / one / more than one.
#'   \code{"exact"} gives the exact number selected.
#' @param labels The three category labels for \code{mode = "grouped"}, always
#'   in the order \emph{none, exactly one, more than one} whatever the display
#'   order.
#' @param order Row order. \code{"descending"} (default) puts the largest
#'   selection count first.
#' @param suffix Appended to the variable name to make the derived column name.
#' @param verbose Logical. Default \code{TRUE}.
#' @return A list with \code{dataset} and \code{map} (\code{analysis_var},
#'   \code{count_column}, \code{n_choices_counted}).
#' @export
ck_add_selection_counts <- function(dataset,
                                    count_selections,
                                    sm_separator = "/",
                                    sm_child_style = c("auto", "label", "dummy"),
                                    exclude_choices = NULL,
                                    ignore_case = TRUE,
                                    mode = c("grouped", "exact"),
                                    labels = c(
                                      "No choice selected",
                                      "Selected exactly 1 choice",
                                      "Selected more than 1 choice"
                                    ),
                                    order = c("descending", "ascending"),
                                    suffix = "_selection_count",
                                    verbose = TRUE) {
  sm_child_style <- match.arg(sm_child_style)
  mode <- match.arg(mode)
  order <- match.arg(order)

  empty_map <- data.frame(
    analysis_var = character(0),
    count_column = character(0),
    n_choices_counted = integer(0),
    stringsAsFactors = FALSE
  )

  if (length(count_selections) == 0) {
    return(list(dataset = dataset, map = empty_map))
  }

  if (mode == "grouped" && length(labels) != 3) {
    stop("labels must have exactly three elements: none, one, more than one.", call. = FALSE)
  }

  vars <- unique(trimws(as.character(count_selections)))
  vars <- vars[!is.na(vars) & vars != ""]

  map_rows <- list()

  for (v in vars) {
    m <- ck_sm_selection_matrix(
      dataset, v, sm_separator, sm_child_style,
      exclude_choices = exclude_choices, ignore_case = ignore_case
    )

    if (is.null(m)) {
      ck_warn(
        "No countable child columns left for '", v,
        "' after applying exclude_choices. Skipped."
      )
      next
    }

    n_selected <- rowSums(m$selected)

    # A filled parent with nothing selected is a real inconsistency, not a skip.
    # Saying whether any exported choice label appears in the offending values
    # distinguishes "exclude_choices removed their only pick" from "a choice
    # column is missing from the export".
    if (v %in% names(dataset)) {
      parent_chr <- trimws(as.character(dataset[[v]]))
      odd <- !is.na(parent_chr) & parent_chr != "" & n_selected == 0

      if (any(odd)) {
        examples <- unique(parent_chr[odd])
        all_suffixes <- unname(ck_sm_child_suffixes(v, names(dataset), sm_separator))
        any_label_seen <- any(vapply(
          examples,
          function(p) any(vapply(all_suffixes, grepl, logical(1), x = p, fixed = TRUE)),
          logical(1),
          USE.NAMES = FALSE
        ))

        diagnosis <- if (!any_label_seen) {
          paste0(
            " None of the exported '", v, sm_separator, "...' choice labels ",
            "appear in these values, so a choice column is probably missing ",
            "from the export."
          )
        } else if (length(exclude_choices) > 0) {
          " Their only selected choice was probably removed by exclude_choices."
        } else {
          paste0(
            " The choice label is present in '", v,
            "' but its child column is blank - check the export."
          )
        }

        ck_warn(
          sum(odd), " row(s) have a value in '", v,
          "' but no child column selected; counted as \"", labels[1], "\".",
          diagnosis, " Example value(s): ",
          paste0("\"", utils::head(examples, 3), "\"", collapse = ", "),
          if (length(examples) > 3) paste0(" (and ", length(examples) - 3, " more)") else ""
        )
      }
    }

    count_col <- paste0(v, suffix)

    # A factor keeps the categories in reading order rather than alphabetical
    # order, and keeps an empty category visible as a zero instead of dropping
    # the row.
    if (mode == "grouped") {
      bucket <- ifelse(n_selected == 0, labels[1], ifelse(n_selected == 1, labels[2], labels[3]))
      lv <- if (order == "descending") rev(labels) else labels
      dataset[[count_col]] <- factor(bucket, levels = lv)
    } else {
      steps <- seq(0, max(n_selected))
      lv <- as.character(if (order == "descending") rev(steps) else steps)
      dataset[[count_col]] <- factor(as.character(n_selected), levels = lv)
    }

    map_rows[[length(map_rows) + 1]] <- data.frame(
      analysis_var = v,
      count_column = count_col,
      n_choices_counted = length(m$columns),
      stringsAsFactors = FALSE
    )

    ck_note(
      "'", v, "': counting selections over ", length(m$columns),
      " choice(s); none = ", sum(n_selected == 0),
      ", one = ", sum(n_selected == 1),
      ", more than one = ", sum(n_selected > 1),
      verbose = verbose
    )
  }

  list(
    dataset = dataset,
    map = if (length(map_rows) > 0) dplyr::bind_rows(map_rows) else empty_map
  )
}


#' Convert Select Multiple Child Columns to 0/1 Dummies
#'
#' \code{analysistools} estimates a select_multiple proportion as the weighted
#' mean of each child dummy, so the children must be numeric 0/1 with \code{NA}
#' for respondents never asked the question. Selection detection is
#' \code{\link{ck_sm_selection_matrix}}.
#'
#' Who was asked is taken from the concatenated parent column when present
#' (non-blank parent = asked), unioned with "selected at least one child" so a
#' blank parent cell cannot delete a real selection - and because in a
#' dummy-style export a never-asked row can be filled with zeros. When the
#' parent column is absent every row is treated as asked, which inflates
#' \code{n_total}; a warning is issued.
#'
#' A respondent who was asked but selected nothing has a blank parent and blank
#' children and is therefore excluded from the denominator. For the 4Mi tools
#' that is correct: "None", "Don't know" and "Refused" are explicit choices, so
#' a genuinely answered question is never entirely blank.
#'
#' @param dataset A dataframe.
#' @param parents Character vector of select_multiple parent variables.
#' @param sm_separator Separator between parent and choice. Default \code{"/"}.
#' @param sm_child_style \code{"auto"} (default), \code{"label"} or
#'   \code{"dummy"}.
#' @param verbose Logical. Default \code{TRUE}.
#' @return The dataframe with the child columns as numeric 0/1/NA.
#' @export
ck_sm_children_to_binary <- function(dataset,
                                     parents,
                                     sm_separator = "/",
                                     sm_child_style = c("auto", "label", "dummy"),
                                     verbose = TRUE) {
  sm_child_style <- match.arg(sm_child_style)

  data_names <- names(dataset)
  parents <- unique(parents[!is.na(parents)])

  for (p in parents) {
    m <- ck_sm_selection_matrix(dataset, p, sm_separator, sm_child_style)
    if (is.null(m)) next

    any_selected <- rowSums(m$selected) > 0

    if (p %in% data_names) {
      parent_chr <- trimws(as.character(dataset[[p]]))
      asked <- (!is.na(parent_chr) & parent_chr != "") | any_selected
    } else {
      asked <- rep(TRUE, nrow(dataset))
      ck_warn(
        "Parent column '", p, "' is not in the export, so every row is treated ",
        "as having been asked this select_multiple. n_total may be inflated. ",
        "Consider recreate_sm_parents = TRUE or exporting the parent column."
      )
    }

    for (j in seq_along(m$columns)) {
      out <- as.numeric(m$selected[, j])
      out[!asked] <- NA_real_
      dataset[[m$columns[j]]] <- out
    }

    ck_note(
      "'", p, "': ", length(m$columns),
      " select_multiple child column(s) converted to 0/1 (style: ",
      paste(sort(unique(m$styles)), collapse = "+"), "); ",
      sum(asked), " of ", length(asked), " rows in the denominator",
      verbose = verbose
    )
  }

  dataset
}


#' Repair Truncated Select Multiple Choice Values
#'
#' Depending on the \code{analysistools} version, the reported
#' \code{analysis_var_value} may be derived by \emph{splitting} the child column
#' name on the separator rather than by removing the parent prefix, which
#' truncates a label that contains the separator (\code{"QN9/Interception at
#' sea/pull-back..."} becomes \code{"Interception at sea"}). A value is replaced
#' only when exactly one child suffix starts with it, so a correct value is
#' never altered and an ambiguous one is left alone with a warning.
#'
#' Only reachable via the survey engine - \code{\link{ck_fast_analysis}} takes
#' the suffix by prefix removal and never truncates.
#'
#' @param results_table A long results table.
#' @param dataset The analysis dataset, for the real child column names.
#' @param sm_separator Separator between parent and choice. Default \code{"/"}.
#' @param verbose Logical. Default \code{TRUE}.
#' @return \code{results_table} with select_multiple choice values repaired.
#' @export
ck_fix_sm_choice_values <- function(results_table,
                                    dataset,
                                    sm_separator = "/",
                                    verbose = TRUE) {
  needed <- c("analysis_type", "analysis_var", "analysis_var_value")
  if (!all(needed %in% names(results_table))) {
    return(results_table)
  }

  is_sm <- !is.na(results_table$analysis_type) &
    tolower(as.character(results_table$analysis_type)) == "prop_select_multiple"

  if (!any(is_sm)) {
    return(results_table)
  }

  values <- as.character(results_table$analysis_var_value)
  parents <- as.character(results_table$analysis_var)

  n_fixed <- 0
  unresolved <- character(0)

  for (p in unique(parents[is_sm])) {
    suffixes <- unname(ck_sm_child_suffixes(p, names(dataset), sm_separator))
    if (length(suffixes) == 0) next

    for (r in which(is_sm & parents == p)) {
      v <- values[r]
      if (is.na(v) || v %in% suffixes) next

      hits <- suffixes[startsWith(suffixes, v)]

      if (length(hits) == 1) {
        values[r] <- hits
        n_fixed <- n_fixed + 1
      } else {
        unresolved <- c(unresolved, paste0(p, sm_separator, v))
      }
    }
  }

  if (n_fixed > 0) {
    ck_note(
      "repaired ", n_fixed,
      " truncated select_multiple choice value(s) from the child column names",
      verbose = verbose
    )
    results_table$analysis_var_value <- values
  }

  if (length(unresolved) > 0) {
    ck_warn(
      "Could not match these select_multiple choice values to a child column: ",
      paste(unique(unresolved), collapse = "; "),
      ". Check the sm_separator and the choice labels."
    )
  }

  results_table
}


#' Exclude Non-Substantive Choices from an Analysis
#'
#' Removes choices such as "Don't know" and "Refused" so that percentages are
#' reported over respondents who gave a substantive answer. This is deliberately
#' a change to the \emph{denominator}, not a filter on the output rows: dropping
#' the rows afterwards would leave the excluded respondents in every other
#' choice's denominator, which is the opposite of "of those who answered".
#'
#' \describe{
#'   \item{select_one}{The cell is set to \code{NA}, so the respondent leaves
#'     the denominator entirely.}
#'   \item{select_multiple}{A respondent who selected an excluded choice has
#'     \emph{all} children of that question set to \code{NA} and so leaves the
#'     denominator. The excluded child columns are then blanked so they produce
#'     no result row.}
#' }
#'
#' Grouping variables are left untouched - excluding a category from a
#' disaggregation changes which respondents appear in the table at all, so
#' filter the dataset yourself if that is what you want.
#'
#' @param dataset The analysis dataset. For select_multiple, run this
#'   \emph{after} \code{\link{ck_sm_children_to_binary}}.
#' @param loa The list of analyses, used to find the variables to act on.
#' @param exclude_choices Character vector of choice labels, matched after
#'   trimming. \code{NULL} (default) does nothing.
#' @param sm_separator Separator between parent and choice. Default \code{"/"}.
#' @param ignore_case Logical. Match case-insensitively. Default \code{TRUE}.
#' @param verbose Logical. Default \code{TRUE}.
#' @return A list with \code{dataset} and \code{excluded} (the audit trail).
#' @export
ck_exclude_choices <- function(dataset,
                               loa,
                               exclude_choices = NULL,
                               sm_separator = "/",
                               ignore_case = TRUE,
                               verbose = TRUE) {
  empty_log <- data.frame(
    analysis_var = character(0),
    analysis_type = character(0),
    choice = character(0),
    n_respondents_removed = integer(0),
    stringsAsFactors = FALSE
  )

  exclude_choices <- trimws(as.character(exclude_choices))
  exclude_choices <- exclude_choices[!is.na(exclude_choices) & exclude_choices != ""]

  if (length(exclude_choices) == 0) {
    return(list(dataset = dataset, excluded = empty_log))
  }

  norm <- function(x) if (isTRUE(ignore_case)) tolower(x) else x
  targets <- norm(exclude_choices)

  log_rows <- list()
  seen <- character(0)

  loa_vars <- function(type) {
    v <- unique(loa$analysis_var[
      !is.na(loa$analysis_type) & tolower(loa$analysis_type) == type
    ])
    v[!is.na(v)]
  }

  add_log <- function(var, type, choice, n) {
    seen <<- c(seen, norm(trimws(choice)))
    log_rows[[length(log_rows) + 1]] <<- data.frame(
      analysis_var = var,
      analysis_type = type,
      choice = choice,
      n_respondents_removed = n,
      stringsAsFactors = FALSE
    )
  }

  # --- select_one ------------------------------------------------------------
  for (v in intersect(loa_vars("prop_select_one"), names(dataset))) {
    values <- trimws(as.character(dataset[[v]]))
    hit <- !is.na(values) & norm(values) %in% targets

    if (any(hit)) {
      for (ch in unique(values[hit])) {
        add_log(v, "prop_select_one", ch, sum(!is.na(values) & values == ch))
      }
      dataset[[v]][hit] <- NA
    }
  }

  # --- select_multiple -------------------------------------------------------
  for (p in loa_vars("prop_select_multiple")) {
    suffixes <- ck_sm_child_suffixes(p, names(dataset), sm_separator)
    if (length(suffixes) == 0) next

    drop_cols <- names(suffixes)[norm(trimws(unname(suffixes))) %in% targets]
    if (length(drop_cols) == 0) next

    # A respondent who picked an excluded choice leaves the denominator.
    picked <- rep(FALSE, nrow(dataset))
    for (cc in drop_cols) {
      x <- suppressWarnings(as.numeric(dataset[[cc]]))
      picked <- picked | (!is.na(x) & x == 1)
    }

    for (cc in drop_cols) {
      add_log(p, "prop_select_multiple", unname(suffixes[cc]), sum(picked))
    }

    for (cc in names(suffixes)) {
      dataset[[cc]][picked] <- NA
    }

    # Blank the excluded children so no result row is produced for them.
    for (cc in drop_cols) {
      dataset[[cc]] <- NA_real_
    }
  }

  never_found <- exclude_choices[!norm(exclude_choices) %in% unique(seen)]
  if (length(never_found) > 0) {
    ck_warn(
      "exclude_choices value(s) not found in any analysis variable: ",
      paste(never_found, collapse = "; "),
      ". Check the exact label, including punctuation and apostrophes."
    )
  }

  excluded <- if (length(log_rows) > 0) dplyr::bind_rows(log_rows) else empty_log

  if (nrow(excluded) > 0) {
    ck_note(
      "excluded ", length(unique(excluded$choice)), " choice(s) from ",
      length(unique(excluded$analysis_var)), " variable(s): ",
      paste(unique(excluded$choice), collapse = ", "),
      verbose = verbose
    )
  }

  list(dataset = dataset, excluded = excluded)
}


#' Build a Survey Design with Optional Weights and Strata
#'
#' Both are optional. With neither, the dataset is used as it is - an unweighted
#' SRS design where \code{n_w} equals \code{n}.
#'
#' @param dataset The analysis dataset (label row already removed).
#' @param weight_column Optional weights column name. Default \code{NULL}.
#' @param strata_column Optional strata column name. Default \code{NULL}.
#' @param verbose Logical. Default \code{TRUE}.
#' @return An \pkg{srvyr} \code{tbl_svy} object.
#' @export
ck_build_survey_design <- function(dataset,
                                   weight_column = NULL,
                                   strata_column = NULL,
                                   verbose = TRUE) {
  ck_require_pkg("srvyr")

  usable <- function(col, what) {
    ok <- !is.null(col) && length(col) == 1 && !is.na(col) && col %in% names(dataset)
    if (!is.null(col) && !ok) {
      ck_warn(
        what, " column '", col, "' not found in the dataset. ",
        if (identical(what, "Weight")) {
          "Running the analysis unweighted."
        } else {
          "Running the analysis without strata."
        }
      )
    }
    ok
  }

  use_weights <- usable(weight_column, "Weight")
  use_strata <- usable(strata_column, "Strata")

  if (use_weights) {
    dataset <- ck_coerce_numeric(dataset, weight_column, verbose = verbose)

    if (all(is.na(dataset[[weight_column]]))) {
      stop(
        paste0("Weight column '", weight_column, "' is entirely NA after coercion."),
        call. = FALSE
      )
    }
    if (any(dataset[[weight_column]] <= 0, na.rm = TRUE)) {
      ck_warn("Some weights are zero or negative.")
    }
  }

  ck_note(
    "survey design: ",
    if (use_weights) "weighted" else "unweighted", ", ",
    if (use_strata) "stratified" else "no strata",
    if (!use_weights && !use_strata) " (dataset used as is)" else "",
    verbose = verbose
  )

  # srvyr uses tidyselect, so the arguments cannot be spliced with do.call()
  # without losing the selection context - hence the explicit branches.
  if (use_weights && use_strata) {
    return(srvyr::as_survey_design(
      dataset,
      weights = dplyr::all_of(weight_column),
      strata = dplyr::all_of(strata_column)
    ))
  }
  if (use_weights) {
    return(srvyr::as_survey_design(dataset, weights = dplyr::all_of(weight_column)))
  }
  if (use_strata) {
    return(srvyr::as_survey_design(dataset, strata = dplyr::all_of(strata_column)))
  }
  srvyr::as_survey_design(dataset)
}


# -----------------------------------------------------------------------------
# 3. LOA handling
# -----------------------------------------------------------------------------

#' Columns analysistools::create_analysis() Understands
#' @keywords internal
ck_loa_columns <- function() {
  c(
    "analysis_type", "analysis_var", "group_var", "level",
    "analysis_var_numerator", "analysis_var_denominator",
    "numerator_NA_to_0", "filter_denominator_0"
  )
}


#' Resolve the Confidence Level for Each LOA Row
#'
#' \code{level} is optional. An empty cell (or a missing column) means no
#' confidence interval is reported for that analysis. \code{analysistools}
#' always requests an interval, so \code{fallback_level} is handed to it as an
#' internal placeholder; the \code{supplied} flag is what the pipeline uses
#' afterwards to blank \code{stat_low} / \code{stat_upp} on those rows, and what
#' routes them to the fast engine. Percentages are accepted: 95 reads as 0.95.
#'
#' @param loa The list of analyses.
#' @param fallback_level Placeholder level for rows with none. Default
#'   \code{0.95}.
#' @return A list with \code{level} (numeric) and \code{supplied} (logical), one
#'   element per row.
#' @export
ck_level_info <- function(loa, fallback_level = 0.95) {
  n <- nrow(loa)

  if (!"level" %in% names(loa)) {
    return(list(level = rep(fallback_level, n), supplied = rep(FALSE, n)))
  }

  raw <- trimws(as.character(loa$level))
  supplied <- !is.na(raw) & raw != "" & raw != "NA"
  level <- rep(fallback_level, n)

  if (any(supplied)) {
    parsed <- suppressWarnings(as.numeric(raw[supplied]))

    if (any(is.na(parsed))) {
      stop(
        paste0(
          "Non-numeric value(s) in the LOA level column: ",
          paste(unique(raw[supplied][is.na(parsed)]), collapse = ", ")
        ),
        call. = FALSE
      )
    }

    as_percent <- parsed > 1 & parsed <= 100
    parsed[as_percent] <- parsed[as_percent] / 100

    if (any(parsed <= 0 | parsed >= 1)) {
      stop(
        paste0(
          "LOA level values must be between 0 and 1 (or 1 and 100 as a ",
          "percentage). Offending value(s): ",
          paste(unique(parsed[parsed <= 0 | parsed >= 1]), collapse = ", ")
        ),
        call. = FALSE
      )
    }

    level[supplied] <- parsed
  }

  list(level = level, supplied = supplied)
}


#' Check Which Analysis Variables Are Usable in an ONA Export
#'
#' A variable is usable if it is a column in the dataset, or if it is a
#' select_multiple parent whose child columns are present. ONA does not always
#' carry the concatenated parent column, so filtering on column names alone
#' silently drops every select_multiple analysis.
#'
#' @param vars Character vector of variable names.
#' @param dataset The analysis dataset.
#' @param sm_separator Select_multiple separator. Default \code{"/"}.
#' @return A logical vector the same length as \code{vars}.
#' @keywords internal
ck_var_is_available <- function(vars, dataset, sm_separator = "/") {
  data_names <- colnames(dataset)

  has_children <- vapply(
    vars,
    function(v) {
      if (is.na(v)) return(FALSE)
      any(startsWith(data_names, paste0(v, sm_separator)))
    },
    logical(1),
    USE.NAMES = FALSE
  )

  (vars %in% data_names) | has_children
}


#' Which LOA Rows Can Be Run Against the Dataset
#'
#' Handles ratios, whose variables live in \code{analysis_var_numerator} /
#' \code{analysis_var_denominator} rather than \code{analysis_var}.
#'
#' @param loa The list of analyses.
#' @param dataset The analysis dataset.
#' @param sm_separator Select_multiple separator.
#' @return A logical vector, one element per LOA row.
#' @keywords internal
ck_loa_is_available <- function(loa, dataset, sm_separator = "/") {
  n <- nrow(loa)
  if (n == 0) return(logical(0))

  is_ratio <- !is.na(loa$analysis_type) & tolower(loa$analysis_type) == "ratio"
  ok <- logical(n)

  if (any(!is_ratio)) {
    ok[!is_ratio] <- ck_var_is_available(loa$analysis_var[!is_ratio], dataset, sm_separator)
  }

  if (any(is_ratio) &&
      all(c("analysis_var_numerator", "analysis_var_denominator") %in% names(loa))) {
    ok[is_ratio] <- loa$analysis_var_numerator[is_ratio] %in% names(dataset) &
      loa$analysis_var_denominator[is_ratio] %in% names(dataset)
  }

  ok
}


#' The analysis_var Value analysistools Will Report for Each LOA Row
#'
#' For ratios, \code{create_analysis()} reports \code{analysis_var} as
#' \code{"numerator \%/\% denominator"}. This reproduces that so LOA metadata
#' can be joined back onto ratio rows too. Verify the separator against your
#' installed analysistools version.
#'
#' @param loa The list of analyses.
#' @return A character vector, one element per LOA row.
#' @keywords internal
ck_loa_var_key <- function(loa) {
  key <- as.character(loa$analysis_var)

  is_ratio <- !is.na(loa$analysis_type) & tolower(loa$analysis_type) == "ratio"

  if (any(is_ratio) &&
      all(c("analysis_var_numerator", "analysis_var_denominator") %in% names(loa))) {
    key[is_ratio] <- paste0(
      loa$analysis_var_numerator[is_ratio], " %/% ",
      loa$analysis_var_denominator[is_ratio]
    )
  }

  key
}


#' Stack One LOA per Grouping Variable into a Single LOA
#'
#' \code{create_analysis()} accepts an LOA whose rows carry different
#' \code{group_var} values, so the whole cross-tab set is estimated in one call
#' instead of looping and joining blocks afterwards - which is what makes the
#' rows align by construction.
#'
#' @param loa The list of analyses.
#' @param dataset The analysis dataset.
#' @param group_variables Grouping variables; \code{overall_label} means the
#'   ungrouped analysis.
#' @param sm_separator Select_multiple separator. Default \code{"/"}.
#' @param overall_label Label for the ungrouped analysis. Default
#'   \code{"Overall"}.
#' @param fallback_level Placeholder level for rows with an empty \code{level}
#'   cell. Default \code{0.95}. See \code{\link{ck_level_info}}.
#' @param verbose Logical. Default \code{TRUE}.
#' @return The stacked LOA, containing only the columns
#'   \code{create_analysis()} understands, with a \code{"level_supplied"}
#'   attribute.
#' @export
ck_stack_loa <- function(loa,
                         dataset,
                         group_variables = c("Overall"),
                         sm_separator = "/",
                         overall_label = "Overall",
                         fallback_level = 0.95,
                         verbose = TRUE) {
  # Resolve level once, on the original LOA, so every block agrees.
  lv <- ck_level_info(loa, fallback_level = fallback_level)
  loa$level <- lv$level
  loa$.ck_level_supplied <- lv$supplied

  blocks <- list()

  for (g in group_variables) {
    block <- loa

    if (identical(g, overall_label)) {
      block$group_var <- NA_character_
    } else {
      block$group_var <- g
      # Never analyse the grouping variable against itself.
      block <- block[!(!is.na(block$analysis_var) & block$analysis_var == g), , drop = FALSE]
    }

    keep <- ck_loa_is_available(block, dataset, sm_separator)
    dropped <- unique(stats::na.omit(block$analysis_var[!keep]))

    if (length(dropped) > 0) {
      ck_note(
        "group '", g, "': ", length(dropped),
        " analysis variable(s) not found in the export and skipped: ",
        paste(utils::head(dropped, 10), collapse = ", "),
        if (length(dropped) > 10) ", ..." else "",
        verbose = verbose
      )
    }

    block <- block[keep, , drop = FALSE]

    if (nrow(block) > 0) {
      blocks[[length(blocks) + 1]] <- block
    } else {
      ck_warn("No runnable analyses left for group variable '", g, "'.")
    }
  }

  if (length(blocks) == 0) {
    stop("No runnable analyses in the LOA for any of the group_variables.", call. = FALSE)
  }

  stacked <- dplyr::bind_rows(blocks)

  # The per-row "was a level actually supplied" flag travels as an attribute
  # rather than a column, because create_analysis() must only see its own
  # columns.
  out <- stacked[, intersect(ck_loa_columns(), names(stacked)), drop = FALSE]
  attr(out, "level_supplied") <- as.logical(stacked$.ck_level_supplied)

  out
}


#' Attach LOA Metadata Columns to a Results Table
#'
#' Joins DAP columns such as \code{sector} or \code{indicator} onto the results.
#' Deduplicated first and explicitly many-to-one, so a DAP with more than one
#' row per analysis_type + analysis_var cannot silently multiply result rows.
#' Run after the pivot, so it can never split rows.
#'
#' @param results_table A results table with \code{analysis_type} and
#'   \code{analysis_var}.
#' @param loa The original LOA (before stacking).
#' @param extra_columns LOA columns to carry through.
#' @return \code{results_table} with the metadata columns at the front.
#' @export
ck_attach_loa_metadata <- function(results_table, loa, extra_columns = NULL) {
  extra_columns <- intersect(extra_columns, names(loa))
  if (length(extra_columns) == 0) {
    return(results_table)
  }

  meta <- loa
  meta$analysis_var <- ck_loa_var_key(meta)
  meta <- dplyr::distinct(
    meta[, unique(c("analysis_type", "analysis_var", extra_columns)), drop = FALSE]
  )

  dup <- duplicated(meta[, c("analysis_type", "analysis_var")])
  if (any(dup)) {
    ck_warn(
      "The LOA has conflicting ", paste(extra_columns, collapse = "/"),
      " values for the same analysis_type + analysis_var. The first row wins for: ",
      paste(unique(meta$analysis_var[dup]), collapse = ", ")
    )
    meta <- meta[!dup, , drop = FALSE]
  }

  out <- dplyr::left_join(results_table, meta, by = c("analysis_type", "analysis_var"))

  # Derived analyses - the selection counts - have an analysis_type the DAP
  # never mentions, so fall back to matching on the variable alone for any row
  # that picked up nothing. Without this a count row lands in the output with a
  # blank sector and indicator.
  still_missing <- Reduce(`&`, lapply(extra_columns, function(cc) is.na(out[[cc]])))

  if (any(still_missing)) {
    var_meta <- dplyr::distinct(meta[, c("analysis_var", extra_columns), drop = FALSE])
    var_meta <- var_meta[!duplicated(var_meta$analysis_var), , drop = FALSE]
    hit <- match(out$analysis_var[still_missing], var_meta$analysis_var)

    for (cc in extra_columns) {
      out[[cc]][still_missing] <- var_meta[[cc]][hit]
    }
  }

  dplyr::relocate(out, dplyr::all_of(extra_columns))
}


# -----------------------------------------------------------------------------
# 4. Fast tabulation engine (point estimates, no variance)
# -----------------------------------------------------------------------------

#' Missing-Group Sentinel
#' @keywords internal
ck_missing_key <- function() ".__ck_missing__"


#' Weighted and Unweighted Two-Way Tabulation
#'
#' One-pass cross-tabulation of \code{x} by \code{g} returning both weighted and
#' unweighted cell counts. Uses \code{rowsum()} on an integer cell index, which
#' is O(n) and does not copy the data per group.
#'
#' @param w Numeric weights.
#' @param g Group vector (character).
#' @param x Value vector.
#' @param x_levels Optional value levels in reporting order. Levels with no
#'   respondents come back as a zero rather than disappearing - which is what a
#'   derived category such as "no choice selected" needs.
#' @return A list with matrices \code{w} and \code{n}; rows = groups,
#'   columns = values.
#' @keywords internal
ck_wtab <- function(w, g, x, x_levels = NULL) {
  gf <- factor(g)
  xf <- if (is.null(x_levels)) factor(x) else factor(x, levels = x_levels)

  n_g <- nlevels(gf)
  n_x <- nlevels(xf)

  cell <- (as.integer(xf) - 1L) * n_g + as.integer(gf)

  w_out <- numeric(n_g * n_x)
  n_out <- numeric(n_g * n_x)

  agg_w <- rowsum(w, cell, reorder = TRUE)
  agg_n <- rowsum(rep(1, length(cell)), cell, reorder = TRUE)
  pos <- as.integer(rownames(agg_w))

  w_out[pos] <- agg_w[, 1]
  n_out[pos] <- agg_n[, 1]

  list(
    w = matrix(w_out, nrow = n_g, dimnames = list(levels(gf), levels(xf))),
    n = matrix(n_out, nrow = n_g, dimnames = list(levels(gf), levels(xf)))
  )
}


#' Group-Wise Sums of a Numeric Vector
#'
#' @param values Numeric vector to sum.
#' @param g Group vector (character).
#' @param levels_g Group levels to report, in order.
#' @return A numeric vector, one element per level of \code{levels_g}.
#' @keywords internal
ck_gsum <- function(values, g, levels_g) {
  out <- stats::setNames(numeric(length(levels_g)), levels_g)
  if (length(values) == 0) return(out)

  agg <- rowsum(values, g, reorder = TRUE)
  hit <- intersect(rownames(agg), levels_g)
  out[hit] <- agg[hit, 1]
  out
}


#' Group-Wise Sums of Several Numeric Vectors at Once
#'
#' The same arithmetic as calling \code{\link{ck_gsum}} once per vector, but a
#' single \code{rowsum()} pass over a matrix instead of one per statistic. Each
#' analysis needs two to four group-wise sums over the same grouping, so this is
#' where the repetition was.
#'
#' @param x A numeric matrix (or vector) whose rows align with \code{g}.
#'   Column names become the column names of the result.
#' @param g Group vector (character).
#' @param levels_g Group levels to report, in order.
#' @return A numeric matrix: one row per level of \code{levels_g}, one column
#'   per column of \code{x}. Levels with no rows are zero.
#' @keywords internal
ck_group_sums <- function(x, g, levels_g) {
  x <- as.matrix(x)

  out <- matrix(
    0, nrow = length(levels_g), ncol = ncol(x),
    dimnames = list(levels_g, colnames(x))
  )

  if (nrow(x) == 0 || length(levels_g) == 0) return(out)

  agg <- rowsum(x, g, reorder = TRUE)
  hit <- intersect(rownames(agg), levels_g)

  if (length(hit) > 0) {
    out[hit, ] <- agg[hit, , drop = FALSE]
  }

  out
}


#' Weighted Median
#'
#' The standard lower weighted median: the smallest observed value at which the
#' cumulative weight reaches half the total. Note this returns an \emph{observed}
#' data value, whereas \code{survey::svyquantile()} interpolates by default, so
#' the two can differ by one step between adjacent observations. For the integer
#' 4Mi indicators (age, months, counts) they agree.
#'
#' @param x Numeric vector.
#' @param w Numeric weights.
#' @return A single numeric value, or \code{NA}.
#' @export
ck_weighted_median <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) return(NA_real_)

  x <- x[ok]
  w <- w[ok]

  o <- order(x)
  x <- x[o]
  w <- w[o]

  as.numeric(x[which(cumsum(w) >= sum(w) / 2)[1]])
}


#' Build the analysistools Analysis Key
#'
#' Reproduces \code{type @@/@@ var \%/\% value @@/@@ group \%/\% group_value} so
#' the fast engine's output is interchangeable with \code{create_analysis()}
#' output. Nothing in this file parses it.
#'
#' @param analysis_type,analysis_var,analysis_var_value,group_var,group_var_value
#'   Character vectors of the same length.
#' @return A character vector of analysis keys.
#' @keywords internal
ck_make_analysis_key <- function(analysis_type,
                                 analysis_var,
                                 analysis_var_value,
                                 group_var,
                                 group_var_value) {
  na_chr <- function(x) ifelse(is.na(x), "NA", as.character(x))

  paste0(
    na_chr(analysis_type), " @/@ ",
    na_chr(analysis_var), " %/% ", na_chr(analysis_var_value), " @/@ ",
    na_chr(group_var), " %/% ", na_chr(group_var_value)
  )
}


#' One Block of Long Results
#'
#' The single shape every branch of \code{\link{ck_fast_analysis}} returns.
#' Scalars recycle, so a branch passes whichever of its arguments vary.
#'
#' @param type,avar,avalue,gvar,gvalue Key columns.
#' @param stat,n,n_total,n_w,n_w_total Statistic columns.
#' @return A dataframe with the ten long-format columns.
#' @keywords internal
ck_res_df <- function(type, avar, avalue, gvar, gvalue,
                      stat, n, n_total, n_w, n_w_total) {
  data.frame(
    analysis_type = type,
    analysis_var = avar,
    analysis_var_value = avalue,
    group_var = gvar,
    group_var_value = gvalue,
    stat = unname(stat),
    n = unname(n),
    n_total = unname(n_total),
    n_w = unname(n_w),
    n_w_total = unname(n_w_total),
    stringsAsFactors = FALSE
  )
}


#' Point Estimates Without the Survey Package
#'
#' A drop-in replacement for \code{analysistools::create_analysis()} returning
#' the same long results table but point estimates and counts only - no
#' confidence intervals. The design affects nothing but the variance, so the
#' estimates are identical to the survey package's to floating-point precision
#' while being roughly 200x faster on a large disaggregation. Use it whenever
#' the LOA \code{level} cell is empty, which is what \code{engine = "auto"}
#' does.
#'
#' @param dataset The prepared analysis dataset (label row removed,
#'   select_multiple children 0/1, mean/median variables numeric).
#' @param loa A stacked LOA from \code{\link{ck_stack_loa}}.
#' @param weight_column Optional weights column. \code{NULL} makes \code{stat} a
#'   plain proportion or mean.
#' @param sm_separator Select_multiple separator. Default \code{"/"}.
#' @param keep_missing_groups Logical. Report respondents missing on the
#'   grouping variable as their own group. Default \code{TRUE}, matching
#'   \pkg{srvyr}; \code{survey::svyby} drops them.
#' @param verbose Logical. Default \code{TRUE}.
#' @return A long results table with \code{analysis_type}, \code{analysis_var},
#'   \code{analysis_var_value}, \code{group_var}, \code{group_var_value},
#'   \code{stat}, \code{n}, \code{n_total}, \code{n_w}, \code{n_w_total} and
#'   \code{analysis_key}.
#' @export
ck_fast_analysis <- function(dataset,
                             loa,
                             weight_column = NULL,
                             sm_separator = "/",
                             keep_missing_groups = TRUE,
                             verbose = TRUE) {
  if (nrow(loa) == 0) return(NULL)

  n_rows <- nrow(dataset)

  w <- if (!is.null(weight_column) && weight_column %in% names(dataset)) {
    ww <- suppressWarnings(as.numeric(dataset[[weight_column]]))
    ww[is.na(ww)] <- 0
    ww
  } else {
    rep(1, n_rows)
  }

  missing_key <- ck_missing_key()
  overall_key <- ".__ck_overall__"

  # Group vectors are built once per distinct grouping variable, not per
  # analysis.
  group_cache <- list()

  get_group <- function(gv) {
    tag <- if (is.na(gv)) "__overall__" else gv
    if (!is.null(group_cache[[tag]])) return(group_cache[[tag]])

    if (is.na(gv)) {
      gvec <- rep(overall_key, n_rows)
    } else {
      gvec <- trimws(as.character(dataset[[gv]]))
      gvec[is.na(gvec) | gvec == "" | gvec == "NA"] <- missing_key
    }

    group_cache[[tag]] <<- gvec
    gvec
  }

  out <- vector("list", nrow(loa))

  for (i in seq_len(nrow(loa))) {
    type <- tolower(as.character(loa$analysis_type[i]))
    avar <- as.character(loa$analysis_var[i])
    gvar <- as.character(loa$group_var[i])

    gvec <- get_group(gvar)
    keep_row <- if (isTRUE(keep_missing_groups)) {
      rep(TRUE, n_rows)
    } else {
      gvec != missing_key
    }

    res <- NULL

    if (type == "prop_select_one") {
      x_raw <- dataset[[avar]]

      # A factor column carries its own category order, and the derived
      # selection-count columns rely on that order being respected.
      x_levels <- if (is.factor(x_raw)) levels(x_raw) else NULL
      x <- trimws(as.character(x_raw))
      ok <- keep_row & !is.na(x) & x != "" & x != "NA"

      if (any(ok)) {
        tab <- ck_wtab(w[ok], gvec[ok], x[ok], x_levels)
        g_lv <- rownames(tab$w)
        x_lv <- colnames(tab$w)

        w_tot <- rowSums(tab$w)
        n_tot <- rowSums(tab$n)

        res <- ck_res_df(
          type, avar,
          avalue = rep(x_lv, each = length(g_lv)),
          gvar,
          gvalue = rep(g_lv, times = length(x_lv)),
          stat = as.vector(tab$w) / rep(w_tot, length(x_lv)),
          n = as.vector(tab$n),
          n_total = rep(n_tot, length(x_lv)),
          n_w = as.vector(tab$w),
          n_w_total = rep(w_tot, length(x_lv))
        )
      }
    } else if (type == "prop_select_multiple") {
      suffixes <- ck_sm_child_suffixes(avar, names(dataset), sm_separator)
      blocks <- vector("list", length(suffixes))

      for (j in seq_along(suffixes)) {
        xc <- suppressWarnings(as.numeric(dataset[[names(suffixes)[j]]]))
        ok <- keep_row & !is.na(xc)
        if (!any(ok)) next

        g_ok <- gvec[ok]
        g_lv <- sort(unique(g_ok))

        s <- ck_group_sums(
          cbind(w = w[ok], n = 1, wx = w[ok] * xc[ok], nx = xc[ok]),
          g_ok, g_lv
        )

        blocks[[j]] <- ck_res_df(
          type, avar, unname(suffixes[j]), gvar, g_lv,
          stat = s[, "wx"] / s[, "w"],
          n = s[, "nx"],
          n_total = s[, "n"],
          n_w = s[, "wx"],
          n_w_total = s[, "w"]
        )
      }

      blocks <- blocks[!vapply(blocks, is.null, logical(1))]
      if (length(blocks) > 0) res <- dplyr::bind_rows(blocks)
    } else if (type %in% c("mean", "median")) {
      x <- suppressWarnings(as.numeric(dataset[[avar]]))
      ok <- keep_row & !is.na(x)

      if (any(ok)) {
        g_ok <- gvec[ok]
        g_lv <- sort(unique(g_ok))

        s <- ck_group_sums(
          cbind(w = w[ok], n = 1, wx = w[ok] * x[ok]),
          g_ok, g_lv
        )

        stat <- if (type == "mean") {
          s[, "wx"] / s[, "w"]
        } else {
          idx <- split(which(ok), g_ok)[g_lv]
          vapply(
            idx,
            function(ii) if (is.null(ii)) NA_real_ else ck_weighted_median(x[ii], w[ii]),
            numeric(1),
            USE.NAMES = FALSE
          )
        }

        res <- ck_res_df(
          type, avar, NA_character_, gvar, g_lv,
          stat = stat,
          n = s[, "n"],
          n_total = s[, "n"],
          n_w = s[, "w"],
          n_w_total = s[, "w"]
        )
      }
    } else if (type == "ratio") {
      num_var <- as.character(loa$analysis_var_numerator[i])
      den_var <- as.character(loa$analysis_var_denominator[i])

      num <- suppressWarnings(as.numeric(dataset[[num_var]]))
      den <- suppressWarnings(as.numeric(dataset[[den_var]]))

      if (isTRUE(as.logical(loa$numerator_NA_to_0[i]))) {
        num[is.na(num) & !is.na(den)] <- 0
      }

      ok <- keep_row & !is.na(num) & !is.na(den)
      if (isTRUE(as.logical(loa$filter_denominator_0[i]))) {
        ok <- ok & den != 0
      }

      if (any(ok)) {
        g_ok <- gvec[ok]
        g_lv <- sort(unique(g_ok))

        s <- ck_group_sums(
          cbind(wn = w[ok] * num[ok], wd = w[ok] * den[ok], n = 1),
          g_ok, g_lv
        )

        res <- ck_res_df(
          type, paste0(num_var, " %/% ", den_var), NA_character_, gvar, g_lv,
          stat = s[, "wn"] / s[, "wd"],
          n = s[, "n"],
          n_total = s[, "n"],
          n_w = s[, "wn"],
          n_w_total = s[, "wd"]
        )
      }
    }

    out[[i]] <- res
  }

  out <- out[!vapply(out, is.null, logical(1))]
  if (length(out) == 0) return(NULL)

  results <- dplyr::bind_rows(out)

  # Map the internal sentinels back: overall -> NA group, missing -> NA value.
  results$group_var[results$group_var_value == overall_key] <- NA_character_
  results$group_var_value[results$group_var_value == overall_key] <- NA_character_
  results$group_var_value[results$group_var_value == missing_key] <- NA_character_

  results$analysis_key <- ck_make_analysis_key(
    results$analysis_type, results$analysis_var, results$analysis_var_value,
    results$group_var, results$group_var_value
  )

  ck_note(
    "fast engine: ", nrow(loa), " analyses -> ", nrow(results),
    " result rows (point estimates only, no confidence intervals)",
    verbose = verbose
  )

  results
}


#' Force the Column Types of a Results Table
#'
#' When a run uses both engines their outputs are stacked. A column that is
#' character in one and a factor or double in the other makes
#' \code{dplyr::bind_rows()} fail with a type error that says nothing useful, so
#' the key columns are pinned to character and the statistics to numeric first.
#'
#' @param x A long results table.
#' @return \code{x} with predictable column types.
#' @keywords internal
ck_harmonise_results <- function(x) {
  if (is.null(x) || nrow(x) == 0) return(x)

  chr_cols <- c(
    "analysis_type", "analysis_var", "analysis_var_value",
    "group_var", "group_var_value", "analysis_key"
  )
  num_cols <- c("stat", "stat_low", "stat_upp", "n", "n_total", "n_w", "n_w_total")

  for (cc in intersect(chr_cols, names(x))) {
    x[[cc]] <- as.character(x[[cc]])
  }
  for (cc in intersect(num_cols, names(x))) {
    if (!is.numeric(x[[cc]])) {
      x[[cc]] <- suppressWarnings(as.numeric(as.character(x[[cc]])))
    }
  }

  x
}


#' Drop Group Levels With Too Few Observations
#'
#' A 200-town disaggregation usually contains towns with a handful of
#' interviews. Those estimates are not reportable, and they are also what makes
#' the run slow. Setting them to \code{NA} removes the column from the output.
#'
#' @param dataset The analysis dataset.
#' @param group_variables Character vector of grouping variables.
#' @param min_group_n Minimum observations a level must have. \code{NULL}
#'   (default) keeps everything.
#' @param verbose Logical. Default \code{TRUE}.
#' @return A list with \code{dataset} and \code{dropped}.
#' @export
ck_drop_small_groups <- function(dataset,
                                 group_variables,
                                 min_group_n = NULL,
                                 verbose = TRUE) {
  empty <- data.frame(
    group_variable = character(0),
    group_value = character(0),
    n = numeric(0),
    stringsAsFactors = FALSE
  )

  if (is.null(min_group_n) || !is.finite(min_group_n) || min_group_n <= 1) {
    return(list(dataset = dataset, dropped = empty))
  }

  logs <- list()

  for (gv in setdiff(group_variables, "Overall")) {
    if (!gv %in% names(dataset)) next

    values <- trimws(as.character(dataset[[gv]]))
    values[values == "" | values == "NA"] <- NA_character_

    counts <- table(values, useNA = "no")
    small <- names(counts)[counts < min_group_n]
    if (length(small) == 0) next

    logs[[length(logs) + 1]] <- data.frame(
      group_variable = gv,
      group_value = small,
      n = as.numeric(counts[small]),
      stringsAsFactors = FALSE
    )

    values[values %in% small] <- NA_character_
    dataset[[gv]] <- values

    ck_note(
      "'", gv, "': ", length(small), " level(s) below n=", min_group_n,
      " set aside (", sum(counts[small]), " respondents)",
      verbose = verbose
    )
  }

  list(
    dataset = dataset,
    dropped = if (length(logs) > 0) dplyr::bind_rows(logs) else empty
  )
}


# -----------------------------------------------------------------------------
# 5. Long to wide
# -----------------------------------------------------------------------------

#' Pivot a Long Analysis Table into Variable x Group Wide Format
#'
#' ONA-flavoured replacement for
#' \code{presentresults::create_table_variable_x_group}. Differences:
#' \itemize{
#'   \item Reads \code{group_var} / \code{group_var_value} straight off the
#'     results table instead of round-tripping through the analysis key, so a
#'     label containing the key separators cannot break it.
#'   \item Column names are prefixed with the grouping variable
#'     (\code{stat_gender_male}), so two grouping variables sharing a value
#'     cannot collide.
#'   \item The ungrouped analysis is labelled explicitly instead of \code{"NA"}.
#' }
#'
#' @param results_table A long results table.
#' @param value_columns Statistic columns to spread. Default
#'   \code{c("stat", "n", "n_total")}.
#' @param overall_label Label for the ungrouped analysis. Default
#'   \code{"Overall"}.
#' @param missing_group_label Label for respondents missing a value on the
#'   grouping variable. Default \code{"Missing"}. These must not be folded into
#'   \code{overall_label} - they are a distinct, usually small, group.
#' @param use_group_prefix Logical. Prefix column names with the grouping
#'   variable name. Default \code{TRUE}.
#' @return A wide dataframe: one row per analysis_type / analysis_var /
#'   analysis_var_value, one column per value column x group value.
#' @export
ck_pivot_variable_x_group <- function(results_table,
                                      value_columns = c("stat", "n", "n_total"),
                                      overall_label = "Overall",
                                      missing_group_label = "Missing",
                                      use_group_prefix = TRUE) {
  required <- c("analysis_type", "analysis_var", "analysis_var_value")
  if (!all(required %in% names(results_table))) {
    stop(
      paste0("results_table must contain: ", paste(required, collapse = ", ")),
      call. = FALSE
    )
  }

  value_columns <- intersect(value_columns, names(results_table))
  if (length(value_columns) == 0) {
    stop("None of the value_columns are present in results_table.", call. = FALSE)
  }

  if ("group_var" %in% names(results_table)) {
    group_var <- as.character(results_table$group_var)
    group_var_value <- as.character(results_table$group_var_value)
  } else {
    group_var <- rep(NA_character_, nrow(results_table))
    group_var_value <- group_var
  }

  blank <- function(x) is.na(x) | x == "" | x == "NA"

  # A blank group_var means the ungrouped analysis. A blank group_var_value with
  # a real group_var means the respondent is missing on the grouping variable -
  # a distinct group, not the overall column.
  group_tag <- ifelse(blank(group_var_value), missing_group_label, group_var_value)

  slim <- results_table[, c(required, value_columns), drop = FALSE]
  slim$column_group <- ifelse(
    blank(group_var),
    overall_label,
    if (isTRUE(use_group_prefix)) paste0(group_var, "_", group_tag) else group_tag
  )

  # Guard against list-columns: duplicated LOA rows would otherwise make
  # pivot_wider silently nest the values.
  dup_key <- duplicated(slim[, c(required, "column_group"), drop = FALSE])
  if (any(dup_key)) {
    ck_warn(
      sum(dup_key), " duplicated analysis row(s) found (same analysis_type, ",
      "analysis_var, analysis_var_value and group). The first occurrence is ",
      "kept. Check the LOA for repeated entries."
    )
    slim <- slim[!dup_key, , drop = FALSE]
  }

  wide <- tidyr::pivot_wider(
    slim,
    id_cols = dplyr::all_of(required),
    names_from = "column_group",
    values_from = dplyr::all_of(value_columns),
    names_vary = "slowest",
    names_sep = "_"
  )

  # With a single values_from column, pivot_wider omits the "{.value}_" prefix.
  if (length(value_columns) == 1) {
    plain <- setdiff(names(wide), required)
    names(wide)[match(plain, names(wide))] <- paste0(value_columns, "_", plain)
  }

  wide
}


#' Move Each Derived Block Under Its Own Question
#'
#' The selection counts and choice combinations are appended to the LOA, so left
#' alone they land in a lump at the bottom. Only the derived rows move; every
#' other row keeps the order the DAP put it in, so a DAP that deliberately
#' analyses the same variable at two separate points is not silently reshuffled.
#' A derived block whose question is not otherwise in the DAP stays at the end.
#'
#' Where a question has both, the order of \code{derived_types} decides which
#' block comes first: counts, then combinations.
#'
#' @param wide_table The wide results table.
#' @param count_map The derived map from \code{\link{ck_derived_map}} (or the
#'   \code{map} from \code{\link{ck_add_selection_counts}}, for compatibility).
#' @param derived_types The \code{analysis_type} values that mark a derived
#'   block, in the order the blocks should appear.
#' @return \code{wide_table} with the derived blocks repositioned.
#' @export
ck_order_count_blocks <- function(wide_table,
                                  count_map,
                                  derived_types = c(
                                    "count_select_multiple",
                                    "combination_select_multiple"
                                  )) {
  if (is.null(count_map) || nrow(count_map) == 0) return(wide_table)
  if (!all(c("analysis_type", "analysis_var") %in% names(wide_table))) {
    return(wide_table)
  }

  type_chr <- as.character(wide_table$analysis_type)
  is_derived <- !is.na(type_chr) & type_chr %in% derived_types
  if (!any(is_derived)) return(wide_table)

  pos <- seq_len(nrow(wide_table))
  anchor <- as.numeric(pos)
  tier <- rep(0, nrow(wide_table))

  for (v in unique(wide_table$analysis_var[is_derived])) {
    parent <- which(!is_derived & wide_table$analysis_var == v)
    if (length(parent) == 0) next

    block <- which(is_derived & wide_table$analysis_var == v)
    anchor[block] <- max(pos[parent])
    tier[block] <- match(type_chr[block], derived_types)
  }

  out <- wide_table[order(anchor, tier, pos), , drop = FALSE]
  rownames(out) <- NULL
  out
}


#' Insert a Spacer and a Heading Above Each Derived Block
#'
#' Without a break, the derived rows sit flush against the question's own choice
#' rows and read as a few more choices - which they are not, and which would
#' invite someone to add them into a total. A \code{row_type} column marks every
#' row \code{"data"}, \code{"spacer"} or \code{"heading"} so the export step can
#' style or skip them without guessing from blank cells.
#'
#' Called \emph{after} the column map is built, so \code{row_type} is never read
#' as a statistic column.
#'
#' @param wide_table The wide results table, already ordered by
#'   \code{\link{ck_order_count_blocks}}.
#' @param count_map The derived map from \code{\link{ck_derived_map}} (or the
#'   \code{map} from \code{\link{ck_add_selection_counts}}, for compatibility).
#' @param heading Heading text. A single value applies to every derived block; a
#'   vector named by \code{analysis_type} sets one per block type. \code{""}
#'   inserts no heading row.
#' @param spacer Logical. Insert the blank row. Named by \code{analysis_type} to
#'   set one per block type. Default \code{TRUE}.
#' @param derived_types The \code{analysis_type} values that mark a derived block.
#' @return \code{wide_table} with the separator rows inserted and a
#'   \code{row_type} column added.
#' @export
ck_insert_count_separators <- function(wide_table,
                                       count_map,
                                       heading = "Select multiple count",
                                       spacer = TRUE,
                                       derived_types = c(
                                         "count_select_multiple",
                                         "combination_select_multiple"
                                       )) {
  if (is.null(count_map) || nrow(count_map) == 0) return(wide_table)
  if (!"analysis_type" %in% names(wide_table)) return(wide_table)

  if (!"row_type" %in% names(wide_table)) {
    wide_table$row_type <- "data"
  }

  type_chr <- as.character(wide_table$analysis_type)
  is_derived <- !is.na(type_chr) & type_chr %in% derived_types
  if (!any(is_derived)) return(wide_table)

  # A length-1 setting applies to every block; a named one is looked up by the
  # block's analysis_type.
  setting <- function(x, type, default) {
    if (length(x) == 0) return(default)
    if (!is.null(names(x))) {
      if (type %in% names(x)) return(unname(x[[type]]))
      return(default)
    }
    unname(x[[1]])
  }

  # A run is one contiguous stretch of the same derived type for the same
  # question, so a counts block and a combinations block sitting next to each
  # other each get their own break instead of sharing one heading.
  run_id <- paste(type_chr, as.character(wide_table$analysis_var), sep = " @/@ ")
  starts <- which(is_derived & c(TRUE, run_id[-1] != utils::head(run_id, -1)))

  # An all-NA row taken from the table itself, so every column keeps its type.
  blank <- wide_table[NA_integer_, , drop = FALSE]

  # The machine columns get an explicit sentinel rather than NA: left NA, every
  # existing `analysis_type == "prop_select_one"` filter would silently pick
  # these rows up, because NA == "x" is NA, not FALSE. The display columns are
  # left blank, which is what a report wants to render.
  marker_row <- function(kind, at_row, text = NULL) {
    r <- blank
    r$row_type <- kind
    r$analysis_type <- kind
    r$analysis_var <- wide_table$analysis_var[at_row]

    if (!is.null(text)) {
      # Written into both label columns, so the heading shows up whichever one
      # the export uses as its row label.
      for (cc in intersect(c("analysis_var_value", "label_analysis_var"), names(r))) {
        r[[cc]] <- text
      }
    }
    r
  }

  pieces <- list()
  prev <- 1L

  for (s in starts) {
    head_s <- as.character(setting(heading, type_chr[s], ""))
    spacer_s <- isTRUE(setting(spacer, type_chr[s], TRUE))

    if (!spacer_s && !nzchar(head_s)) next

    if (s > prev) {
      pieces[[length(pieces) + 1]] <- wide_table[prev:(s - 1), , drop = FALSE]
    }
    if (spacer_s) {
      pieces[[length(pieces) + 1]] <- marker_row("spacer", s)
    }
    if (nzchar(head_s)) {
      pieces[[length(pieces) + 1]] <- marker_row("heading", s, head_s)
    }
    prev <- s
  }

  pieces[[length(pieces) + 1]] <- wide_table[prev:nrow(wide_table), , drop = FALSE]

  out <- dplyr::bind_rows(pieces)
  rownames(out) <- NULL
  out
}


#' Columns Actually Needed by an Analysis
#'
#' A 4Mi export is hundreds of columns wide and \code{survey::svyby} copies the
#' whole data frame once per group level. Handing the design only the columns
#' the analysis touches cut a 200-level disaggregation from 4.64 s to 2.78 s per
#' three questions in testing.
#'
#' @param dataset The analysis dataset.
#' @param loa The stacked LOA.
#' @param group_variables Grouping variables.
#' @param weight_column,strata_column Optional design columns.
#' @param sm_separator Select_multiple separator.
#' @return A character vector of column names to keep.
#' @export
ck_design_columns <- function(dataset,
                              loa,
                              group_variables = NULL,
                              weight_column = NULL,
                              strata_column = NULL,
                              sm_separator = "/") {
  wanted <- c(
    loa$analysis_var, loa$analysis_var_numerator, loa$analysis_var_denominator,
    loa$group_var, setdiff(group_variables, "Overall"),
    weight_column, strata_column
  )
  wanted <- unique(wanted[!is.na(wanted)])

  children <- unlist(
    lapply(wanted, function(v) names(ck_sm_child_suffixes(v, names(dataset), sm_separator))),
    use.names = FALSE
  )

  unique(c(intersect(wanted, names(dataset)), children))
}


# -----------------------------------------------------------------------------
# 6. The pipeline
# -----------------------------------------------------------------------------

#' Run a Grouped Analysis Pipeline on an ONA Export
#'
#' Runs an analysis plan (LOA / DAP) over one or more grouping variables and
#' returns a single wide table where each grouping variable contributes a block
#' of columns. Differences from the Kobo/XLSForm version:
#'
#' \itemize{
#'   \item \strong{Labels come from the dataset, not the tool.} Row 1 of an ONA
#'     export is the label row. It is set aside before the analysis runs (so it
#'     never contaminates the statistics) and used to relabel the question names
#'     in the output. No \code{tool_survey} / \code{tool_choices} needed.
#'   \item \strong{Only questions are relabelled.} ONA exports store choice
#'     labels, so \code{analysis_var_value} is already human readable (except
#'     select_multiple children, where the child column label is used).
#'   \item \strong{Weights and strata are optional.}
#'   \item \strong{One estimation pass.} The LOA is stacked across grouping
#'     variables and estimated once, so rows align by construction and no uuid
#'     bookkeeping or horizontal joins are needed.
#'   \item \strong{\code{sm_separator} defaults to \code{"/"}}, the ONA style.
#' }
#'
#' @param dataset The ONA export dataframe. Row 1 is the label row by default.
#' @param loa The list of analyses / DAP. Must contain \code{analysis_type} and
#'   \code{analysis_var}; \code{group_var} optional. \code{analysis_type} must
#'   be one of \code{"prop_select_one"}, \code{"prop_select_multiple"},
#'   \code{"mean"}, \code{"median"}, \code{"ratio"}. Ratios also need
#'   \code{analysis_var_numerator} / \code{analysis_var_denominator}. The
#'   \code{level} column is optional and may be empty per row: empty means no
#'   confidence interval, a value (usually \code{0.95}) means an interval.
#' @param group_variables Grouping variables. \code{"Overall"} is the ungrouped
#'   analysis. Default \code{c("Overall")}.
#' @param skip_label_row Logical. Remove row 1 and use it as the label row.
#'   Default \code{TRUE}.
#' @param label_row Optional one-row dataframe of labels; takes precedence over
#'   the extracted row.
#' @param weight_column Optional weights column. Default \code{NULL}.
#' @param strata_column Optional strata column. Default \code{NULL}.
#' @param value_columns Statistic columns to spread. Default
#'   \code{c("stat", "n", "n_total")}. Add \code{"n_w"} / \code{"n_w_total"}
#'   when weighting, or \code{"stat_low"} / \code{"stat_upp"} for CIs.
#' @param extra_columns Optional LOA/DAP columns to carry into the output.
#' @param exclude_choices Optional choice labels to exclude from the
#'   denominator, e.g. \code{c("Don't know", "Refused")}. See
#'   \code{\link{ck_exclude_choices}} - this changes the denominator, it does
#'   not merely hide rows. Grouping variables are not affected.
#' @param exclude_ignore_case Logical. Default \code{TRUE}.
#' @param count_selections Optional \strong{select_multiple} parents for which
#'   to report how many choices each respondent picked, as extra rows with
#'   \code{analysis_type = "count_select_multiple"}. Passing a single select is
#'   an error. Note "no choice selected" counts respondents with nothing
#'   recorded, including any never asked the question.
#' @param count_selections_mode \code{"grouped"} (default) or \code{"exact"}.
#' @param count_selections_labels The three labels for \code{"grouped"}, always
#'   in the order \emph{none, exactly one, more than one}.
#' @param count_selections_order \code{"descending"} (default) or
#'   \code{"ascending"}.
#' @param count_selections_heading Heading row above each count block, so the
#'   rows cannot be read as three more choices. \code{""} inserts none.
#' @param count_selections_spacer Logical. Blank row above the heading. Default
#'   \code{TRUE}. With the heading this adds a \code{row_type} column.
#' @param count_selections_title_suffix Optionally appended to the question
#'   label on count rows. Default \code{""}.
#' @param count_combinations Optional named list asking, for one or more
#'   \strong{select_multiple} questions, which \emph{combination} of a chosen set
#'   of choices each respondent selected. Names are the parent variables, values
#'   are the choice labels of interest; name the choices to get short row labels.
#'   For example
#'   \code{list(Q78 = c(Economic = "Economic reasons", Conflict = "Armed conflict, generalised violence, and insecurity"))}
#'   gives four rows - \emph{Economic + Conflict}, \emph{Economic} (and not
#'   Conflict, whatever else was selected), \emph{Conflict}, \emph{None of these} -
#'   reported overall and across every grouping variable with
#'   \code{analysis_type = "combination_select_multiple"}. Choices other than the
#'   listed ones are ignored, so the rows are mutually exclusive and add to 100\%.
#'   Only respondents who answered the question are in the denominator. See
#'   \code{\link{ck_add_choice_combinations}}.
#' @param count_combinations_ignore_case Logical. Match the choice labels
#'   case-insensitively. Default \code{TRUE}.
#' @param count_combinations_none_label Row label for respondents who selected
#'   none of the listed choices. Default \code{"None of these"}.
#' @param count_combinations_joiner Placed between the display labels of a
#'   multi-choice combination. Default \code{" + "}.
#' @param count_combinations_order \code{"descending"} (default) puts the
#'   largest combinations first; \code{"ascending"} reverses it.
#' @param count_combinations_heading Heading row above each combination block.
#'   \code{""} inserts none.
#' @param count_combinations_spacer Logical. Blank row above the heading.
#'   Default \code{TRUE}.
#' @param count_combinations_title_suffix Optionally appended to the question
#'   label on combination rows. Default \code{""}.
#' @param max_combination_choices Refuse more than this many choices per
#'   question, so a long list cannot silently produce hundreds of rows. Default
#'   \code{6}.
#' @param fallback_level Placeholder level for LOA rows with an empty
#'   \code{level} cell. Default \code{0.95}; the resulting interval is blanked
#'   in the output.
#' @param engine \code{"auto"} (default) sends rows that asked for an interval
#'   to \code{analysistools}/\pkg{survey} and everything else to
#'   \code{\link{ck_fast_analysis}}. \code{"fast"} forces the fast engine (no
#'   intervals; neither \pkg{analysistools} nor \pkg{srvyr} needed).
#'   \code{"survey"} forces the old behaviour.
#' @param min_group_n Optional minimum interviews a group level must have.
#' @param slim_design Logical. Hand the design only the columns the analysis
#'   touches. Default \code{TRUE}.
#' @param keep_missing_groups Logical. Report respondents missing on the
#'   grouping variable as their own group. Default \code{TRUE}.
#' @param sm_separator Select_multiple separator. Default \code{"/"}.
#' @param prepare_sm Logical. Convert select_multiple children to 0/1 with an NA
#'   mask for rows never asked. Default \code{TRUE}.
#' @param sm_child_style \code{"auto"} (default), \code{"label"} or
#'   \code{"dummy"}.
#' @param blank_to_na Logical. Treat "" as \code{NA}. Default \code{TRUE}.
#' @param label_choices Logical. Relabel select_multiple choice values. Default
#'   \code{TRUE}.
#' @param add_analysis_type_label Logical. Add \code{label_analysis_type}.
#' @param analysis_type_labels Optional override dataframe.
#' @param recreate_sm_parents Logical. Rebuild select_multiple parents from
#'   their children first. Default \code{FALSE}; note it cannot distinguish
#'   "not asked" from "nothing selected".
#' @param drop_empty_prop_rows Logical. Drop the \code{NA}-valued placeholder
#'   row proportions produce. Default \code{TRUE}.
#' @param summary_value_label Value written into \code{analysis_var_value} for
#'   mean / median / ratio rows. Default \code{NA}.
#' @param missing_group_label Column label for respondents missing on the
#'   grouping variable. Default \code{"Missing"}.
#' @param use_group_prefix Logical. Prefix output columns with the grouping
#'   variable name. Default \code{TRUE}; \code{FALSE} gives the older
#'   \code{stat_<value>} naming, which collides if two grouping variables share
#'   a value.
#' @param lonely_psu How \pkg{survey} treats strata with a single observation.
#'   Default \code{"adjust"}; \code{NULL} leaves the option alone.
#' @param verbose Logical. Default \code{TRUE}.
#'
#' @return A list with \code{combined_results} (the wide table),
#'   \code{results_long}, \code{column_map}, \code{label_lookup},
#'   \code{loa_used}, \code{excluded_choices}, \code{dropped_groups},
#'   \code{selection_counts} and \code{choice_combinations}.
#' @export
#' @importFrom dplyr all_of bind_rows distinct left_join relocate
#' @importFrom tidyr pivot_wider
#' @importFrom stringr str_squish str_detect str_remove
run_group_analysis_pipeline <- function(dataset,
                                        loa,
                                        group_variables = c("Overall"),
                                        skip_label_row = TRUE,
                                        label_row = NULL,
                                        weight_column = NULL,
                                        strata_column = NULL,
                                        value_columns = c("stat", "n", "n_total"),
                                        extra_columns = NULL,
                                        exclude_choices = NULL,
                                        exclude_ignore_case = TRUE,
                                        count_selections = NULL,
                                        count_selections_mode = c("grouped", "exact"),
                                        count_selections_labels = c(
                                          "No choice selected",
                                          "Selected exactly 1 choice",
                                          "Selected more than 1 choice"
                                        ),
                                        count_selections_order = c("descending", "ascending"),
                                        count_selections_heading = "Select multiple count",
                                        count_selections_spacer = TRUE,
                                        count_selections_title_suffix = "",
                                        count_combinations = NULL,
                                        count_combinations_ignore_case = TRUE,
                                        count_combinations_none_label = "None of these",
                                        count_combinations_joiner = " + ",
                                        count_combinations_order = c("descending", "ascending"),
                                        count_combinations_heading = "Choice combination",
                                        count_combinations_spacer = TRUE,
                                        count_combinations_title_suffix = "",
                                        max_combination_choices = 6,
                                        fallback_level = 0.95,
                                        engine = c("auto", "fast", "survey"),
                                        min_group_n = NULL,
                                        slim_design = TRUE,
                                        keep_missing_groups = TRUE,
                                        sm_separator = "/",
                                        prepare_sm = TRUE,
                                        sm_child_style = c("auto", "label", "dummy"),
                                        blank_to_na = TRUE,
                                        label_choices = TRUE,
                                        add_analysis_type_label = TRUE,
                                        analysis_type_labels = NULL,
                                        recreate_sm_parents = FALSE,
                                        drop_empty_prop_rows = TRUE,
                                        summary_value_label = NA_character_,
                                        missing_group_label = "Missing",
                                        use_group_prefix = TRUE,
                                        lonely_psu = "adjust",
                                        verbose = TRUE) {
  # --- 0. Contract ----------------------------------------------------------
  sm_child_style <- match.arg(sm_child_style)
  engine <- match.arg(engine)
  count_selections_mode <- match.arg(count_selections_mode)
  count_selections_order <- match.arg(count_selections_order)
  count_combinations_order <- match.arg(count_combinations_order)

  if (!is.data.frame(dataset)) stop("dataset must be a dataframe.", call. = FALSE)
  if (!is.data.frame(loa)) stop("loa must be a dataframe.", call. = FALSE)

  dup_names <- unique(names(dataset)[duplicated(names(dataset))])
  if (length(dup_names) > 0) {
    ck_warn(
      "Duplicated column names in the export: ",
      paste(utils::head(dup_names, 10), collapse = ", "),
      ". Two choices in the same list probably share a label. Results for these ",
      "columns cannot be told apart."
    )
  }

  if (!all(c("analysis_type", "analysis_var") %in% names(loa))) {
    stop("loa must contain analysis_type and analysis_var.", call. = FALSE)
  }
  if (!"group_var" %in% names(loa)) {
    loa$group_var <- NA_character_
  }

  known_types <- c("prop_select_one", "prop_select_multiple", "mean", "median", "ratio")
  bad_types <- setdiff(unique(stats::na.omit(loa$analysis_type)), known_types)
  if (length(bad_types) > 0) {
    stop(
      paste0(
        "Unsupported analysis_type value(s): ", paste(bad_types, collapse = ", "),
        ". create_analysis() silently drops these. Allowed: ",
        paste(known_types, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (length(group_variables) == 0) {
    stop("group_variables must contain at least one entry.", call. = FALSE)
  }

  # Fail on a non-select_multiple before doing any work, so the message is the
  # first thing the user sees.
  count_selections <- ck_check_count_selections(count_selections, dataset, loa, sm_separator)

  count_combinations <- ck_check_choice_combinations(
    combinations = count_combinations,
    dataset = dataset,
    loa = loa,
    sm_separator = sm_separator,
    ignore_case = count_combinations_ignore_case,
    exclude_choices = exclude_choices,
    max_choices = max_combination_choices
  )

  if (any(duplicated(group_variables))) {
    ck_warn("Duplicated entries in group_variables were removed.")
    group_variables <- unique(group_variables)
  }

  if (!is.null(lonely_psu)) {
    old_option <- options(survey.lonely.psu = lonely_psu)
    on.exit(options(old_option), add = TRUE)
  }

  # --- 1. Label row ---------------------------------------------------------
  split <- ck_split_label_row(dataset, skip_label_row = skip_label_row)
  dataset <- split$dataset

  if (isTRUE(skip_label_row)) {
    ck_note("ONA label row set aside and excluded from the analysis", verbose = verbose)
  }

  if (is.null(label_row)) label_row <- split$label_row

  label_lookup <- ck_build_label_lookup(label_row)
  if (length(label_lookup) == 0) {
    ck_warn("No label row available. Machine (XML) names will be used in the outputs.")
  }

  if (nrow(dataset) == 0) {
    stop("The dataset has no rows left after removing the label row.", call. = FALSE)
  }

  # --- 2. Clean up the export ----------------------------------------------
  if (isTRUE(blank_to_na)) {
    dataset <- ck_blank_to_na(dataset)
  }

  if (isTRUE(recreate_sm_parents)) {
    ck_require_pkg("cleaningtools", "impact-initiatives/cleaningtools")
    ck_note("recreating select_multiple parent columns", verbose = verbose)
    dataset <- cleaningtools::recreate_parent_column(
      dataset = dataset,
      uuid_column = "uuid",
      sm_separator = sm_separator
    )[["data_with_fix_concat"]]
  }

  numeric_vars <- loa$analysis_var[
    !is.na(loa$analysis_type) & tolower(loa$analysis_type) %in% c("mean", "median")
  ]
  for (cc in intersect(c("analysis_var_numerator", "analysis_var_denominator"), names(loa))) {
    numeric_vars <- c(numeric_vars, loa[[cc]])
  }
  dataset <- ck_coerce_numeric(dataset, numeric_vars, verbose = verbose)

  # Selection counts are taken from the raw selection pattern, so this must run
  # before the children are masked - otherwise "no choice selected" would be
  # empty by construction, having already been removed as "not asked".
  counts <- ck_add_selection_counts(
    dataset = dataset,
    count_selections = count_selections,
    sm_separator = sm_separator,
    sm_child_style = sm_child_style,
    exclude_choices = exclude_choices,
    ignore_case = exclude_ignore_case,
    mode = count_selections_mode,
    labels = count_selections_labels,
    order = count_selections_order,
    verbose = verbose
  )
  dataset <- counts$dataset

  # Same reason as the counts: the combination is read off the raw selection
  # pattern, before the not-asked mask turns the children into 0/1/NA.
  combos <- ck_add_choice_combinations(
    dataset = dataset,
    combinations = count_combinations,
    sm_separator = sm_separator,
    sm_child_style = sm_child_style,
    exclude_choices = exclude_choices,
    ignore_case = count_combinations_ignore_case,
    none_label = count_combinations_none_label,
    joiner = count_combinations_joiner,
    order = count_combinations_order,
    verbose = verbose
  )
  dataset <- combos$dataset

  # Both features are derived categorical columns, so everything below works off
  # one table rather than branching per feature.
  derived <- ck_derived_map(counts$map, combos$map)

  if (isTRUE(prepare_sm)) {
    dataset <- ck_sm_children_to_binary(
      dataset,
      parents = loa$analysis_var[
        !is.na(loa$analysis_type) & tolower(loa$analysis_type) == "prop_select_multiple"
      ],
      sm_separator = sm_separator,
      sm_child_style = sm_child_style,
      verbose = verbose
    )
  }

  # Excluded choices leave the denominator, so this must happen before the
  # design is built - and after the select_multiple children are 0/1.
  exclusion <- ck_exclude_choices(
    dataset = dataset,
    loa = loa,
    exclude_choices = exclude_choices,
    sm_separator = sm_separator,
    ignore_case = exclude_ignore_case,
    verbose = verbose
  )
  dataset <- exclusion$dataset

  missing_groups <- setdiff(group_variables[group_variables != "Overall"], names(dataset))
  if (length(missing_groups) > 0) {
    ck_warn(
      "Grouping variable(s) not in the dataset and skipped: ",
      paste(missing_groups, collapse = ", ")
    )
    group_variables <- setdiff(group_variables, missing_groups)
  }
  if (length(group_variables) == 0) {
    stop("None of the group_variables are present in the dataset.", call. = FALSE)
  }

  small <- ck_drop_small_groups(
    dataset = dataset,
    group_variables = group_variables,
    min_group_n = min_group_n,
    verbose = verbose
  )
  dataset <- small$dataset

  # --- 3. Estimation --------------------------------------------------------
  # The derived columns are ordinary categorical variables, so they join the LOA
  # as plain proportions and pick up every grouping variable for free. Their
  # analysis_type is renamed after estimation.
  analysis_loa <- loa

  if (nrow(derived) > 0) {
    # Build the appended rows from the LOA itself rather than from scratch, so
    # every column keeps the type it already has. Constructing them literally
    # breaks on any DAP whose level column is numeric, because a literal
    # NA_character_ level cannot be combined with a double one. level stays NA,
    # i.e. no interval requested, which is what sends them to the fast engine.
    derived_loa <- analysis_loa[rep(NA_integer_, nrow(derived)), , drop = FALSE]
    rownames(derived_loa) <- NULL

    derived_loa$analysis_type <- "prop_select_one"
    derived_loa$analysis_var <- derived$derived_column
    derived_loa$group_var <- NA

    analysis_loa <- dplyr::bind_rows(analysis_loa, derived_loa)
  }

  stacked_loa <- ck_stack_loa(
    loa = analysis_loa,
    dataset = dataset,
    group_variables = group_variables,
    sm_separator = sm_separator,
    fallback_level = fallback_level,
    verbose = verbose
  )

  level_info <- ck_level_info(analysis_loa, fallback_level = fallback_level)

  if (!any(level_info$supplied) && any(c("stat_low", "stat_upp") %in% value_columns)) {
    ck_warn(
      "value_columns asks for stat_low / stat_upp but the LOA level column is ",
      "empty for every row, so no confidence interval was requested. Put 0.95 ",
      "in the level column of the rows you want intervals for."
    )
  }

  # Route each analysis to an engine. A row that asked for no confidence
  # interval has nothing for the survey package to do.
  supplied <- attr(stacked_loa, "level_supplied")
  if (is.null(supplied)) supplied <- rep(TRUE, nrow(stacked_loa))

  use_fast <- switch(
    engine,
    fast = rep(TRUE, nrow(stacked_loa)),
    survey = rep(FALSE, nrow(stacked_loa)),
    auto = !supplied
  )

  ck_note(
    "estimating ", nrow(stacked_loa), " analyses across ",
    length(group_variables), " grouping variable(s) - ",
    sum(use_fast), " by fast tabulation, ", sum(!use_fast), " by survey",
    verbose = verbose
  )

  parts <- list()

  if (any(use_fast)) {
    parts[[length(parts) + 1]] <- ck_fast_analysis(
      dataset = dataset,
      loa = stacked_loa[use_fast, , drop = FALSE],
      weight_column = weight_column,
      sm_separator = sm_separator,
      keep_missing_groups = keep_missing_groups,
      verbose = verbose
    )

    if (!is.null(strata_column) && engine == "fast") {
      ck_note(
        "strata affect the variance only, so '", strata_column,
        "' does not change these point estimates",
        verbose = verbose
      )
    }
  }

  if (any(!use_fast)) {
    ck_require_pkg("analysistools", "impact-initiatives/analysistools")
    ck_require_pkg("srvyr")

    survey_loa <- stacked_loa[!use_fast, , drop = FALSE]

    design_data <- dataset
    if (isTRUE(slim_design)) {
      keep_cols <- ck_design_columns(
        dataset, survey_loa, group_variables,
        weight_column, strata_column, sm_separator
      )
      if (length(keep_cols) > 0 && length(keep_cols) < ncol(dataset)) {
        ck_note(
          "survey design slimmed from ", ncol(dataset), " to ",
          length(keep_cols), " columns",
          verbose = verbose
        )
        design_data <- dataset[, keep_cols, drop = FALSE]
      }
    }

    design <- ck_build_survey_design(
      dataset = design_data,
      weight_column = weight_column,
      strata_column = strata_column,
      verbose = verbose
    )

    parts[[length(parts) + 1]] <- analysistools::create_analysis(
      design,
      loa = survey_loa,
      sm_separator = sm_separator
    )[["results_table"]]
  }

  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (length(parts) == 0) {
    stop("No results were produced.", call. = FALSE)
  }

  results_long <- dplyr::bind_rows(lapply(parts, ck_harmonise_results))
  if (nrow(results_long) == 0) {
    stop("The analysis returned no results.", call. = FALSE)
  }

  # Choice labels can contain the separator itself, so repair any value that was
  # truncated by a split rather than a prefix removal.
  results_long <- ck_fix_sm_choice_values(
    results_long,
    dataset = dataset,
    sm_separator = sm_separator,
    verbose = verbose
  )

  # Rename the derived analyses back to the question they came from, and give
  # them their own analysis_type so they cannot be mistaken for a response
  # distribution.
  if (nrow(derived) > 0) {
    hit <- match(as.character(results_long$analysis_var), derived$derived_column)
    is_derived <- !is.na(hit)

    if (any(is_derived)) {
      results_long$analysis_type[is_derived] <- derived$analysis_type[hit[is_derived]]
      results_long$analysis_var[is_derived] <- derived$analysis_var[hit[is_derived]]

      if ("analysis_key" %in% names(results_long)) {
        results_long$analysis_key[is_derived] <- ck_make_analysis_key(
          results_long$analysis_type[is_derived],
          results_long$analysis_var[is_derived],
          results_long$analysis_var_value[is_derived],
          results_long$group_var[is_derived],
          results_long$group_var_value[is_derived]
        )
      }

      for (tp in unique(derived$analysis_type)) {
        ck_note(
          "derived rows: ", sum(results_long$analysis_type == tp),
          " reported as ", tp,
          verbose = verbose
        )
      }
    }
  }

  # --- 3b. Honour an empty level: no interval requested, none reported ------
  ci_cols <- intersect(c("stat_low", "stat_upp"), names(results_long))

  if (length(ci_cols) > 0) {
    if (!any(level_info$supplied)) {
      results_long <- results_long[, setdiff(names(results_long), ci_cols), drop = FALSE]
      ck_note(
        "no level supplied in the LOA, so no confidence intervals are reported",
        verbose = verbose
      )
    } else if (any(!level_info$supplied)) {
      no_level_keys <- unique(paste(
        analysis_loa$analysis_type[!level_info$supplied],
        ck_loa_var_key(analysis_loa)[!level_info$supplied],
        sep = " @/@ "
      ))
      blank_ci <- paste(
        results_long$analysis_type, results_long$analysis_var, sep = " @/@ "
      ) %in% no_level_keys

      if (any(blank_ci)) {
        for (cc in ci_cols) {
          results_long[[cc]][blank_ci] <- NA_real_
        }
        ck_note(
          "cleared the confidence interval on ", sum(blank_ci),
          " row(s) whose LOA level cell was empty",
          verbose = verbose
        )
      }
    }
  }

  # --- 4. Drop the empty proportion placeholder rows ------------------------
  if (isTRUE(drop_empty_prop_rows)) {
    value_chr <- as.character(results_long$analysis_var_value)
    drop_rows <- (is.na(value_chr) | value_chr == "" | value_chr == "NA") &
      grepl("prop", tolower(as.character(results_long$analysis_type)))

    if (any(drop_rows)) {
      ck_note(
        "dropped ", sum(drop_rows), " proportion row(s) with no response category",
        verbose = verbose
      )
      results_long <- results_long[!drop_rows, , drop = FALSE]
    }
  }

  # --- 5. Wide format ------------------------------------------------------
  wide <- ck_pivot_variable_x_group(
    results_long,
    value_columns = value_columns,
    overall_label = "Overall",
    missing_group_label = missing_group_label,
    use_group_prefix = use_group_prefix
  )

  if (!is.na(summary_value_label)) {
    blank_value <- is.na(wide$analysis_var_value) |
      as.character(wide$analysis_var_value) == "NA"
    wide$analysis_var_value[blank_value] <- summary_value_label
  }

  # --- 6. Labels and metadata ----------------------------------------------
  wide <- ck_relabel_questions(
    results_table = wide,
    label_lookup = label_lookup,
    sm_separator = sm_separator,
    label_choices = label_choices
  )

  if (isTRUE(add_analysis_type_label)) {
    type_labels <- analysis_type_labels
    if (is.null(type_labels)) type_labels <- ck_analysis_type_labels()

    wide <- dplyr::left_join(wide, type_labels, by = "analysis_type")
    wide$label_analysis_type <- ifelse(
      is.na(wide$label_analysis_type), wide$analysis_type, wide$label_analysis_type
    )
    wide <- dplyr::relocate(wide, "label_analysis_type", .after = "analysis_type")
  }

  wide <- ck_attach_loa_metadata(wide, loa, extra_columns)

  # --- 6b. Title and position the derived blocks ---------------------------
  if (nrow(derived) > 0 && "analysis_type" %in% names(wide)) {
    title_suffix <- c(
      count_select_multiple = count_selections_title_suffix,
      combination_select_multiple = count_combinations_title_suffix
    )

    for (tp in names(title_suffix)) {
      if (!nzchar(title_suffix[[tp]])) next

      rows <- !is.na(wide$analysis_type) & wide$analysis_type == tp
      if (any(rows)) {
        wide$label_analysis_var[rows] <- paste0(
          wide$label_analysis_var[rows], title_suffix[[tp]]
        )
      }
    }

    wide <- ck_order_count_blocks(wide, derived)
  }

  # --- 7. Column map -------------------------------------------------------
  id_cols <- c(
    intersect(extra_columns, names(wide)),
    "analysis_type", "label_analysis_type",
    "analysis_var", "label_analysis_var", "analysis_var_value",
    "row_type"
  )
  stat_cols <- setdiff(names(wide), id_cols)

  column_map <- NULL
  if (length(stat_cols) > 0) {
    owner <- rep(NA_character_, length(stat_cols))
    for (g in c("Overall", setdiff(group_variables, "Overall"))) {
      tag <- if (identical(g, "Overall")) "_Overall" else paste0("_", g, "_")
      hit <- is.na(owner) & grepl(tag, stat_cols, fixed = TRUE)
      owner[hit] <- g
    }
    column_map <- data.frame(
      column = stat_cols,
      group_variable = owner,
      group_variable_label = ifelse(
        !is.na(owner) & owner %in% names(label_lookup),
        unname(label_lookup[owner]),
        owner
      ),
      stringsAsFactors = FALSE
    )
  }

  # --- 7b. Separate the count blocks from the choice rows ------------------
  # After the column map, so row_type is never mistaken for a statistic column.
  if (nrow(derived) > 0) {
    wide <- ck_insert_count_separators(
      wide,
      count_map = derived,
      heading = c(
        count_select_multiple = count_selections_heading,
        combination_select_multiple = count_combinations_heading
      ),
      spacer = c(
        count_select_multiple = count_selections_spacer,
        combination_select_multiple = count_combinations_spacer
      )
    )
  }

  ck_note("ANALYSIS DONE: ", nrow(wide), " rows, ", ncol(wide), " columns", verbose = verbose)

  list(
    combined_results = wide,
    results_long = results_long,
    column_map = column_map,
    label_lookup = label_lookup,
    loa_used = stacked_loa,
    excluded_choices = exclusion$excluded,
    dropped_groups = small$dropped,
    selection_counts = counts$map,
    choice_combinations = combos$map
  )
}
