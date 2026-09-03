# =============================================================================
# MMC-branded Excel export for variable x group analysis tables
# SIMPLIFIED BUILD.  Same public functions, same arguments, same workbook.
# =============================================================================
#
# What changed relative to format_my_xlsx_variable_x_group.R
# ----------------------------------------------------------
#   * ck_sample_values() removed - it was defined and never called
#   * the four style closures (index_style / stat_style / cat_style /
#     cell_style) are one ck_body_style() factory; the two that were textually
#     identical were stat_style and cell_style
#   * the left / right mirror in the blocks layout is a loop over a `panels`
#     list instead of a dozen `if (show_counts)` duplications
#   * ck_write_readme_sheet() rebuilt around three local helpers (wr / mg / st)
#     and a style list, instead of ~20 open-coded write + merge + style triples
#   * the blocks layout no longer inserts separator rows only to filter them
#     back out one line later: ck_sort_within_groups() does the sorting, and
#     add_empty_rows_between_groups() now calls it rather than repeating it
#
# Speed notes that still apply (do not undo these)
# ------------------------------------------------
#   * ONE style object per distinct style. openxlsx walks every style object
#     against every populated cell at save time, so cost is
#     (number of addStyle calls) x (number of cells), and stack = TRUE makes it
#     super-linear. The collector gathers rectangles and issues one addStyle()
#     per distinct style with stack = FALSE: a 500-block table goes from tens of
#     thousands of style objects to about twenty.
#   * mergeCells() re-checks every existing merge on each call, so merging a few
#     thousand header cells one at a time is quadratic. ck_add_merges() appends
#     non-overlapping ranges straight to the worksheet.
#   * numFmt uses the built-in ids ("NUMBER" = 2, "3") so openxlsx does not
#     rescan every style object for a free custom numFmtId. Proportions are the
#     exception: the built-in "PERCENTAGE" id is fixed at two decimals, so they
#     use one custom format string built once per workbook by
#     ck_percent_format() and reused on every cell.
# =============================================================================

# -----------------------------------------------------------------------------
# 1. Palette
# -----------------------------------------------------------------------------

#' Which Build of the Formatter Is Loaded
#'
#' Two files in this project define `format_my_xlsx_variable_x_group()`, so
#' `source()` order decides which definition survives - silently, and with no
#' error, because the signatures overlap. This is printed on every run so the
#' answer is never in doubt.
#'
#' @return A one-line build description.
#' @export
ck_formatter_build <- function() {
  "simplified build: n-ordering, empty-group drop, NaN blanking, percent_digits"
}


#' MMC Brand Colour Palette
#'
#' @return A named character vector of hex colours.
#' @keywords internal
mmc_colours <- function() {
  c(
    navy = "#003D58", # primary  - group header band, borders, body text
    teal = "#00A2A5", # primary  - column header band
    light_blue = "#AFDFE4", # left index panel (key column)
    pale_blue = "#D5EEF0", # left index panel (secondary)
    yellow = "#FFE07C",
    mauve = "#B88AAB",
    green = "#BBD876",
    orange = "#FBBC75",
    salmon = "#F8AB9E",
    dark_green = "#5B9E62",
    blue = "#009BD9",
    red = "#F15B5B",
    maroon = "#63193B",
    white = "#FFFFFF",
    off_white = "#F4FAFB", # alternating statistic block
    grid = "#FFFFFF" # header cell gridlines
  )
}


#' Resolve the MMC Palette
#'
#' Merges user supplied colours over the defaults.
#'
#' @param palette Optional named character vector or list of overrides.
#' @return A named character vector of hex colours.
#' @keywords internal
resolve_mmc_palette <- function(palette = NULL) {
  base_palette <- mmc_colours()
  if (is.null(palette)) {
    return(base_palette)
  }

  palette <- unlist(palette)
  unknown <- setdiff(names(palette), names(base_palette))

  if (length(unknown) > 0) {
    warning(sprintf(
      "Ignoring unknown palette element(s): %s",
      paste(unknown, collapse = ", ")
    ))
    palette <- palette[names(palette) %in% names(base_palette)]
  }

  base_palette[names(palette)] <- palette
  base_palette
}


# -----------------------------------------------------------------------------
# 2. Row ordering and separators
# -----------------------------------------------------------------------------

#' Sort Rows Within Each Group, Largest First
#'
#' Sorts rows inside each run of `col` by the first `stat_` column, descending.
#' Because the permutation only ever moves rows within a run of identical `col`
#' values, the run boundaries are unchanged - which is why
#' [add_empty_rows_between_groups()] can sort first and then walk the runs.
#'
#' @param df The data frame.
#' @param col The column that identifies groups.
#' @return `df` with the rows of each group reordered.
#' @keywords internal
ck_sort_within_groups <- function(df, col) {
  if (!col %in% names(df)) {
    stop(sprintf("Column '%s' was not found in the dataset.", col))
  }
  if (nrow(df) == 0) {
    return(df)
  }

  first_stat_col <- which(startsWith(names(df), "stat_"))[1]
  if (is.na(first_stat_col)) {
    return(df)
  }

  r <- rle(as.character(df[[col]]))
  starts <- cumsum(c(1L, utils::head(r$lengths, -1)))

  idx <- integer(nrow(df))
  at <- 0L
  for (i in seq_along(r$lengths)) {
    rows <- starts[i]:(starts[i] + r$lengths[i] - 1L)
    idx[(at + 1L):(at + r$lengths[i])] <- rows[
      order(df[[first_stat_col]][rows], decreasing = TRUE, na.last = TRUE)
    ]
    at <- at + r$lengths[i]
  }

  out <- df[idx, , drop = FALSE]
  rownames(out) <- NULL
  out
}


#' Insert Empty Rows Between Grouped Elements
#'
#' Inserts an all-`NA` row between groups so each group is visually separated in
#' the Excel output. Rows within each group can be sorted by the first `stat_`
#' column, largest first.
#'
#' @param df The data frame.
#' @param col The column used to identify groups.
#' @param sort_desc Logical. Sort within each group. Default `TRUE`.
#' @return The data frame with empty rows separating distinct groups.
#' @keywords internal
add_empty_rows_between_groups <- function(df, col, sort_desc = TRUE) {
  if (!col %in% names(df)) {
    stop(sprintf("Column '%s' was not found in the dataset.", col))
  }
  if (nrow(df) == 0) {
    return(df)
  }

  if (isTRUE(sort_desc)) {
    df <- ck_sort_within_groups(df, col)
  }

  r <- rle(as.character(df[[col]]))

  # One all-NA row that preserves the column classes of df
  empty_row <- df[NA_integer_, , drop = FALSE]
  rownames(empty_row) <- NULL

  # Build the pieces in a list and bind once - rbind() inside the loop is
  # quadratic and is a real cost on tables with hundreds of questions.
  pieces <- vector("list", 2L * length(r$lengths))
  idx <- 1L
  k <- 0L

  for (i in seq_along(r$lengths)) {
    k <- k + 1L
    pieces[[k]] <- df[idx:(idx + r$lengths[i] - 1L), , drop = FALSE]

    if (i < length(r$lengths)) {
      k <- k + 1L
      pieces[[k]] <- empty_row
    }
    idx <- idx + r$lengths[i]
  }

  new_df <- do.call(rbind, pieces[seq_len(k)])
  rownames(new_df) <- NULL
  new_df
}


# -----------------------------------------------------------------------------
# 3. Style collector
# -----------------------------------------------------------------------------

#' Create a Style Collector
#'
#' @return An environment holding the pending rectangles and the style cache.
#' @keywords internal
ck_new_style_grid <- function() {
  sg <- new.env(parent = emptyenv())
  sg$rect <- list()
  sg$keys <- character(0)
  sg$styles <- list()
  sg
}


#' Register a Style and Return its Key
#'
#' Memoises [openxlsx::createStyle()] so identical styles are built once and
#' share a single style object at save time.
#'
#' @param sg A style grid from [ck_new_style_grid()].
#' @param ... Arguments passed to [openxlsx::createStyle()].
#' @return The cache key of the style.
#' @keywords internal
ck_style <- function(sg, ...) {
  args <- list(...)
  key <- paste(
    names(args),
    vapply(
      args,
      function(z) paste(as.character(z), collapse = ","),
      character(1)
    ),
    sep = "=",
    collapse = "|"
  )

  if (is.null(sg$styles[[key]])) {
    sg$styles[[key]] <- do.call(openxlsx::createStyle, args)
  }

  key
}


#' Queue a Rectangle of Cells for a Style
#'
#' @param sg A style grid from [ck_new_style_grid()].
#' @param key A style key returned by [ck_style()].
#' @param rows Integer vector of sheet rows.
#' @param cols Integer vector of sheet columns.
#' @return Invisibly `NULL`.
#' @keywords internal
ck_add_rect <- function(sg, key, rows, cols) {
  if (length(rows) == 0 || length(cols) == 0) {
    return(invisible(NULL))
  }

  n <- length(sg$rect) + 1L
  sg$rect[[n]] <- list(rows = as.integer(rows), cols = as.integer(cols))
  sg$keys[n] <- key
  invisible(NULL)
}


#' Write All Queued Styles to a Worksheet
#'
#' One `addStyle()` call per distinct style, with `stack = FALSE`, which is what
#' keeps `saveWorkbook()` fast.
#'
#' @param sg A style grid from [ck_new_style_grid()].
#' @param wb An `openxlsx` workbook.
#' @param sheet Sheet name.
#' @return Invisibly the number of style objects written.
#' @keywords internal
ck_flush_styles <- function(sg, wb, sheet) {
  if (length(sg$rect) == 0) {
    return(invisible(0L))
  }

  unique_keys <- unique(sg$keys)

  for (k in unique_keys) {
    idx <- which(sg$keys == k)
    rr <- vector("list", length(idx))
    cc <- vector("list", length(idx))

    for (j in seq_along(idx)) {
      rct <- sg$rect[[idx[j]]]
      rr[[j]] <- rep.int(rct$rows, times = length(rct$cols))
      cc[[j]] <- rep(rct$cols, each = length(rct$rows))
    }

    openxlsx::addStyle(
      wb,
      sheet = sheet,
      style = sg$styles[[k]],
      rows = unlist(rr, use.names = FALSE),
      cols = unlist(cc, use.names = FALSE),
      gridExpand = FALSE,
      stack = FALSE
    )
  }

  sg$rect <- list()
  sg$keys <- character(0)
  invisible(length(unique_keys))
}


#' Build a Border Specification
#'
#' openxlsx assigns colours and styles to sides in the order the side names
#' appear in the `border` string, so all three values are built together.
#'
#' @param sides Any of "Top", "Bottom", "Left", "Right".
#' @param thick Which of those sides should be thick.
#' @param thin_colour Colour of the thin sides.
#' @param thick_colour Colour of the thick sides.
#' @return A list with `border`, `borderColour` and `borderStyle`.
#' @keywords internal
ck_border <- function(
  sides = c("Top", "Bottom", "Left", "Right"),
  thick = character(0),
  thin_colour = "#AFDFE4",
  thick_colour = "#003D58"
) {
  sides <- intersect(c("Top", "Bottom", "Left", "Right"), sides)
  if (length(sides) == 0) {
    return(NULL)
  }

  is_thick <- sides %in% thick

  list(
    border = paste(sides, collapse = ""),
    borderColour = ifelse(is_thick, thick_colour, thin_colour),
    borderStyle = ifelse(is_thick, "thick", "thin")
  )
}


#' Register a Body Cell Style
#'
#' The one factory behind every data-area style on both layouts: the index
#' panel, the statistic cells, the category column and the count cells. Only
#' the fill, alignment, number format, emphasis and which borders are thick
#' actually differ between them.
#'
#' `deco = NULL` omits `textDecoration` entirely rather than passing `""`, which
#' is the distinction the original index / category styles relied on.
#'
#' @param sg A style grid from [ck_new_style_grid()].
#' @param pal Resolved MMC palette.
#' @param font_name Font used throughout.
#' @param fill Cell fill colour.
#' @param halign Horizontal alignment.
#' @param numfmt Optional number format ("PERCENTAGE", "NUMBER", "3").
#' @param deco Optional `textDecoration` value.
#' @param sides Which sides are drawn.
#' @param thick Which of those sides are thick.
#' @return The cache key of the style.
#' @keywords internal
ck_body_style <- function(
  sg,
  pal,
  font_name,
  fill,
  halign,
  numfmt = NULL,
  deco = NULL,
  sides = c("Top", "Bottom", "Left", "Right"),
  thick = character(0)
) {
  bd <- ck_border(
    sides = sides,
    thick = thick,
    thin_colour = pal[["light_blue"]],
    thick_colour = pal[["navy"]]
  )

  args <- list(
    sg,
    fontSize = 11,
    fontName = font_name,
    fontColour = pal[["navy"]],
    fgFill = fill,
    halign = halign,
    valign = "center"
  )

  if (!is.null(deco)) {
    args$textDecoration <- deco
  }
  if (!is.null(numfmt)) {
    args$numFmt <- numfmt
  }

  args$border <- bd$border
  args$borderColour <- bd$borderColour
  args$borderStyle <- bd$borderStyle

  do.call(ck_style, args)
}


#' The Excel Number Format for a Percentage
#'
#' Percentages are stored as proportions and displayed through Excel's number
#' format, so the cell keeps its full precision while the sheet shows a rounded
#' figure. That distinction matters: a column of displayed whole numbers still
#' sums and averages correctly, which it would not if the stored values had
#' been truncated on the way in.
#'
#' Built as a format string rather than using openxlsx's built-in
#' `"PERCENTAGE"` id, which is fixed at two decimals.
#'
#' @param digits Number of decimal places, 0 or more.
#' @return An Excel number format string, e.g. `"0%"` or `"0.0%"`.
#' @keywords internal
ck_percent_format <- function(digits = 0) {
  digits <- suppressWarnings(as.integer(digits))
  if (length(digits) != 1 || is.na(digits) || digits < 0) {
    stop("percent_digits must be a single non-negative whole number.", call. = FALSE)
  }
  if (digits == 0) {
    return("0%")
  }
  paste0("0.", strrep("0", digits), "%")
}


#' Register a Header Band Style
#'
#' The three coloured header bands share everything except size, fill and
#' alignment.
#'
#' @param sg A style grid from [ck_new_style_grid()].
#' @param pal Resolved MMC palette.
#' @param font_name Font used throughout.
#' @param size Font size.
#' @param fill Band fill colour.
#' @param halign Horizontal alignment.
#' @param wrap Logical. Wrap the text.
#' @param border_colour Colour of the header gridlines.
#' @return The cache key of the style.
#' @keywords internal
ck_band_style <- function(
  sg,
  pal,
  font_name,
  size,
  fill,
  halign,
  wrap = FALSE,
  border_colour = pal[["grid"]]
) {
  ck_style(
    sg,
    fontSize = size,
    fontName = font_name,
    fontColour = pal[["white"]],
    fgFill = fill,
    textDecoration = "bold",
    halign = halign,
    valign = "center",
    wrapText = wrap,
    border = "TopBottomLeftRight",
    borderColour = border_colour,
    borderStyle = "thin"
  )
}


#' Add Many Merged Ranges at Once
#'
#' The ranges built by this file never overlap by construction, so they are
#' appended straight to the worksheet instead of going through
#' [openxlsx::mergeCells()] once each.
#'
#' @param wb An openxlsx workbook.
#' @param sheet Sheet name.
#' @param refs Character vector of ranges, e.g. `c("C2:E2", "I2:K2")`.
#' @return Invisibly `NULL`.
#' @keywords internal
ck_add_merges <- function(wb, sheet, refs) {
  refs <- refs[!is.na(refs) & nzchar(refs)]
  if (length(refs) == 0) {
    return(invisible(NULL))
  }

  i <- wb$validateSheet(sheet)
  wb$worksheets[[i]]$mergeCells <- c(
    wb$worksheets[[i]]$mergeCells,
    sprintf('<mergeCell ref="%s"/>', refs)
  )
  invisible(NULL)
}


#' Build an Excel Range Reference for a Single Row
#'
#' @param row Row number.
#' @param col_from First column.
#' @param col_to Last column.
#' @return A range string, or `NA` when the range covers one cell.
#' @keywords internal
ck_row_ref <- function(row, col_from, col_to) {
  if (col_to <= col_from) {
    return(NA_character_)
  }

  sprintf(
    "%s%s:%s%s",
    openxlsx::int2col(col_from),
    row,
    openxlsx::int2col(col_to),
    row
  )
}


# -----------------------------------------------------------------------------
# 4. Sheet planning
# -----------------------------------------------------------------------------

#' Drop the Disaggregation Variable Prefix from a Group Label
#'
#' `analysistools::unite_variables()` names each group value after its variable,
#' so a block comes back as `"Respondent_Gender_Female"`. Where the variable
#' name is already shown in the band above, repeating it in every column just
#' makes the header unreadable, so it is stripped back to `"Female"`.
#'
#' @param label The group value label.
#' @param group_variable The disaggregation variable the group belongs to.
#' @return The shortened label, or the original when there is nothing to strip.
#' @keywords internal
ck_short_group_label <- function(label, group_variable) {
  if (is.na(label) || is.na(group_variable)) {
    return(label)
  }
  if (
    identical(label, "Overall") || group_variable %in% c("Overall", "results")
  ) {
    return(label)
  }

  for (sep in c("_", ".", " - ", "-", ":", "/", " ")) {
    prefix <- paste0(group_variable, sep)
    if (startsWith(label, prefix) && nchar(label) > nchar(prefix)) {
      return(substring(label, nchar(prefix) + 1L))
    }
  }

  label
}


#' Make a Sheet Name Legal and Unique
#'
#' @param x Character vector of candidate sheet names.
#' @param taken Character vector of names already used.
#' @return A legal, unique sheet name vector.
#' @keywords internal
ck_safe_sheet_name <- function(x, taken = character(0)) {
  out <- character(length(x))

  for (i in seq_along(x)) {
    nm <- trimws(gsub("[][*?/\\:]", "-", as.character(x[i])))
    if (is.na(nm) || nm == "") {
      nm <- paste0("sheet_", i)
    }
    nm <- substr(nm, 1, 31)

    base_nm <- nm
    j <- 1L
    while (nm %in% c(taken, out[seq_len(i - 1)])) {
      j <- j + 1L
      suffix <- paste0("_", j)
      nm <- paste0(substr(base_nm, 1, 31 - nchar(suffix)), suffix)
    }

    out[i] <- nm
  }

  out
}


#' Work Out Which Sheets to Write
#'
#' Two kinds of split can be combined:
#' \itemize{
#'   \item `"group_variable"` splits down the *columns*: each disaggregation
#'     variable (country, town, ...) gets its own sheet.
#'   \item Any identifier column name splits down the *rows*: each value of
#'     `sector`, `indicator`, `analysis_type`, ... gets its own sheet.
#' }
#'
#' @param dat The table, after renaming.
#' @param blocks The statistic blocks.
#' @param block_group Grouping variable of each block.
#' @param overall_idx Index of the Overall block(s).
#' @param n_index Number of identifier columns before the statistics.
#' @param split_by `"none"`, `"group_variable"` and/or column names.
#' @param repeat_overall Logical. Repeat the Overall block on every sheet.
#' @param order_groups_by_n Logical, default `TRUE`. Order the group columns
#'   within each grouping variable by sample size, largest first, instead of the
#'   alphabetical order they come out of the pipeline in - so
#'   `Female (n=740), Male (n=1152)` is written `Male (n=1152), Female (n=740)`,
#'   and `No, Refused, Yes` becomes `Yes, No, Refused`. Only blocks belonging to
#'   the same grouping variable move, so the merged variable band above them
#'   still spans a contiguous stretch and `Overall` stays put. Needs a count
#'   column: pass `total_columns` (e.g. `c("n", "n_total")`) or there is nothing
#'   to sort by and the order is left alone. `FALSE` restores the alphabetical
#'   order.
#'
#'   This sets the sheet-wide order, from each group's sample size across the
#'   whole table. In `layout = "blocks"` it is then refined per question unless
#'   `order_groups_per_question` is turned off.
#' @param order_groups_per_question Logical, default `TRUE`, `layout = "blocks"`
#'   only. Give every question its own group order, largest first by that
#'   question's own denominator, rather than having them all follow the
#'   sheet-wide order. Questions with different coverage then show their groups
#'   in different orders, which is the point.
#'
#'   The permutation stays inside each grouping variable's run, so the merged
#'   variable band above the columns, the alternating block shading and the
#'   column widths are all unaffected - only which block sits in which column
#'   changes, and every header carries its own name and `(n=)`.
#'
#'   The cost: reading straight down a column no longer follows one group. The
#'   third column may be `Male` on one question and `Female` on the next. Set
#'   this to `FALSE` if you need the columns to line up down the sheet;
#'   `layout = "matrix"` always does, since there every question is a row under
#'   one shared header.
#' @param drop_empty_groups Logical, default `TRUE`. Leave out any group whose
#'   sample size is zero on that sheet, in both the percentage panel and the
#'   count panel. A grouping variable's levels are fixed across the whole table,
#'   so a country with no interviews for the questions on a sheet would
#'   otherwise get a full block of empty (or `#NUM!`) columns. Judged per sheet
#'   rather than per question, so the group columns stay aligned down the sheet;
#'   the Overall block is never dropped. Anything dropped is listed in a readme
#'   note, since silently omitting a country from a table is easy to misread.
#' @param table_sheet_name Fallback sheet name.
#' @param readme_sheet_name Reserved sheet name.
#' @param max_sheets Maximum number of sheets to allow.
#' @param say Progress function.
#' @return A list with `sheets` (each with `name`, `description`, `rows`,
#'   `blocks`) and the resolved `split_by`.
#' @keywords internal
ck_build_sheet_plan <- function(
  dat,
  blocks,
  block_group,
  overall_idx,
  n_index,
  split_by,
  repeat_overall,
  table_sheet_name,
  readme_sheet_name,
  max_sheets,
  say = function(...) invisible(NULL)
) {
  split_by <- as.character(split_by)
  split_by <- split_by[!is.na(split_by) & nzchar(split_by)]

  if (length(split_by) == 0) {
    split_by <- "none"
  }
  if ("none" %in% split_by) {
    if (length(split_by) > 1) {
      warning("'none' cannot be combined with another split; using 'none'.")
    }
    split_by <- "none"
  }

  by_group <- "group_variable" %in% split_by
  row_cols_raw <- setdiff(split_by, c("none", "group_variable"))

  # Accept the pre-rename names as aliases
  alias <- c(
    analysis_var = "question",
    analysis_var_value = "option",
    label_analysis_var = "question_label"
  )
  index_names <- names(dat)[seq_len(n_index)]

  row_cols <- character(0)
  for (cc in row_cols_raw) {
    resolved <- if (cc %in% names(dat)) {
      cc
    } else if (cc %in% names(alias) && alias[[cc]] %in% names(dat)) {
      alias[[cc]]
    } else {
      stop(sprintf(
        "split_by = '%s' is not a column of the table. Available identifier columns are: %s. Use 'group_variable' to split by disaggregation, or 'none' for a single sheet.",
        cc,
        paste(index_names, collapse = ", ")
      ))
    }
    row_cols <- c(row_cols, resolved)
  }

  # --- row levels -------------------------------------------------------------
  if (length(row_cols) > 0) {
    parts <- lapply(row_cols, function(cc) {
      v <- as.character(dat[[cc]])
      v[is.na(v) | v == ""] <- "not specified"
      v
    })
    row_key <- do.call(paste, c(parts, list(sep = " - ")))
    row_levels <- unique(row_key)
  } else {
    row_key <- rep("", nrow(dat))
    row_levels <- ""
  }

  # --- column levels ----------------------------------------------------------
  if (by_group) {
    col_levels <- setdiff(unique(block_group), "Overall")
    if (length(col_levels) == 0) {
      col_levels <- unique(block_group)
    }
    if (length(col_levels) == 1 && identical(col_levels, "results")) {
      say(
        "--> no column_map supplied, so the disaggregation groups cannot be told apart; keeping them on one sheet"
      )
      col_levels <- NA_character_
    }
  } else {
    col_levels <- NA_character_
  }

  # --- assemble ---------------------------------------------------------------
  sheets <- list()

  for (rl in row_levels) {
    rows <- which(row_key == rl)
    if (length(rows) == 0) {
      next
    }

    for (cl in col_levels) {
      if (is.na(cl)) {
        bidx <- seq_along(blocks)
        col_label <- NULL
      } else {
        bidx <- which(block_group == cl)
        if (isTRUE(repeat_overall) && length(overall_idx) > 0) {
          bidx <- sort(unique(c(overall_idx, bidx)))
        }
        col_label <- cl
      }
      if (length(bidx) == 0) {
        next
      }

      label_parts <- c(if (nzchar(rl)) rl else NULL, col_label)
      label <- if (length(label_parts) > 0) {
        paste(label_parts, collapse = " - ")
      } else {
        table_sheet_name
      }

      description <- paste0(
        if (nzchar(rl)) {
          sprintf("%s = %s. ", paste(row_cols, collapse = " / "), rl)
        } else {
          ""
        },
        if (!is.null(col_label)) {
          sprintf("Disaggregated by %s.", col_label)
        } else {
          "All disaggregation groups."
        }
      )

      sheets[[length(sheets) + 1]] <- list(
        rows = rows,
        blocks = bidx,
        label = label,
        description = description
      )
    }
  }

  if (length(sheets) == 0) {
    stop("The split_by specification produced no sheets.")
  }
  if (length(sheets) > max_sheets) {
    stop(sprintf(
      "split_by would create %s sheets (limit %s). Narrow the split or raise max_sheets.",
      length(sheets),
      max_sheets
    ))
  }

  names_out <- ck_safe_sheet_name(
    vapply(sheets, function(z) z$label, character(1)),
    taken = readme_sheet_name
  )
  for (i in seq_along(sheets)) {
    sheets[[i]]$name <- names_out[i]
  }

  say(
    "--> sheet plan: %s sheet(s) [%s]",
    length(sheets),
    paste(split_by, collapse = " + ")
  )

  list(sheets = sheets, split_by = split_by)
}


#' Find the Denominator Column to Read Sample Sizes From
#'
#' The denominator can reach the formatter through either argument: as
#' `total_columns = c("n", "n_total")`, or folded into
#' `value_columns = c("stat", "n", "n_total")` with `total_columns` left `NULL`.
#' The block splitter cannot tell the two apart - it only counts columns - so
#' anything that reads sample sizes must not depend on which was used. It looks
#' at `total_columns` first, then at the base names the blocks actually hold.
#'
#' @param blocks The statistic blocks.
#' @param total_columns The `total_columns` argument, possibly `NULL`.
#' @return A base name, or `NULL` when the blocks carry no count column at all.
#' @keywords internal
ck_denominator_base <- function(blocks, total_columns = NULL) {
  if (length(total_columns) > 0) {
    return(
      if ("n_total" %in% total_columns) {
        "n_total"
      } else {
        total_columns[length(total_columns)]
      }
    )
  }

  present <- unique(unlist(
    lapply(blocks, function(b) b$base_names),
    use.names = FALSE
  ))

  for (candidate in c("n_total", "n")) {
    if (candidate %in% present) {
      return(candidate)
    }
  }

  NULL
}


#' Sample Size of Each Group Block
#'
#' The number of respondents behind a block is the largest denominator recorded
#' for it across the rows considered - the same rule the block headers and the
#' readme composition table use, so the three always agree.
#'
#' @param dat The table.
#' @param blocks The statistic blocks.
#' @param sample_base Base name of the denominator column (e.g. `"n_total"`), or
#'   `NULL` when the table carries no count column.
#' @param rows Optional row indices to restrict to. Defaults to the whole table.
#' @return A numeric vector, one element per block, `NA` where no finite
#'   denominator was recorded.
#' @keywords internal
ck_block_sample_sizes <- function(dat, blocks, sample_base, rows = NULL) {
  if (is.null(sample_base) || length(blocks) == 0) {
    return(rep(NA_real_, length(blocks)))
  }
  if (is.null(rows)) {
    rows <- seq_len(nrow(dat))
  }

  vapply(
    blocks,
    function(b) {
      k <- which(b$base_names == sample_base)
      if (length(k) == 0) {
        return(NA_real_)
      }

      v <- suppressWarnings(as.numeric(dat[[b$cols[k[1]]]][rows]))
      v <- v[is.finite(v)]

      if (length(v) == 0) NA_real_ else max(v)
    },
    numeric(1)
  )
}


#' Order Group Blocks by Sample Size Within Each Grouping Variable
#'
#' Group values come out of the pipeline alphabetically, which puts `Female`
#' before `Male` and `No, Refused, Yes` in that order regardless of how many
#' respondents are behind each. Sorting them largest first puts the substantial
#' columns where they will be read.
#'
#' Only blocks *within* the same grouping variable move, so the merged variable
#' band above them still spans a contiguous stretch and `Overall` - a run of one
#' - stays where it is. The order is decided once for the whole workbook from
#' each block's overall sample size, not per question: the block layout writes
#' one header per question, and re-sorting each of them independently would
#' leave the columns unaligned down the sheet.
#'
#' @param blocks The statistic blocks.
#' @param block_group Grouping variable of each block.
#' @param size Sample size of each block, from [ck_block_sample_sizes()].
#' @return An integer permutation of `seq_along(blocks)`.
#' @keywords internal
ck_order_blocks_by_size <- function(blocks, block_group, size) {
  runs <- rle(as.character(block_group))
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1L

  perm <- integer(0)

  for (i in seq_along(runs$lengths)) {
    idx <- starts[i]:ends[i]
    # idx breaks ties on the original position, so the result is deterministic
    # and blocks of unknown size fall to the end of their own variable.
    perm <- c(perm, idx[order(-size[idx], idx, na.last = TRUE)])
  }

  perm
}


#' Which Group Blocks Actually Have Respondents on This Sheet
#'
#' A grouping variable's levels are fixed across the whole table, so a country
#' with no interviews for the questions on one sheet still gets a full block of
#' columns there - every cell empty, or `#NUM!` where the estimator divided
#' nothing by nothing. Those blocks are dropped.
#'
#' A block counts as having respondents when its denominator column holds a
#' finite value above zero somewhere in the sheet's rows. With no count column
#' to read, the fallback is whether any statistic in the block is finite at all;
#' `NaN` and `Inf` are not, which is exactly the case being removed.
#'
#' The Overall block is never dropped - it is the reference column, and dropping
#' it would leave a sheet with nothing to compare against.
#'
#' @param dat The table.
#' @param rows The sheet's row indices.
#' @param blocks All statistic blocks.
#' @param idx The block indices assigned to this sheet.
#' @param sample_base Base name of the denominator column (e.g. `"n_total"`),
#'   or `NULL`.
#' @return The subset of `idx` worth writing. If every block would go, `idx` is
#'   returned unchanged rather than producing a sheet with no columns.
#' @keywords internal
ck_nonempty_blocks <- function(dat, rows, blocks, idx, sample_base = NULL) {
  if (length(idx) == 0) {
    return(idx)
  }

  has_data <- vapply(
    idx,
    function(i) {
      b <- blocks[[i]]
      if (identical(b$label, "Overall")) {
        return(TRUE)
      }

      k <- if (is.null(sample_base)) {
        integer(0)
      } else {
        which(b$base_names == sample_base)
      }

      if (length(k) > 0) {
        n_i <- ck_block_sample_sizes(dat, list(b), sample_base, rows)[1]
        return(!is.na(n_i) && n_i > 0)
      }

      any(vapply(
        b$cols,
        function(j) {
          any(is.finite(suppressWarnings(as.numeric(dat[[j]][rows]))))
        },
        logical(1)
      ))
    },
    logical(1)
  )

  if (!any(has_data)) {
    return(idx)
  }

  idx[has_data]
}


#' Summarise the Sample Composition
#'
#' The number of respondents in a group is the largest denominator recorded for
#' that group across all the analyses; the share is that number over the whole
#' sample.
#'
#' @param dat The table.
#' @param blocks All statistic blocks.
#' @param block_group Grouping variable of each block.
#' @param sample_base Base name of the denominator column (e.g. `"n_total"`).
#' @return A list with `total` and `table` (disaggregation / group / share / n /
#'   n_total), or `NULL` when there is no denominator column to read.
#' @keywords internal
ck_sample_composition <- function(dat, blocks, block_group, sample_base) {
  if (is.null(sample_base)) {
    return(NULL)
  }

  group_n <- ck_block_sample_sizes(dat, blocks, sample_base)

  labels <- vapply(blocks, function(b) b$label, character(1))
  is_overall <- labels == "Overall"

  total <- if (any(is_overall) && any(is.finite(group_n[is_overall]))) {
    max(group_n[is_overall], na.rm = TRUE)
  } else if (any(is.finite(group_n))) {
    max(group_n, na.rm = TRUE)
  } else {
    NA_real_
  }

  if (!is.finite(total)) {
    return(NULL)
  }

  keep <- !is_overall & is.finite(group_n)

  list(
    total = total,
    table = if (any(keep)) {
      data.frame(
        disaggregation = block_group[keep],
        group = labels[keep],
        share = group_n[keep] / total,
        n = group_n[keep],
        n_total = rep(total, sum(keep)),
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }
  )
}


# -----------------------------------------------------------------------------
# 5. Main entry point
# -----------------------------------------------------------------------------

#' Format XLSX Variable by Group Using MMC Branding
#'
#' Writes an analysis table to an Excel workbook in a variable-by-group layout
#' styled with the MMC palette: a navy (`#003D58`) band for the merged
#' disaggregation-group headers, a teal (`#00A2A5`) band for the column headers,
#' a light blue index panel on the left for the question / option columns, and
#' alternating white / pale blue shading per block of statistics.
#'
#' Built for the wide outputs of `run_group_analysis_pipeline()`, which can run
#' to thousands of columns when many countries or towns are used as grouping
#' variables. With `column_map` from the pipeline, each grouping variable is
#' written to its own sheet, repeating the index panel and, optionally, the
#' Overall block.
#'
#' @param table_group_x_variable A data frame, a named list containing
#'   `table_name`, or the list returned by `run_group_analysis_pipeline()`
#'   (which supplies both `combined_results` and `column_map`).
#' @param file_path Output path. `NULL` (default) returns the workbook.
#' @param table_name Name of the element holding the table when a list is
#'   supplied. `"combined_results"` is picked up automatically.
#' @param value_columns Statistic column prefixes.
#' @param total_columns Count column prefixes (e.g. `c("n", "n_total")`),
#'   formatted as whole numbers.
#' @param readme_sheet_name Name of the readme sheet.
#' @param table_sheet_name Name of the table sheet; used only when a sheet name
#'   cannot be derived.
#' @param overwrite Logical, overwrite an existing file.
#' @param layout `"matrix"` (default) writes the wide variable-by-group table:
#'   one row per question option, one block of columns per disaggregation group.
#'   `"blocks"` writes one small table per question - the question on top, the
#'   disaggregation variable band under it, then a header row naming each group
#'   with its sample size, then the answer categories - with the percentages on
#'   the left of the sheet and the matching counts on the right.
#' @param insert_empty_rows Logical. Insert a blank row between question groups.
#' @param empty_rows_col Column used for grouping when inserting empty rows.
#' @param sort_within_groups Logical, sort rows inside each group by the first
#'   `stat_` column, descending.
#' @param split_by How results are spread across sheets. Any combination of
#'   `"none"`, `"group_variable"` (one sheet per disaggregation variable, needs
#'   `column_map`) and any column name (one sheet per value of that column - use
#'   for `sector`, `indicator`, `analysis_type` or any LOA column carried
#'   through by `extra_columns`). `c("sector", "group_variable")` writes one
#'   sheet per sector and grouping variable. `"analysis_var"` and
#'   `"analysis_var_value"` are accepted as aliases of the renamed `question`
#'   and `option` columns.
#' @param max_sheets Refuse to build more sheets than this, so a typo in
#'   `split_by` cannot produce hundreds of sheets.
#' @param split_by_group_variable Deprecated logical kept for backwards
#'   compatibility. `TRUE` maps to `split_by = "group_variable"`, `FALSE` to
#'   `"none"`. Ignored when `split_by` is given explicitly.
#' @param column_map Optional data frame with `group_variable` and `column`, as
#'   returned by `run_group_analysis_pipeline()`.
#' @param repeat_overall Logical. Repeat the Overall block on every sheet.
#' @param short_group_labels Logical. In `layout = "blocks"`, drop the
#'   disaggregation variable prefix from each group label, so
#'   `Respondent_Gender_Female (n=11422)` reads `Female (n=11422)` under the
#'   `Respondent_Gender` band. `Overall` is never touched.
#' @param colour_scale Logical. Apply the per-question colour scale to the first
#'   statistic column.
#' @param max_colour_scale_rules Skip the colour scale when it would need more
#'   rules than this on a sheet (hundreds of rules make the workbook slow to
#'   open in Excel).
#' @param round_digits Optional integer. Round numeric statistic columns before
#'   writing. Shortens the XML noticeably on very large tables.
#' @param percent_digits Decimal places shown on proportions. `0` (the default)
#'   displays `0.668039538714992` as `67%`. This is a display format, so the
#'   cell keeps its full precision and the figures still add up correctly.
#'   Means and medians are unaffected; they keep two decimals.
#' @param hidden_columns Identifier columns to hide on every table sheet.
#'   Defaults to the first three (machine names and analysis type), which are
#'   kept in the file but collapsed out of the way. `NULL` shows everything.
#' @param index_width Width of the visible identifier columns.
#' @param stat_width Width of the statistic columns.
#' @param total_width Width of the count columns named in `total_columns`. Kept
#'   narrow so the statistics of neighbouring disaggregations sit side by side.
#' @param palette Optional named vector of colour overrides. See [mmc_colours()].
#' @param font_name Font used throughout.
#' @param readme_text Optional character vector of extra readme lines.
#' @param verbose Logical, print progress messages.
#'
#' @return An `openxlsx` workbook if `file_path` is NULL, otherwise the file is
#'   written and the path returned invisibly.
#' @export
#' @importFrom openxlsx createWorkbook addWorksheet writeData createStyle addStyle mergeCells freezePane conditionalFormatting saveWorkbook setColWidths setRowHeights
#' @importFrom stringr str_detect
format_my_xlsx_variable_x_group <- function(
  table_group_x_variable,
  file_path = NULL,
  table_name = "variable_x_group_table",
  value_columns = c("stat", "stat_low", "stat_upp"),
  total_columns = NULL,
  readme_sheet_name = "readme",
  table_sheet_name = "variable_x_group_table",
  overwrite = FALSE,
  layout = c("matrix", "blocks"),
  insert_empty_rows = FALSE,
  empty_rows_col = "analysis_var",
  sort_within_groups = TRUE,
  split_by = "group_variable",
  max_sheets = 60,
  split_by_group_variable = NULL,
  column_map = NULL,
  repeat_overall = TRUE,
  order_groups_by_n = TRUE,
  order_groups_per_question = TRUE,
  drop_empty_groups = TRUE,
  short_group_labels = TRUE,
  colour_scale = TRUE,
  max_colour_scale_rules = 250,
  round_digits = NULL,
  percent_digits = 0,
  hidden_columns = 1:3,
  index_width = 7,
  stat_width = 13,
  total_width = 5,
  palette = NULL,
  font_name = "Arial Narrow",
  readme_text = NULL,
  verbose = TRUE
) {
  t_start <- Sys.time()
  say <- function(...) {
    if (isTRUE(verbose)) message(sprintf(...))
  }

  layout <- match.arg(layout)
  pal <- resolve_mmc_palette(palette)
  pct_fmt <- ck_percent_format(percent_digits)

  # Two files in this project define format_my_xlsx_variable_x_group(), and
  # source() order silently decides which one wins. This line says which is
  # actually running - if it is absent from the console, the older definition
  # loaded after this one and none of the arguments below exist.
  say("--> format_my_xlsx_variable_x_group [%s]", ck_formatter_build())

  # Validate the output path before doing any work - on a large table the
  # workbook takes minutes to build and failing at the end wastes all of it.
  if (!is.null(file_path)) {
    if (stringr::str_detect(file_path, "\\.xlsx", negate = TRUE)) {
      stop("file_path does not contain .xlsx")
    }
    if (file.exists(file_path) && !isTRUE(overwrite)) {
      stop("File already exists and overwrite is FALSE.")
    }
  }

  # --- 1. Input handling ------------------------------------------------------
  if (is.data.frame(table_group_x_variable)) {
    dat <- table_group_x_variable
  } else if (is.list(table_group_x_variable)) {
    element <- if (table_name %in% names(table_group_x_variable)) {
      table_name
    } else if ("combined_results" %in% names(table_group_x_variable)) {
      "combined_results"
    } else {
      stop(sprintf("Cannot identify '%s' element of the list.", table_name))
    }
    dat <- table_group_x_variable[[element]]

    if (
      is.null(column_map) && "column_map" %in% names(table_group_x_variable)
    ) {
      column_map <- table_group_x_variable[["column_map"]]
    }
  } else {
    stop("table_group_x_variable must be a data frame or a list.")
  }

  dat <- as.data.frame(dat, stringsAsFactors = FALSE)
  dat$uuid <- NULL

  original_names <- names(dat)

  rename_map <- c(
    analysis_var = "question",
    analysis_var_value = "option",
    label_analysis_var = "question_label"
  )
  for (old in intersect(names(rename_map), names(dat))) {
    names(dat)[names(dat) == old] <- rename_map[[old]]
  }
  if (empty_rows_col == "analysis_var" && "question" %in% names(dat)) {
    empty_rows_col <- "question"
  }

  # --- 2. Separator rows ------------------------------------------------------
  # Rows are inserted per sheet, after the row split, so a blank row never leaks
  # to the top or bottom of a sheet.
  if (isTRUE(insert_empty_rows) && !empty_rows_col %in% names(dat)) {
    warning(sprintf(
      "insert_empty_rows is TRUE but column '%s' was not found; no empty rows were inserted.",
      empty_rows_col
    ))
    insert_empty_rows <- FALSE
  }

  # --- 3. Column geometry -----------------------------------------------------
  stat_start <- which(startsWith(names(dat), "stat_"))
  if (length(stat_start) == 0) {
    stop("No columns starting with 'stat_' were found in the dataset.")
  }

  n_index <- stat_start[1] - 1
  if (n_index < 1) {
    stop("The table must have at least one identifier column before 'stat_'.")
  }

  stat_length <- length(c(value_columns, total_columns))
  if (stat_length == 1) {
    warning(
      "Length of value_columns/total_columns is one, function cannot strictly check the number of columns."
    )
  }

  all_names <- names(dat)
  n_col_total <- length(all_names)

  # Walk the statistic columns and cut them into blocks of stat_length
  blocks <- list()
  col_index <- n_index + 1

  while (col_index <= n_col_total) {
    if (!startsWith(all_names[col_index], "stat_")) {
      col_index <- col_index + 1
      next
    }

    suffix <- sub("^stat_", "", all_names[col_index])
    block_end <- col_index + stat_length - 1
    if (block_end > n_col_total) {
      stop(
        "Not enough columns to merge based on sets indicated by value_columns/total_columns count. Check your data."
      )
    }

    block_cols <- col_index:block_end
    # Strip the group suffix to recover the base statistic names
    base_names <- sub(
      paste0("_", suffix),
      "",
      all_names[block_cols],
      fixed = TRUE
    )

    blocks[[length(blocks) + 1]] <- list(
      label = if (identical(suffix, "NA")) "Overall" else suffix,
      suffix = suffix,
      cols = block_cols,
      base_names = base_names,
      is_total = if (is.null(total_columns)) {
        rep(FALSE, length(block_cols))
      } else {
        base_names %in% total_columns
      }
    )
    col_index <- block_end + 1
  }

  if (length(blocks) == 0) {
    stop("Could not identify any statistic blocks in the table.")
  }

  recognised <- unlist(lapply(blocks, function(b) b$cols))
  if (length(recognised) != (n_col_total - n_index)) {
    dropped <- setdiff((n_index + 1):n_col_total, recognised)
    warning(sprintf(
      "These columns were not recognised as statistics and are excluded from the output: %s",
      paste(all_names[dropped], collapse = ", ")
    ))
  }

  say(
    "--> table: %s rows x %s columns, %s group block(s)",
    nrow(dat),
    n_col_total,
    length(blocks)
  )

  # --- 4. Optional rounding (smaller XML, faster write) -----------------------
  if (!is.null(round_digits)) {
    for (j in unlist(lapply(blocks, function(b) b$cols))) {
      if (is.numeric(dat[[j]])) {
        dat[[j]] <- round(dat[[j]], round_digits)
      }
    }
  }

  # --- 4b. Blank the non-finite values ---------------------------------------
  # openxlsx writes NaN and Inf as #NUM!. NaN turns up as 0/0 whenever a group
  # has rows but no usable denominator for a question - most often a group whose
  # weights sum to zero. An error value in a report is never wanted, so the cell
  # is blanked. is.na() is no use here: is.na(NaN) is TRUE, so the test has to
  # name NaN and Inf explicitly.
  n_nonfinite <- 0L
  for (j in unlist(lapply(blocks, function(b) b$cols))) {
    if (!is.numeric(dat[[j]])) {
      next
    }

    bad <- is.nan(dat[[j]]) | is.infinite(dat[[j]])
    if (any(bad)) {
      n_nonfinite <- n_nonfinite + sum(bad)
      dat[[j]][bad] <- NA_real_
    }
  }
  if (n_nonfinite > 0) {
    say(
      "--> %s NaN/Inf value(s) blanked so Excel does not show #NUM!",
      n_nonfinite
    )
  }

  # The denominator column: what sample sizes, ordering and emptiness are read
  # from. Resolved off the blocks, not off total_columns alone, so putting the
  # counts in value_columns does not silently disable all three.
  sample_base <- ck_denominator_base(blocks, total_columns)

  if (!is.null(sample_base)) {
    say("--> sample sizes read from the '%s' column of each block", sample_base)
  }

  # --- 5. Decide the sheet split ----------------------------------------------
  block_group <- rep(NA_character_, length(blocks))

  if (!is.null(column_map) && is.data.frame(column_map)) {
    if (all(c("group_variable", "column") %in% names(column_map))) {
      lookup <- stats::setNames(
        as.character(column_map$group_variable),
        as.character(column_map$column)
      )
      for (i in seq_along(blocks)) {
        hit <- lookup[original_names[blocks[[i]]$cols]]
        hit <- hit[!is.na(hit)]
        if (length(hit) > 0) {
          block_group[i] <- hit[1]
        }
      }
    } else {
      warning(
        "column_map must contain 'group_variable' and 'column'; ignoring it."
      )
    }
  }

  # Blocks whose group variable is unknown fall back to their own label
  overall_idx <- which(vapply(
    blocks,
    function(b) identical(b$label, "Overall"),
    logical(1)
  ))
  if (length(overall_idx) > 0) {
    block_group[overall_idx] <- "Overall"
  }
  block_group[is.na(block_group)] <- "results"

  # --- 5b. Order the group columns by sample size ----------------------------
  # Done before the sheet plan, so every index below refers to the final order.
  if (isTRUE(order_groups_by_n)) {
    if (is.null(sample_base)) {
      # Asked for and not done: a message is too easy to miss, and the output
      # then looks exactly like the unsorted one.
      warning(
        "order_groups_by_n is TRUE but there is no count column to sort by, so the group columns are left in table order. Pass total_columns (e.g. c(\"n\", \"n_total\")).",
        call. = FALSE
      )
    } else {
      size <- ck_block_sample_sizes(dat, blocks, sample_base)

      if (all(is.na(size))) {
        warning(
          sprintf(
            "order_groups_by_n is TRUE but no '%s' column was found inside any group block, so the group columns are left in table order. The blocks hold: %s.",
            sample_base,
            paste(
              unique(unlist(lapply(blocks, function(b) b$base_names))),
              collapse = ", "
            )
          ),
          call. = FALSE
        )
      } else {
        perm <- ck_order_blocks_by_size(blocks, block_group, size)

        blocks <- blocks[perm]
        block_group <- block_group[perm]
        size <- size[perm]
        overall_idx <- which(vapply(
          blocks,
          function(b) identical(b$label, "Overall"),
          logical(1)
        ))

        # Print the order that was applied, so a run that silently did nothing
        # cannot be mistaken for one that worked.
        shown <- utils::head(which(block_group != "Overall"), 6)
        say(
          "--> group columns ordered by %s, largest first: %s%s",
          sample_base,
          paste(
            sprintf(
              "%s (n=%s)",
              vapply(blocks[shown], function(b) b$label, character(1)),
              ifelse(is.na(size[shown]), "?", format(size[shown], trim = TRUE))
            ),
            collapse = ", "
          ),
          if (length(blocks) - length(overall_idx) > length(shown)) {
            ", ..."
          } else {
            ""
          }
        )
      }
    }
  }

  # Short group labels for the layouts that show the variable name separately
  for (i in seq_along(blocks)) {
    blocks[[i]]$display <- if (isTRUE(short_group_labels)) {
      ck_short_group_label(blocks[[i]]$label, block_group[i])
    } else {
      blocks[[i]]$label
    }
  }

  # Backwards compatibility with the old logical argument
  if (!is.null(split_by_group_variable)) {
    if (missing(split_by)) {
      split_by <- if (isTRUE(split_by_group_variable)) {
        "group_variable"
      } else {
        "none"
      }
    } else {
      warning(
        "Both split_by and split_by_group_variable were supplied; split_by wins."
      )
    }
  }

  plan <- ck_build_sheet_plan(
    dat = dat,
    blocks = blocks,
    block_group = block_group,
    overall_idx = overall_idx,
    n_index = n_index,
    split_by = split_by,
    repeat_overall = repeat_overall,
    table_sheet_name = table_sheet_name,
    readme_sheet_name = readme_sheet_name,
    max_sheets = max_sheets,
    say = say
  )

  # --- 6. Workbook ------------------------------------------------------------
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(
    wb,
    sheetName = readme_sheet_name,
    gridLines = FALSE,
    tabColour = pal[["navy"]]
  )

  if (isTRUE(order_groups_per_question) && !identical(layout, "blocks")) {
    say(
      "--> order_groups_per_question applies to layout = 'blocks' only; these sheets use the shared order"
    )
  }

  dropped_group_labels <- character(0)

  for (s in seq_along(plan$sheets)) {
    sh <- plan$sheets[[s]]

    if (isTRUE(drop_empty_groups)) {
      keep_blocks <- ck_nonempty_blocks(
        dat,
        sh$rows,
        blocks,
        sh$blocks,
        sample_base
      )
      gone <- setdiff(sh$blocks, keep_blocks)

      if (length(gone) > 0) {
        gone_labels <- vapply(blocks[gone], function(b) b$label, character(1))
        dropped_group_labels <- unique(c(dropped_group_labels, gone_labels))

        say(
          "--> %s: %s group(s) left out for having no respondents (%s)",
          sh$name,
          length(gone),
          paste(utils::head(gone_labels, 8), collapse = ", ")
        )
        sh$blocks <- keep_blocks
      }
    }

    say(
      "--> writing sheet %s/%s: %s (%s rows, %s group block(s))",
      s,
      length(plan$sheets),
      sh$name,
      length(sh$rows),
      length(sh$blocks)
    )

    sheet_dat <- dat[sh$rows, , drop = FALSE]

    if (identical(layout, "blocks")) {
      # Each question becomes its own little table, so the blank separator rows
      # of the matrix layout are not used here - only the ordering is.
      if (isTRUE(sort_within_groups)) {
        sheet_dat <- ck_sort_within_groups(sheet_dat, empty_rows_col)
        sheet_dat <- sheet_dat[
          !is.na(sheet_dat[[empty_rows_col]]),
          ,
          drop = FALSE
        ]
      }
      ck_write_block_sheet(
        wb = wb,
        sheet = sh$name,
        dat = sheet_dat,
        blocks = blocks[sh$blocks],
        block_group = block_group[sh$blocks],
        value_columns = value_columns,
        total_columns = total_columns,
        order_per_question = order_groups_per_question,
        pal = pal,
        font_name = font_name,
        colour_scale = colour_scale,
        max_colour_scale_rules = max_colour_scale_rules,
        pct_fmt = pct_fmt
      )
    } else {
      if (isTRUE(insert_empty_rows)) {
        sheet_dat <- add_empty_rows_between_groups(
          sheet_dat,
          empty_rows_col,
          sort_desc = sort_within_groups
        )
      }
      ck_write_group_sheet(
        wb = wb,
        sheet = sh$name,
        dat = sheet_dat,
        n_index = n_index,
        blocks = blocks[sh$blocks],
        value_columns = value_columns,
        pal = pal,
        font_name = font_name,
        colour_scale = colour_scale,
        max_colour_scale_rules = max_colour_scale_rules,
        hidden_columns = hidden_columns,
        index_width = index_width,
        stat_width = stat_width,
        total_width = total_width,
        pct_fmt = pct_fmt
      )
    }
  }

  # --- 7. Readme --------------------------------------------------------------
  is_matrix <- identical(layout, "matrix")

  ck_write_readme_sheet(
    wb = wb,
    sheet = readme_sheet_name,
    pal = pal,
    font_name = font_name,
    composition = ck_sample_composition(dat, blocks, block_group, sample_base),
    sheet_table = data.frame(
      sheet = vapply(plan$sheets, function(z) z$name, character(1)),
      content = vapply(plan$sheets, function(z) z$description, character(1)),
      stringsAsFactors = FALSE
    ),
    split_by = plan$split_by,
    hidden_columns = if (is_matrix) hidden_columns else integer(0),
    hidden_names = if (is_matrix && length(hidden_columns) > 0) {
      names(dat)[intersect(hidden_columns, seq_len(n_index))]
    } else {
      character(0)
    },
    dropped_groups = dropped_group_labels,
    readme_text = readme_text,
    pct_fmt = pct_fmt
  )

  say(
    "--> workbook built in %.1f s",
    as.numeric(difftime(Sys.time(), t_start, units = "secs"))
  )

  # --- 8. Output --------------------------------------------------------------
  if (is.null(file_path)) {
    return(wb)
  }

  openxlsx::saveWorkbook(wb, file_path, overwrite = overwrite)
  say(
    "--> saved '%s' (total %.1f s)",
    file_path,
    as.numeric(difftime(Sys.time(), t_start, units = "secs"))
  )
  invisible(file_path)
}


# -----------------------------------------------------------------------------
# 6. Matrix layout: one sheet, index panel plus statistic blocks
# -----------------------------------------------------------------------------

#' Write One Variable-by-Group Sheet
#'
#' Writes the index panel plus a set of statistic blocks to a single worksheet
#' and applies the MMC styling through a style collector, so the number of
#' openxlsx style objects stays constant regardless of the number of blocks.
#'
#' @param wb An openxlsx workbook.
#' @param sheet Sheet name (the sheet is created here).
#' @param dat The rows for this sheet.
#' @param n_index Number of identifier columns on the left.
#' @param blocks List of block descriptions.
#' @param value_columns Statistic column prefixes.
#' @param pal Resolved MMC palette.
#' @param font_name Font used throughout.
#' @param colour_scale Logical, apply the per-question colour scale.
#' @param max_colour_scale_rules Rule ceiling for the colour scale.
#' @param hidden_columns Identifier columns to hide.
#' @param index_width,stat_width,total_width Column widths.
#' @param pct_fmt Excel number format for proportion rows, from
#'   [ck_percent_format()].
#' @return Invisibly the number of style objects written.
#' @keywords internal
ck_write_group_sheet <- function(
  wb,
  sheet,
  dat,
  n_index,
  blocks,
  value_columns,
  pal,
  font_name,
  colour_scale = TRUE,
  max_colour_scale_rules = 250,
  hidden_columns = 1:3,
  index_width = 7,
  stat_width = 13,
  total_width = 5,
  pct_fmt = ck_percent_format(0)
) {
  index_cols <- seq_len(n_index)
  block_cols <- unlist(lapply(blocks, function(b) b$cols))
  keep_cols <- c(index_cols, block_cols)

  sheet_dat <- dat[, keep_cols, drop = FALSE]
  n_row <- nrow(sheet_dat)
  n_col <- ncol(sheet_dat)
  row_last <- n_row + 2L

  openxlsx::addWorksheet(
    wb,
    sheetName = sheet,
    gridLines = FALSE,
    tabColour = pal[["teal"]]
  )
  openxlsx::writeData(wb, sheet = sheet, x = sheet_dat, startRow = 2)

  # Re-map the block column positions into this sheet's coordinates
  pos <- match(block_cols, keep_cols)
  offset <- 0
  for (i in seq_along(blocks)) {
    len <- length(blocks[[i]]$cols)
    blocks[[i]]$sheet_cols <- pos[(offset + 1):(offset + len)]
    offset <- offset + len
  }

  # --- Row 1: merged group labels, row 2: stripped headers --------------------
  header_names <- names(sheet_dat)
  merges <- character(0)

  for (i in seq_along(blocks)) {
    cols_i <- blocks[[i]]$sheet_cols
    merges <- c(merges, ck_row_ref(1, cols_i[1], cols_i[length(cols_i)]))
    openxlsx::writeData(
      wb,
      sheet = sheet,
      x = blocks[[i]]$label,
      startRow = 1,
      startCol = cols_i[1],
      colNames = FALSE,
      rowNames = FALSE
    )
    header_names[cols_i] <- blocks[[i]]$base_names
  }

  if (n_index >= 2) {
    merges <- c(merges, ck_row_ref(1, 1, n_index))
  }
  ck_add_merges(wb, sheet, merges)

  openxlsx::writeData(
    wb,
    sheet = sheet,
    x = t(header_names),
    startRow = 2,
    startCol = 1,
    colNames = FALSE,
    rowNames = FALSE
  )

  # --- Row classes ------------------------------------------------------------
  if ("analysis_type" %in% names(sheet_dat)) {
    at <- tolower(as.character(sheet_dat[["analysis_type"]]))
    is_dec <- !is.na(at) & grepl("mean|median", at)
    is_pct <- !is.na(at) & !is_dec
  } else {
    is_dec <- rep(TRUE, n_row)
    is_pct <- rep(FALSE, n_row)
  }

  # Separator rows: every identifier cell empty
  is_sep <- rowSums(!is.na(sheet_dat[, index_cols, drop = FALSE])) == 0
  is_pct <- is_pct & !is_sep
  is_dec <- is_dec & !is_sep

  rows_sep <- which(is_sep) + 2L

  # The last data row carries a thick bottom border, so each row class is split
  # into "everything else" and "the last row".
  split_last <- function(flag) {
    rows <- which(flag) + 2L
    list(main = rows[rows != row_last], last = rows[rows == row_last])
  }
  r_pct <- split_last(is_pct)
  r_dec <- split_last(is_dec)

  # --- Styles -----------------------------------------------------------------
  sg <- ck_new_style_grid()

  cell <- function(
    fill,
    halign,
    numfmt = NULL,
    deco = NULL,
    edges = character(0),
    bottom = FALSE
  ) {
    ck_body_style(
      sg,
      pal,
      font_name,
      fill = fill,
      halign = halign,
      numfmt = numfmt,
      deco = deco,
      thick = c(edges, if (bottom) "Bottom" else character(0))
    )
  }

  # Separator rows: plain white, no borders
  sep_key <- ck_style(
    sg,
    fontSize = 11,
    fontName = font_name,
    fontColour = pal[["navy"]],
    fgFill = pal[["white"]]
  )

  # 1. Index panel
  key_col <- which(names(sheet_dat) == "question")
  key_col <- if (length(key_col) == 0) 1L else key_col[1]

  for (j in index_cols) {
    edges <- c(if (j == 1) "Left", if (j == n_index) "Right")
    is_key <- identical(j, key_col)
    fill <- if (is_key) pal[["light_blue"]] else pal[["pale_blue"]]
    deco <- if (is_key) "bold" else ""

    for (rr in list(r_pct, r_dec)) {
      ck_add_rect(
        sg,
        cell(fill, "left", deco = deco, edges = edges),
        rr$main,
        j
      )
      ck_add_rect(
        sg,
        cell(fill, "left", deco = deco, edges = edges, bottom = TRUE),
        rr$last,
        j
      )
    }
  }
  ck_add_rect(sg, sep_key, rows_sep, index_cols)

  # 2. Statistic blocks
  for (i in seq_along(blocks)) {
    b <- blocks[[i]]
    fill <- if ((i %% 2) == 0) pal[["off_white"]] else pal[["white"]]
    cols_i <- b$sheet_cols

    for (k in seq_along(cols_i)) {
      j <- cols_i[k]
      edges <- c(if (k == 1) "Left", if (k == length(cols_i)) "Right")

      # Counts are whole numbers on every row; statistics take the format of
      # their analysis type. The built-in numFmt ids ("NUMBER" = 2, "3") avoid
      # openxlsx rescanning every style object for a free custom numFmtId on
      # each addStyle() call. Proportions use an explicit format string instead,
      # because the built-in "PERCENTAGE" id is fixed at two decimals.
      fmt_rows <- if (isTRUE(b$is_total[k])) {
        list(
          `3` = list(
            main = c(r_pct$main, r_dec$main),
            last = c(r_pct$last, r_dec$last)
          )
        )
      } else {
        stats::setNames(list(r_pct, r_dec), c(pct_fmt, "NUMBER"))
      }

      for (fmt in names(fmt_rows)) {
        rr <- fmt_rows[[fmt]]
        ck_add_rect(
          sg,
          cell(fill, "center", numfmt = fmt, edges = edges),
          rr$main,
          j
        )
        ck_add_rect(
          sg,
          cell(fill, "center", numfmt = fmt, edges = edges, bottom = TRUE),
          rr$last,
          j
        )
      }
    }

    ck_add_rect(sg, sep_key, rows_sep, cols_i)
  }

  # 3. Header bands
  ck_add_rect(
    sg,
    ck_band_style(sg, pal, font_name, 12, pal[["navy"]], "center"),
    1L,
    seq_len(n_col)
  )
  ck_add_rect(
    sg,
    ck_band_style(sg, pal, font_name, 11, pal[["teal"]], "center", wrap = TRUE),
    2L,
    (n_index + 1L):n_col
  )
  ck_add_rect(
    sg,
    ck_band_style(sg, pal, font_name, 11, pal[["navy"]], "left", wrap = TRUE),
    2L,
    index_cols
  )

  n_style_objects <- ck_flush_styles(sg, wb, sheet)

  # --- Conditional formatting -------------------------------------------------
  if (isTRUE(colour_scale)) {
    # Overall column only - a colour scale on every disaggregation is unreadable
    cf_block <- which(vapply(
      blocks,
      function(b) identical(b$label, "Overall"),
      logical(1)
    ))
    cf_block <- if (length(cf_block) > 0) cf_block[1] else 1L
    first_stat <- blocks[[cf_block]]$sheet_cols[1]

    r <- rle(!is.na(sheet_dat[[first_stat]]))
    ends <- cumsum(r$lengths)
    starts <- ends - r$lengths + 1L
    groups <- which(r$values & r$lengths > 1L)

    if (length(groups) > max_colour_scale_rules) {
      message(sprintf(
        "--> colour scale skipped on '%s': %s rules needed (limit %s). Raise max_colour_scale_rules to force it.",
        sheet,
        length(groups),
        max_colour_scale_rules
      ))
    } else {
      for (g in groups) {
        openxlsx::conditionalFormatting(
          wb,
          sheet = sheet,
          cols = first_stat,
          rows = (starts[g]:ends[g]) + 2L,
          type = "colourScale",
          style = c(pal[["white"]], pal[["pale_blue"]], pal[["teal"]])
        )
      }
    }
  }

  # --- Panes and sizing -------------------------------------------------------
  option_col <- which(names(sheet_dat) %in% c("option", "analysis_var_value"))
  freeze_col <- if (length(option_col) > 0) option_col[1] + 1L else n_index + 1L
  freeze_col <- min(freeze_col, n_col)

  openxlsx::freezePane(
    wb,
    sheet,
    firstActiveRow = 3,
    firstActiveCol = freeze_col
  )

  # Identifier panel: hide the machine-name columns, keep the rest narrow
  hide <- intersect(as.integer(hidden_columns), index_cols)
  shown <- setdiff(index_cols, hide)

  if (length(hide) > 0) {
    openxlsx::setColWidths(
      wb,
      sheet = sheet,
      cols = hide,
      widths = 8.43,
      hidden = TRUE
    )
  }
  if (length(shown) > 0) {
    openxlsx::setColWidths(
      wb,
      sheet = sheet,
      cols = shown,
      widths = index_width
    )
  }

  # Statistics: counts stay narrow so neighbouring disaggregations line up
  total_cols <- unlist(lapply(blocks, function(b) {
    b$sheet_cols[which(b$is_total)]
  }))
  value_cols <- setdiff((n_index + 1L):n_col, total_cols)

  if (length(value_cols) > 0) {
    openxlsx::setColWidths(
      wb,
      sheet = sheet,
      cols = value_cols,
      widths = stat_width
    )
  }
  if (length(total_cols) > 0) {
    openxlsx::setColWidths(
      wb,
      sheet = sheet,
      cols = total_cols,
      widths = total_width
    )
  }

  openxlsx::setRowHeights(wb, sheet = sheet, rows = 1:2, heights = c(26, 32))
  if (length(rows_sep) > 0) {
    openxlsx::setRowHeights(wb, sheet = sheet, rows = rows_sep, heights = 6)
  }

  invisible(n_style_objects)
}


# -----------------------------------------------------------------------------
# 7. Blocks layout: one small table per question
# -----------------------------------------------------------------------------

#' Write One Question-Block Sheet
#'
#' Each question becomes a small table: the question label on top, the
#' disaggregation variable band under it, then a header row naming every group
#' with its sample size, then one row per answer category. Percentages sit on
#' the left panel and the matching counts on the right, separated by a spacer
#' column, so a figure and its count can be read without scrolling.
#'
#' @param wb An openxlsx workbook.
#' @param sheet Sheet name (the sheet is created here).
#' @param dat The rows for this sheet.
#' @param blocks Blocks to show, in column order.
#' @param block_group Grouping variable of each block.
#' @param value_columns Statistic column prefixes; the first is the one shown.
#' @param total_columns Count column prefixes.
#' @param order_per_question Logical. Order each question's groups by its own
#'   denominator, largest first, instead of following the sheet-wide order.
#' @param pal Resolved MMC palette.
#' @param font_name Font used throughout.
#' @param colour_scale Logical, shade the percentage block of each question.
#' @param max_colour_scale_rules Rule ceiling for the colour scale.
#' @param category_width Width of the category columns.
#' @param group_width Width of the group value columns.
#' @param pct_fmt Excel number format for proportion rows, from
#'   [ck_percent_format()].
#' @return Invisibly the number of style objects written.
#' @keywords internal
ck_write_block_sheet <- function(
  wb,
  sheet,
  dat,
  blocks,
  block_group,
  value_columns,
  total_columns,
  pal,
  font_name,
  order_per_question = TRUE,
  colour_scale = TRUE,
  max_colour_scale_rules = 250,
  category_width = 46,
  group_width = 14,
  pct_fmt = ck_percent_format(0)
) {
  n_group <- length(blocks)

  # --- Which base statistic goes where ---------------------------------------
  stat_base <- value_columns[1]
  count_base <- NULL

  # The (n=) in each header comes from the denominator wherever it lives, so a
  # count folded into value_columns still labels the columns.
  denom_base <- ck_denominator_base(blocks, total_columns)

  # The right-hand count panel is only drawn when the counts were declared as
  # total_columns - that argument is what says "show these as counts".
  if (length(total_columns) > 0) {
    others <- setdiff(total_columns, denom_base)
    count_base <- if (length(others) > 0) others[1] else denom_base
  }
  show_counts <- !is.null(count_base)

  col_of <- function(base) {
    if (is.null(base)) {
      return(rep(NA_integer_, n_group))
    }
    vapply(
      blocks,
      function(b) {
        k <- which(b$base_names == base)
        if (length(k) == 0) NA_integer_ else b$cols[k[1]]
      },
      integer(1)
    )
  }

  stat_cols <- col_of(stat_base)
  count_cols <- if (show_counts) {
    col_of(count_base)
  } else {
    rep(NA_integer_, n_group)
  }
  denom_cols <- col_of(denom_base)

  if (all(is.na(stat_cols))) {
    stop("Could not find the statistic column of any group block.")
  }

  # --- Sheet geometry --------------------------------------------------------
  # The left panel holds the statistics, the right panel the matching counts.
  # Everything below loops over `panels`, so the two stay in step.
  left_cat <- 1L
  left_first <- 2L
  spacer <- left_first + n_group
  right_cat <- spacer + 1L
  right_first <- right_cat + 1L

  panels <- list(list(
    cat = left_cat,
    first = left_first,
    numfmt = NULL,
    cols = stat_cols
  ))
  if (show_counts) {
    panels[[2]] <- list(
      cat = right_cat,
      first = right_first,
      numfmt = "3",
      cols = count_cols
    )
  }
  for (p in seq_along(panels)) {
    panels[[p]]$last <- panels[[p]]$first + n_group - 1L
  }
  n_col <- panels[[length(panels)]]$last

  openxlsx::addWorksheet(
    wb,
    sheetName = sheet,
    gridLines = FALSE,
    tabColour = pal[["teal"]]
  )

  # Group variable runs, used for the band and for the alternating shading
  gv_runs <- rle(as.character(block_group))
  run_end <- cumsum(gv_runs$lengths)
  run_start <- run_end - gv_runs$lengths + 1L
  parity <- rep(seq_along(gv_runs$lengths) %% 2 == 0, gv_runs$lengths)

  # Question runs
  if (!"question" %in% names(dat)) {
    stop("The block layout needs a 'question' column.")
  }
  q_rle <- rle(as.character(dat[["question"]]))
  q_end <- cumsum(q_rle$lengths)
  q_start <- q_end - q_rle$lengths + 1L

  label_col <- if ("question_label" %in% names(dat)) {
    "question_label"
  } else {
    "question"
  }
  option_col <- if ("option" %in% names(dat)) "option" else "question"

  # --- Styles ---------------------------------------------------------------
  sg <- ck_new_style_grid()

  title_key <- ck_style(
    sg,
    fontSize = 12,
    fontName = font_name,
    fontColour = pal[["white"]],
    fgFill = pal[["navy"]],
    textDecoration = "bold",
    halign = "left",
    valign = "center",
    indent = 1
  )
  band_key <- ck_band_style(
    sg,
    pal,
    font_name,
    11,
    pal[["teal"]],
    "center",
    border_colour = pal[["white"]]
  )
  head_key <- ck_style(
    sg,
    fontSize = 11,
    fontName = font_name,
    fontColour = pal[["navy"]],
    fgFill = pal[["light_blue"]],
    textDecoration = "bold",
    halign = "center",
    valign = "center",
    wrapText = TRUE,
    border = "TopBottomLeftRight",
    borderColour = pal[["white"]],
    borderStyle = "thin"
  )
  head_cat_key <- ck_style(
    sg,
    fontSize = 11,
    fontName = font_name,
    fontColour = pal[["navy"]],
    fgFill = pal[["light_blue"]],
    textDecoration = "bold",
    halign = "left",
    valign = "center",
    border = "TopBottomLeftRight",
    borderColour = pal[["white"]],
    borderStyle = "thin"
  )

  cat_key <- function(bottom) {
    ck_body_style(
      sg,
      pal,
      font_name,
      fill = pal[["pale_blue"]],
      halign = "left",
      thick = if (bottom) c("Left", "Bottom") else "Left"
    )
  }
  cell_key <- function(shaded, edges, numfmt, bottom) {
    ck_body_style(
      sg,
      pal,
      font_name,
      fill = if (shaded) pal[["off_white"]] else pal[["white"]],
      halign = "center",
      numfmt = numfmt,
      thick = c(edges, if (bottom) "Bottom" else character(0))
    )
  }

  # The colour scale is only ever applied to the Overall column of the left panel
  cf_group <- which(vapply(
    blocks,
    function(b) identical(b$label, "Overall"),
    logical(1)
  ))
  cf_group <- if (length(cf_group) > 0) cf_group[1] else 1L

  merges <- character(0)
  cf_ranges <- list()
  row <- 1L

  for (qi in which(!is.na(q_rle$values))) {
    rows_q <- q_start[qi]:q_end[qi]
    n_r <- length(rows_q)

    q_label <- as.character(dat[[label_col]][rows_q[1]])
    if (is.na(q_label) || q_label == "") {
      q_label <- as.character(dat[["question"]][rows_q[1]])
    }
    title <- paste0("VARIABLE: ", q_label)

    # Sample size per group for this question
    denom_q <- rep(NA_real_, n_group)
    for (g in seq_len(n_group)) {
      if (is.na(denom_cols[g])) {
        next
      }

      v <- suppressWarnings(as.numeric(dat[[denom_cols[g]]][rows_q]))
      v <- v[is.finite(v)]
      if (length(v) > 0) denom_q[g] <- max(v)
    }

    # This question's own group order. The permutation stays inside each
    # grouping variable's run, so run_start, run_end and parity below stay
    # correct as they are: the band merges, the shading and the column widths
    # are all positional and do not move. Only which block a column draws from
    # changes. With no denominator to read, ck_order_blocks_by_size() leaves the
    # order alone.
    ord <- if (isTRUE(order_per_question)) {
      ck_order_blocks_by_size(blocks, block_group, denom_q)
    } else {
      seq_len(n_group)
    }

    head_txt <- vapply(
      seq_len(n_group),
      function(g) {
        b <- blocks[[ord[g]]]
        lbl <- if (!is.null(b$display)) b$display else b$label
        denom <- denom_q[ord[g]]

        if (is.finite(denom)) {
          sprintf(
            "%s (n=%s)",
            lbl,
            format(round(denom), scientific = FALSE, trim = TRUE)
          )
        } else {
          lbl
        }
      },
      character(1)
    )

    # --- three header rows, both panels at once ------------------------------
    hdr <- matrix(NA_character_, nrow = 3, ncol = n_col)
    for (p in panels) {
      hdr[1, p$cat] <- title
      hdr[2, p$first + run_start - 1L] <- gv_runs$values
      hdr[3, p$cat] <- "Category"
      hdr[3, p$first:p$last] <- head_txt
    }
    openxlsx::writeData(
      wb,
      sheet = sheet,
      x = hdr,
      startRow = row,
      startCol = 1,
      colNames = FALSE,
      rowNames = FALSE
    )

    # --- data ----------------------------------------------------------------
    opt <- as.character(dat[[option_col]][rows_q])
    body <- list(opt)

    for (p in seq_along(panels)) {
      if (p > 1) {
        body <- c(body, list(rep(NA_real_, n_r)), list(opt))
      }
      body <- c(
        body,
        lapply(panels[[p]]$cols[ord], function(j) {
          if (is.na(j)) rep(NA_real_, n_r) else dat[[j]][rows_q]
        })
      )
    }

    names(body) <- paste0("v", seq_along(body))
    openxlsx::writeData(
      wb,
      sheet = sheet,
      x = as.data.frame(body, stringsAsFactors = FALSE),
      startRow = row + 3L,
      startCol = 1,
      colNames = FALSE,
      rowNames = FALSE
    )

    # --- merges, headers -----------------------------------------------------
    for (p in panels) {
      merges <- c(merges, ck_row_ref(row, p$cat, p$last))
      for (k in seq_along(gv_runs$lengths)) {
        merges <- c(
          merges,
          ck_row_ref(
            row + 1L,
            p$first + run_start[k] - 1L,
            p$first + run_end[k] - 1L
          )
        )
      }

      ck_add_rect(sg, title_key, row, p$cat:p$last)
      ck_add_rect(sg, band_key, row + 1L, p$first:p$last)
      ck_add_rect(sg, head_cat_key, row + 2L, p$cat)
      ck_add_rect(sg, head_key, row + 2L, p$first:p$last)
    }

    # --- body ----------------------------------------------------------------
    data_rows <- (row + 3L):(row + 2L + n_r)
    last_row <- data_rows[n_r]
    main_rows <- data_rows[-n_r]

    # Percentages or decimals depending on the analysis type of each row
    if ("analysis_type" %in% names(dat)) {
      at <- tolower(as.character(dat[["analysis_type"]][rows_q]))
      is_dec <- !is.na(at) & grepl("mean|median", at)
    } else {
      is_dec <- rep(FALSE, n_r)
    }
    rows_pct <- data_rows[!is_dec]
    rows_dec <- data_rows[is_dec]

    for (p in panels) {
      ck_add_rect(sg, cat_key(FALSE), main_rows, p$cat)
      ck_add_rect(sg, cat_key(TRUE), last_row, p$cat)

      for (g in seq_len(n_group)) {
        edges <- c(if (g %in% run_start) "Left", if (g %in% run_end) "Right")
        col_g <- p$first + g - 1L
        sh <- parity[g]

        if (is.null(p$numfmt)) {
          for (fmt in c(pct_fmt, "NUMBER")) {
            rr <- if (identical(fmt, pct_fmt)) rows_pct else rows_dec
            ck_add_rect(
              sg,
              cell_key(sh, edges, fmt, FALSE),
              rr[rr != last_row],
              col_g
            )
            ck_add_rect(
              sg,
              cell_key(sh, edges, fmt, TRUE),
              rr[rr == last_row],
              col_g
            )
          }
        } else {
          ck_add_rect(
            sg,
            cell_key(sh, edges, p$numfmt, FALSE),
            main_rows,
            col_g
          )
          ck_add_rect(sg, cell_key(sh, edges, p$numfmt, TRUE), last_row, col_g)
        }
      }
    }

    # Colour scale on the Overall column only - shading every disaggregation
    # makes the sheet unreadable
    if (isTRUE(colour_scale) && length(rows_pct) > 1) {
      cf_ranges[[length(cf_ranges) + 1]] <- list(
        rows = rows_pct,
        cols = left_first + which(ord == cf_group) - 1L
      )
    }

    row <- last_row + 3L
  }

  n_style_objects <- ck_flush_styles(sg, wb, sheet)
  ck_add_merges(wb, sheet, merges)

  if (length(cf_ranges) > 0) {
    if (length(cf_ranges) > max_colour_scale_rules) {
      message(sprintf(
        "--> colour scale skipped on '%s': %s rules needed (limit %s).",
        sheet,
        length(cf_ranges),
        max_colour_scale_rules
      ))
    } else {
      for (cf in cf_ranges) {
        openxlsx::conditionalFormatting(
          wb,
          sheet = sheet,
          cols = cf$cols,
          rows = cf$rows,
          type = "colourScale",
          style = c(pal[["white"]], pal[["pale_blue"]], pal[["teal"]])
        )
      }
    }
  }

  openxlsx::setColWidths(
    wb,
    sheet = sheet,
    cols = unlist(lapply(panels, function(p) p$cat)),
    widths = category_width
  )
  openxlsx::setColWidths(
    wb,
    sheet = sheet,
    cols = unlist(lapply(panels, function(p) p$first:p$last)),
    widths = group_width
  )
  if (show_counts) {
    openxlsx::setColWidths(wb, sheet = sheet, cols = spacer, widths = 3)
  }

  openxlsx::freezePane(wb, sheet, firstActiveRow = 1, firstActiveCol = 2)

  invisible(n_style_objects)
}


# -----------------------------------------------------------------------------
# 8. Readme sheet
# -----------------------------------------------------------------------------

#' Write the Branded Readme Sheet
#'
#' An MMC-branded cover sheet holding the sample composition across every
#' disaggregation variable, a directory of the sheets in the workbook, the
#' colour key and the reading notes.
#'
#' This sheet is small and written once, so it uses `addStyle()` directly rather
#' than the style collector.
#'
#' @param wb An openxlsx workbook.
#' @param sheet Readme sheet name (already added to the workbook).
#' @param pal Resolved MMC palette.
#' @param font_name Font used throughout.
#' @param composition Output of [ck_sample_composition()], or `NULL`.
#' @param sheet_table Data frame with `sheet` and `content`.
#' @param split_by The resolved `split_by` specification.
#' @param hidden_columns Identifier columns hidden on the table sheets.
#' @param hidden_names Names of those columns, used in the notes.
#' @param dropped_groups Group labels left out for having no respondents.
#' @param readme_text Optional character vector of extra lines.
#' @param pct_fmt Excel number format for proportions, from
#'   [ck_percent_format()]. Used both for the sample-composition percentages
#'   and for the number-format note, so the note cannot drift from the sheets.
#' @return Invisibly `NULL`.
#' @keywords internal
ck_write_readme_sheet <- function(
  wb,
  sheet,
  pal,
  font_name,
  composition = NULL,
  sheet_table,
  split_by = "none",
  hidden_columns = integer(0),
  hidden_names = character(0),
  dropped_groups = character(0),
  readme_text = NULL,
  pct_fmt = ck_percent_format(0)
) {
  n_col <- 5L

  # --- Styles ---------------------------------------------------------------
  # Every style on this sheet shares the font and vertical centring; only size,
  # colour, fill, emphasis, alignment, number format and border colour differ.
  sty <- function(...) {
    openxlsx::createStyle(fontName = font_name, valign = "center", ...)
  }
  boxed <- function(colour) {
    list(
      border = "TopBottomLeftRight",
      borderColour = colour,
      borderStyle = "thin"
    )
  }
  white_box <- boxed(pal[["white"]])
  blue_box <- boxed(pal[["light_blue"]])

  s <- list(
    title = sty(
      fontSize = 16,
      fontColour = pal[["white"]],
      textDecoration = "bold",
      fgFill = pal[["navy"]],
      halign = "left",
      indent = 1
    ),
    section = sty(
      fontSize = 12,
      fontColour = pal[["white"]],
      textDecoration = "bold",
      fgFill = pal[["teal"]],
      halign = "left",
      indent = 1
    ),
    label = sty(
      fontSize = 11,
      fontColour = pal[["navy"]],
      textDecoration = "bold",
      fgFill = pal[["pale_blue"]],
      halign = "left",
      border = white_box$border,
      borderColour = white_box$borderColour,
      borderStyle = white_box$borderStyle
    ),
    value = sty(
      fontSize = 11,
      fontColour = pal[["navy"]],
      halign = "left",
      border = blue_box$border,
      borderColour = blue_box$borderColour,
      borderStyle = blue_box$borderStyle
    ),
    head = sty(
      fontSize = 11,
      fontColour = pal[["white"]],
      textDecoration = "bold",
      fgFill = pal[["navy"]],
      halign = "left",
      wrapText = TRUE,
      border = white_box$border,
      borderColour = white_box$borderColour,
      borderStyle = white_box$borderStyle
    ),
    head_num = sty(
      fontSize = 11,
      fontColour = pal[["white"]],
      textDecoration = "bold",
      fgFill = pal[["navy"]],
      halign = "center",
      wrapText = TRUE,
      border = white_box$border,
      borderColour = white_box$borderColour,
      borderStyle = white_box$borderStyle
    ),
    key_cell = sty(
      fontSize = 11,
      fontColour = pal[["navy"]],
      textDecoration = "bold",
      fgFill = pal[["light_blue"]],
      halign = "left",
      border = white_box$border,
      borderColour = white_box$borderColour,
      borderStyle = white_box$borderStyle
    ),
    pct = sty(
      fontSize = 11,
      fontColour = pal[["navy"]],
      halign = "center",
      numFmt = pct_fmt,
      border = blue_box$border,
      borderColour = blue_box$borderColour,
      borderStyle = blue_box$borderStyle
    ),
    count = sty(
      fontSize = 11,
      fontColour = pal[["navy"]],
      halign = "center",
      numFmt = "3",
      border = blue_box$border,
      borderColour = blue_box$borderColour,
      borderStyle = blue_box$borderStyle
    ),
    big_number = sty(
      fontSize = 12,
      fontColour = pal[["navy"]],
      textDecoration = "bold",
      halign = "left",
      numFmt = "3",
      border = blue_box$border,
      borderColour = blue_box$borderColour,
      borderStyle = blue_box$borderStyle
    )
  )

  # --- Local write helpers --------------------------------------------------
  wr <- function(x, at_row, at_col = 1) {
    openxlsx::writeData(
      wb,
      sheet = sheet,
      x = x,
      startRow = at_row,
      startCol = at_col,
      colNames = FALSE,
      rowNames = FALSE
    )
  }
  mg <- function(rows, cols) {
    openxlsx::mergeCells(wb, sheet = sheet, rows = rows, cols = cols)
  }
  st <- function(style, rows, cols) {
    openxlsx::addStyle(
      wb,
      sheet = sheet,
      style = style,
      rows = rows,
      cols = cols,
      gridExpand = TRUE
    )
  }

  openxlsx::setColWidths(
    wb,
    sheet = sheet,
    cols = 1:n_col,
    widths = c(30, 40, 13, 13, 13)
  )

  row <- 2L

  # A full-width coloured band: title or section heading.
  band <- function(text, style, height) {
    wr(text, row)
    mg(row, 1:n_col)
    st(style, row, 1:n_col)
    openxlsx::setRowHeights(wb, sheet = sheet, rows = row, heights = height)
    row <<- row + 1L
  }

  # A two-column block: bold label in column A, text spanning the rest.
  key_value <- function(mat) {
    if (is.null(mat) || nrow(mat) == 0) {
      return(invisible(NULL))
    }
    first <- row
    last <- first + nrow(mat) - 1L

    wr(as.data.frame(mat, stringsAsFactors = FALSE), first)
    for (i in first:last) {
      mg(i, 2:n_col)
    }
    st(s$label, first:last, 1)
    st(s$value, first:last, 2:n_col)

    row <<- last + 2L
  }

  # --- Title -----------------------------------------------------------------
  band("Mixed Migration Centre - 4Mi analysis output", s$title, 32)
  row <- row + 1L

  # --- Sample summary --------------------------------------------------------
  band("Sample summary", s$section, 22)

  if (!is.null(composition)) {
    wr("Sample size", row)
    wr(composition$total, row, 2)
    st(s$label, row, 1)
    st(s$big_number, row, 2:n_col)
    mg(row, 2:n_col)
    row <- row + 2L

    if (!is.null(composition$table) && nrow(composition$table) > 0) {
      wr(t(c("Disaggregation", "Group", "%", "n", "n_total")), row)
      st(s$head, row, 1:2)
      st(s$head_num, row, 3:5)
      row <- row + 1L

      first <- row
      last <- row + nrow(composition$table) - 1L

      wr(
        composition$table[, c(
          "disaggregation",
          "group",
          "share",
          "n",
          "n_total"
        )],
        first
      )
      st(s$label, first:last, 1)
      st(s$key_cell, first:last, 2)
      st(s$pct, first:last, 3)
      st(s$count, first:last, 4:5)

      row <- last + 2L
    }
  } else {
    key_value(rbind(c(
      "Sample size",
      "Not available - the results table carries no count column. Pass total_columns (e.g. c(\"n\", \"n_total\")) to report the sample composition here."
    )))
  }

  # --- Sheet directory -------------------------------------------------------
  band(
    sprintf(
      "Sheets in this workbook   (split by: %s)",
      paste(split_by, collapse = " + ")
    ),
    s$section,
    22
  )

  wr(t(c("Sheet", "Content")), row)
  mg(row, 2:n_col)
  st(s$head, row, 1:n_col)
  row <- row + 1L

  first <- row
  last <- row + nrow(sheet_table) - 1L

  wr(sheet_table[, c("sheet", "content")], first)
  for (i in first:last) {
    mg(i, 2:n_col)
  }
  st(s$key_cell, first:last, 1)
  st(s$value, first:last, 2:n_col)

  row <- last + 2L

  # --- Colour key and notes --------------------------------------------------
  band("Colour key", s$section, 22)
  key_value(rbind(
    c("Navy #003D58", "Disaggregation group band and table outline."),
    c("Teal #00A2A5", "Statistic header band and colour scale maximum."),
    c(
      "Light blue #AFDFE4 / pale blue #D5EEF0",
      "Question and option index panel on the left."
    ),
    c("White / #F4FAFB", "Alternating shading between statistic blocks.")
  ))

  band("Notes", s$section, 22)

  notes <- rbind(
    c(
      "Number formats",
      paste0(
        "Proportions as percentages to ",
        if (identical(pct_fmt, "0%")) {
          "the nearest whole number"
        } else {
          paste0(nchar(pct_fmt) - 3L, " decimal place(s)")
        },
        ", means and medians to two decimals, counts as whole numbers. Cells ",
        "keep their full precision; only the display is rounded."
      )
    ),
    c(
      "Reading the table",
      "Row 1 is the disaggregation group, row 2 the statistic. Panes are frozen below the headers."
    )
  )

  if (length(hidden_columns) > 0) {
    notes <- rbind(
      notes,
      c(
        "Hidden columns",
        sprintf(
          "The first %s column%s of each table sheet %s hidden (%s). They are kept in the file for traceability - select the columns on either side, right click and choose Unhide to see them.",
          length(hidden_columns),
          if (length(hidden_columns) == 1) "" else "s",
          if (length(hidden_columns) == 1) "is" else "are",
          if (length(hidden_names) > 0) {
            paste(hidden_names, collapse = ", ")
          } else {
            "columns A to C"
          }
        )
      )
    )
  }

  notes <- rbind(
    notes,
    c(
      "Sample summary",
      "Group sizes are the largest denominator recorded for each group across all analyses; the share is that number over the whole sample."
    )
  )

  if (length(dropped_groups) > 0) {
    notes <- rbind(
      notes,
      c(
        "Groups left out",
        sprintf(
          "%s group%s omitted from the table sheets for having no respondents (%s)%s. They are absent from the columns, not shown as zero - do not read the remaining groups as the full set.",
          length(dropped_groups),
          if (length(dropped_groups) == 1) " was" else "s were",
          paste(utils::head(dropped_groups, 25), collapse = ", "),
          if (length(dropped_groups) > 25) {
            sprintf(" and %s more", length(dropped_groups) - 25)
          } else {
            ""
          }
        )
      )
    )
  }

  if (!is.null(readme_text)) {
    notes <- rbind(
      notes,
      cbind(rep("", length(readme_text)), as.character(readme_text))
    )
  }

  key_value(notes)

  invisible(NULL)
}
