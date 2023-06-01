#' Prepare database for MTS
#'
#' Prepare database for MTS analysis.
#'
#' @param df Data frame in long format.
#' @param id_column Name of the column containing participants' id.
#' @param item_column Name of the column containing distinct trials (e.g. names of items in AUT).
#' @param score_column Name of the column containing divergent thinking scores
#'     (e.g. semantic distance).
#' @param top Integer or vector of integers (see examples), number of top answers
#'     to prepare indicators for. Default is 1, i.e. only the top answer.
#' @param minimal Logical, append columns to df (`FALSE`) or return only `id`, `item`,
#'     and the new columns (`TRUE`).
#' @param ties_method Character string specifying how ties are treated when
#'     ordering. Can be `"average"` (better for continous scores like semantic
#'     distance) or `"random"` (default, better for ratings). See [rank()] for details.
#'
#' @return The input data frame with additional columns:
#'     \describe{
#'         \item{`.z_score`}{Numerical, z-score of the creativity score}
#'         \item{`.ordering`}{Numerical, ranking of the answer relative to participant and item}
#'         \item{`.ordering_topX`}{Numerical, 0 for *X* top answers, otherwise value of `.ordering`}
#'     }
#'     Number of `.ordering_topX` columns depends on the `top` argument. If `minimal = TRUE`,
#'     only the new columns and the item and id columns are returned. The values are
#'     relative to the participant AND item, so the values for different
#'     participants scored for different tasks (e.g. uses for "brick" and "can") are distinct.
#' @export
#'
#' @examples
#' data("mtscr_creativity", package = "mtscr")
#' # Indicators for top 1 and top 2 answers
#' mtscr_prepare(mtscr_creativity, id, item, SemDis_MEAN, top = 1:2, minimal = TRUE)
mtscr_prepare <- function(df, id_column, item_column, score_column, top = 1, minimal = FALSE, ties_method = "random") {
  # ensym to make both strings and symbols work
  id_column <- rlang::ensym(id_column)
  item_column <- rlang::ensym(item_column)
  score_column <- rlang::ensym(score_column)

  # check if df is a data frame
  if (!is.data.frame(df)) {
    cli::cli_abort(
      c(
        "{.arg df} must be a data frame.",
        "x" = "{.var {rlang::expr_text(substitute(df))}} is {.obj_type_friendly {df}}"
      )
    )
  }

  # check if columns exist
  if (!rlang::has_name(df, rlang::as_name(id_column))) {
    cli::cli_abort(
      c(
        "All columns must exist in the data.",
        "x" = "Column {.var {id_column}} does not exist.",
        "i" = "Check the spelling."
      )
    )
  }
  if (!rlang::has_name(df, rlang::as_name(item_column))) {
    cli::cli_abort(
      c(
        "All columns must exist in the data.",
        "x" = "Column {.var {item_column}} does not exist.",
        "i" = "Check the spelling."
      )
    )
  }
  if (!rlang::has_name(df, rlang::as_name(score_column))) {
    cli::cli_abort(
      c(
        "All columns must exist in the data.",
        "x" = "Column {.var {score_column}} does not exist.",
        "i" = "Check the spelling."
      )
    )
  }

  # check if score_column is numeric
  if (!is.numeric(df[[rlang::as_name(score_column)]])) {
    cli::cli_abort(
      c(
        "{.var score_column} must be numeric.",
        "x" = "{.var {rlang::expr_text(substitute(score_column))}} is {.cls {class(df[[rlang::as_name(score_column)]])}}"
      )
    )
  }

  # check if minimal is logical
  if (!is.logical(minimal)) {
    cli::cli_abort(
      c(
        "{.arg minimal} must be logical.",
        "x" = "{.var {rlang::expr_text(substitute(minimal))}} is {.obj_type_friendly {minimal}}"
      )
    )
  }

  # check if top is numeric
  if (!is.numeric(top)) {
    cli::cli_abort(
      c(
        "{.arg top} must be an integer or a vector of integers.",
        "x" = "{.var {rlang::expr_text(substitute(top))}} is {.cls {class(top)}}"
      )
    )
  }

  # check if top is an integer or a vector of integers
  if (!any(top == as.integer(top))) {
    cli::cli_abort(
      c(
        "{.arg top} must be an integer or a vector of integers.",
        "x" = "{.var {rlang::expr_text(substitute(top))}} is not an integer."
      )
    )
  }

  # check if top contains only positive values
  if (any(top < 1)) {
    cli::cli_abort(
      c(
        "{.arg top} must be a positive integer or a vector of positive integers.",
        "x" = "{.var {rlang::expr_text(substitute(top))}} contains non-positive integers."
      )
    )
  }

  if (dplyr::is.grouped_df(df)) {
    cli::cli_inform(
      c(
        "Data must not be grouped.",
        "i" = "It has been ungrouped."
      )
    )
    df <- dplyr::ungroup(df)
  }

  # Remove NA scores if present
  if (any(is.na(df[[rlang::as_name(score_column)]]))) {
    cli::cli_inform(
      c(
        "Removed NA values from {.var {rlang::expr_text(substitute(score_column))}}"
      )
    )
    df <- dplyr::filter(df, !is.na(!!score_column))
  }

  df <- df |>
    dplyr::mutate(df,
      .z_score = as.vector(scale({{ score_column }}))
    ) |>
    dplyr::group_by({{ id_column }}, {{ item_column }}) |>
    dplyr::arrange({{ id_column }}, {{ item_column }}, dplyr::desc(.data$.z_score)) |>
    dplyr::mutate(
      .ordering = rank(
        -.data$.z_score, # minus for descending order
        ties.method = ties_method
      ) - 1 # -1 to start with 0
    )

  top <- as.list(top)

  df <- purrr::map(top, \(x) {
    df |>
      dplyr::mutate(
        !!glue(".ordering_top{x}") := dplyr::case_when(
          .ordering < x ~ 0,
          .default = .data$.ordering
        )
      )
  }) |>
    Reduce(dplyr::full_join, x = _) |>
    suppressMessages() |> # suppress info about joining by
    dplyr::ungroup()

  if (minimal) {
    df <- df |>
      dplyr::select({{ id_column }}, {{ item_column }}, ".z_score", dplyr::starts_with(".ordering"))
  }

  return(df)
}
