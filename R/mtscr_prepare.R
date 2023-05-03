#' Prepare database for MTS
#'
#' Prepare database for MTS analysis.
#'
#' @param df Data frame in long format
#' @param id_column Name of the column containing participants' id
#' @param item_column Name of the column containing distinct trials (e.g. names of items in AUT)
#' @param score_column Name of the column containing divergent thinking scores (e.g. semantic distance)
#' @param minimal Logical, append columns to df (`FALSE`) or return only `id`, `item`, and the new columns (`TRUE`)
#'
#' @return The input data frame with additional columns:
#'     \describe{
#'         \item{`.data$.z_score`}{Numerical, z-score of the creativity score}
#'         \item{`.data$.ordering`}{Numerical, ranking of the answer relative to participant and item}
#'         \item{`.data$.ordering_0`}{Numerical, 0 for the best answer}
#'         \item{`.data$.ordering_top2_0`}{Numerical, 0 for the two best answers}
#'         \item{`.max_ind`}{Numerical, 0 for the best answer, 1 otherwise}
#'         \item{`.top2_ind`}{Numerical, 0 for the two best answers, 1 otherwise}
#'     }
#'     The values are relative to the participant AND item, so the values for different participants
#'     scored for different tasks (e.g. uses for "brick" and "can") are distinct.
#' @export
#'
#' @examples
#' data("mtscr_creativity", package = "mtscr")
#' mtscr_prepare(mtscr_creativity, id, item, SemDis_MEAN, minimal = TRUE)
mtscr_prepare <- function(df, id_column, item_column, score_column, minimal = FALSE) {
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
        "{.var {score_column}} must be numeric.",
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

  df <- df |>
    dplyr::mutate(df,
      .z_score = as.vector(scale({{ score_column }}))
    ) |>
    dplyr::group_by({{ id_column }}, {{ item_column }}) |>
    dplyr::arrange({{ id_column }}, {{ item_column }}, dplyr::desc(.data$.z_score)) |>
    dplyr::mutate(
      .ordering = rank(-.data$.z_score), # minus for descending order
      .ordering_0 = .data$.ordering - 1,
      .ordering_top2_0 = dplyr::case_when(
        .data$.ordering_0 %in% 0:1 ~ 0,
        .default = .data$.ordering_0
      ),
      .max_ind = dplyr::case_when(
        .data$.ordering == 1 ~ 0, # 0 if the best answer
        .default = 1 # 1 otherwise
      ),
      .top2_ind = dplyr::case_when(
        .data$.ordering %in% 1:2 ~ 0, # 0 if the two best answers
        .default = 1 # 1 otherwise
      )
    ) |>
    dplyr::ungroup()

  if (minimal) {
    df <- dplyr::select(
      df,
      {{ id_column }},
      {{ item_column }},
      ".z_score",
      ".ordering",
      ".ordering_0",
      ".ordering_top2_0",
      ".max_ind",
      ".top2_ind"
    )
  }

  return(df)
}
