#' Prepare database for MTS
#'
#' @param df Data frame in long format
#' @param .id_column Name of the column containing participants' id
#' @param .item_column Name of the column containing distinct trials (e.g. names of items in AUT)
#' @param .value_column Name of the column containing divergent thinking scores (e.g. semantic distance)
#' @param preserve_existing Logical, return the whole database with additional columns (`TRUE`) or only the additional columns (`FALSE`)
#'
#' @return The input data frame with additional columns:
#'     \describe{
#'         \item{`max_ind`}{Numerical, 0 for the best answer}
#'         \item{`top2_ind`}{Numerical, 0 for the two best answers}
#'         \item{`rank`}{Numerical, ranking of the answer relative to participant and item}
#'     }
#'     The values are relative to the participant AND item, so the values for different participants scored for different tasks (e.g. uses for "brick" and "can") are distinct.
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' data("mtscr_creativity", package = "mtscr")
#' mtscr_prepare(mtscr_creativity, id, item, SemDis_MEAN) # columns' names can also be "quoted"
mtscr_prepare <- function(df, .id_column, .item_column, .value_column, preserve_existing = TRUE) {
  .id_column <- rlang::enquo(.id_column)
  .item_column <- rlang::enquo(.item_column)
  .value_column <- rlang::enquo(.value_column)

  # check if .value_column contains NA
  if (any(is.na(df[[rlang::as_name(.value_column)]]))) {
    warning("The value column contains NA values. Rows with NA creativity scores have been deleted.")
    df <- dplyr::filter(df, any(is.na(.data[[.value_column]])))
  }

  # check if .value_column is numeric
  if (!is.numeric(df[[rlang::as_name(.value_column)]])) {
    stop(".value_column must be numeric.")
  }

  # check if df is groupped and ungroup if so
  if (dplyr::is_grouped_df(df)) {
    warning("The data frame has been ungroupped.")
    df <- dplyr::ungroup(df)
  }

  df <- df |>
    dplyr::mutate(
      .z_score = as.vector(scale(.data[[.value_column]]))
    ) |>
    dplyr::group_by({{ .id_column }}, {{ .item_column }}) |>
    dplyr::arrange(dplyr::desc(.data$.z_score)) |>
    dplyr::mutate(
      .ordering = rank(-.data$.z_score),
      .max_ind = dplyr::case_match(
        .data$ordering,
        1 ~ 0,
        .default = 1
      ),
      .top2_ind = dplyr::case_match(
        .data$ordering,
        1:2 ~ 0,
        .default = 1
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange({{ .id_column }}, {{ .item_column }}) |>
    dplyr::relocate(.data$.ordering, .after = .data$.top2_ind)

  if (!preserve_existing) {
    df <- dplyr::select(df, .data$.max_ind, .data$.top2_ind, .data$.ordering)
  }

  return(df)
}
