#' Score creativity with MTS
#'
#' @inheritParams mtscr_prepare
#' @inheritParams mtscr_model
#' @param format Character, controls the format of the output data frame. Accepts:
#'     \describe{
#'         \item{`"minimal"`}{default, returns only the creativity scores and id columns.}
#'         \item{`"full"`}{returns the original data frame with creativity scores columns added.}
#'     }
#' @return
#' A tibble with creativity scores. If `format = "full"`, the original data frame is
#' returned with scores columns added. Otherwise, only the scores and id columns are returned.
#' number of creativity scores columns (e.g. `creativity_score_top2`) depends on the `top` argument.
#'
#' @seealso [tidyr::pivot_wider()] for converting the output to wide format by yourself.
#'
#' @export
#'
#' @examples
#' data("mtscr_creativity", package = "mtscr")
#' mtscr_score(mtscr_creativity, id, item, SemDis_MEAN, top = 1:2)
#'
#' # add scores to the original data frame
#' mtscr_score(mtscr_creativity, id, item, SemDis_MEAN, format = "full")
mtscr_score <- function(df, id_column, item_column = NULL, score_column, top = 1, format = "minimal", ties_method = "random") {
  id_column <- rlang::ensym(id_column)
  item_column_quo <- enquo(item_column)
  if (!rlang::quo_is_null(item_column_quo)) {
    item_column <- rlang::ensym(item_column)
  } else {
    item_column <- item_column_quo
  }
  score_column <- rlang::ensym(score_column)
  df_original <- df

  # check that format is either "minimal" or "full"
  if (!format %in% c("minimal", "full")) {
    cli::cli_abort(
      c(
        "{.arg format} must be either \"minimal\" or \"full\".",
        "x" = "{.var {rlang::expr_text(substitute(format))}} is invalid."
      )
    )
  }

  # prepare
  df <- mtscr_prepare(df, !!id_column, !!item_column, !!score_column, top = top, minimal = FALSE, ties_method = ties_method)
  model <- mtscr_model(df, !!id_column, !!item_column, !!score_column, top = top, prepared = TRUE, ties_method = ties_method)

  if (length(top) == 1) {
    model <- list(model)
  }

  # score
  df <- purrr::map2(
    model,
    top,
    \(current_model, top_number) {
      col_name <- paste0(".creativity_score_top", top_number)

      glmmTMB::ranef(current_model)$cond$id |>
        dplyr::as_tibble(rownames = "id") |>
        dplyr::select("id", !!col_name := "(Intercept)")
    }
  ) |>
    Reduce(dplyr::full_join, x = _) |>
    suppressMessages() # for full_join column names message

  # append
  if (format == "full") {
    # if id_column is numeric, convert it back to numeric
    # otherwise join will fail
    if (is.numeric(df_original[[rlang::as_name(id_column)]])) {
      df <- df |>
        dplyr::mutate(!!rlang::as_name(id_column) := readr::parse_number(!!id_column))
    }
    df <- dplyr::left_join(df_original, df, by = rlang::as_name(id_column))
  }

  return(df)
}
