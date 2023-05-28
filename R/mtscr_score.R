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
mtscr_score <- function(df, id_column, item_column, score_column, top = 1, format = "minimal") {
  id_column <- rlang::ensym(id_column)
  item_column <- rlang::ensym(item_column)
  score_column <- rlang::ensym(score_column)
  df_original <- df

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

  # check if top is an integer or a vector of integers
  if (!identical(top, as.integer(top))) {
    cli::cli_abort(
      c(
        "{.arg top} must be an integer or a vector of integers.",
        "x" = "{.var {rlang::expr_text(substitute(top))}} is {.obj_type_friendly {top}}"
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

  # prepare
  df <- mtscr_prepare(df, !!id_column, !!item_column, !!score_column, top = top, minimal = FALSE)
  model <- mtscr_model(df, !!id_column, !!item_column, !!score_column, top = top, prepared = TRUE)

  # score
  df <- purrr::map2(
    model,
    top,
    \(model, top_number) {
      col_name <- paste0(".creativity_score_top", top_number)

      glmmTMB::ranef(model)$cond$id |>
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
