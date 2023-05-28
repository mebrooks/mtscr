#' Create MTS model
#'
#' Create MTS model for creativity analysis.
#'
#' @inheritParams mtscr_prepare
#' @param top Integer or vector of integers (see examples), number of top answers
#'     to include in the model. Default is 1, i.e. only the top answer.
#' @param prepared Logical, is the data already prepared with `mtscr_prepare()`?
#'
#' @return The return value depends on length of the `top` argument. If `top` is a single
#'     integer, a `glmmTMB` model is returned. If `top` is a vector of integers, a list
#'     of `glmmTMB` models is returned, with names corresponding to the `top` values,
#'     e.g. `top1`, `top2`, etc.
#'
#' @export
#'
#' @examples
#' data("mtscr_creativity", package = "mtscr")
#'
#' mtscr_model(mtscr_creativity, id, item, SemDis_MEAN) |>
#'   broom.mixed::tidy() # tidy print, can use `summary()`
#'
#' # three models for top 1, 2, and 3 answers
#' mtscr_model(mtscr_creativity, id, item, SemDis_MEAN, top = 1:3) |>
#'   lapply(broom.mixed::tidy)
#'
#' # you can prepare data first
#' data <- mtscr_prepare(mtscr_creativity, id, item, SemDis_MEAN)
#' mtscr_model(data, id, item, SemDis_MEAN, prepared = TRUE)
#'
#' # extract effects for creativity score by hand
#' model <- mtscr_model(mtscr_creativity, id, item, SemDis_MEAN, top = 2)
#' creativity_score <- glmmTMB::ranef(model)$cond$id[, 1]
mtscr_model <- function(df, id_column, item_column, score_column, top = 1, prepared = FALSE) {
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

  # check if all .ordering_X columns exist
  ordering_columns <- purrr::map(
    as.list(top),
    \(x) {
      paste0(".ordering_top", x)
    }
  )
  if (prepared && !any(ordering_columns %in% names(df))) {
    cli::cli_warn(
      c(
        "Couldn't find all {.var .ordering_top} columns.",
        "i" = "The dataframe was prepared again."
      )
    )
    prepared <- FALSE
  }

  # prepare
  if (!prepared) {
    df <- mtscr_prepare(df, !!id_column, !!item_column, !!score_column, top = top, minimal = TRUE)
  }

  # create formulas
  # formula example: .z_score ~ -1 + item + item:.ordering_topX + (.ordering_X | id)
  formulas <- purrr::map_vec(
    ordering_columns,
    \(x) {
      c(
        stats::as.formula(
          paste0(
            ".z_score ~ -1 + ",
            rlang::as_name(item_column),
            " + ",
            rlang::as_name(item_column),
            ":",
            x,
            " + (",
            x,
            " | ",
            rlang::as_name(id_column),
            ")"
          )
        )
      )
    }
  )

  # models
  models <- purrr::map(formulas, function(formula) {
    glmmTMB::glmmTMB(
      formula,
      data = df,
      family = stats::gaussian()
    )
  })

  if (length(top) == 1) {
    return(models[[1]])
  } else {
    names(models) <- paste0("top", top)
    return(models)
  }
}
