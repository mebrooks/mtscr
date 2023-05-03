#' Score creativity with MTS
#'
#' @inheritParams mtscr_prepare
#' @inheritParams mtscr_model
#' @param summarise_for Character, whether to get creativity scores for each person, each item or both.
#'     Can be `"person"`, `"item"` or `"both"`.
#' @param format Character, controls the format of the output data frame. Accepts:
#'     \describe{
#'         \item{`"minimal_long"`}{default, returns only the columns important for scoring
#'             (usually ID, item and scores) in long format.}
#'         \item{`"minimal_wide"`}{returns only the columns important for scoring
#'             (usually ID, item and scores) in wide format. Works only if `summarise_for` is `"both"`.}
#'         \item{`"full"`}{returns the original data frame with scores columns added.}
#'     }
#' @return
#' A tibble with creativity scores. If `summarise_for` is `"person"`, the data frame
#' will have one row per person. If `summarise_for` is `"item"`, the data frame will have one
#' row per item. If `summarise_for` is `"both"`, the format depends on the `format` argument.
#' If it is `"minimal_long"`, the data frame will have one row per person-item combination.
#' If it is `"minimal_wide"`, the data frame will have one row per person and one column per item.
#'
#' **Important:** if format is `"full"`, the data frame will always be in a long format. This means
#' that even though one row in the input data frame represents one creative idea, the score
#' in the added columns reflects a collective score for that person, item or person-item combination.
#' For example if Kowalski has 3 ideas about the use of a brick, all the rows with the brick ideas
#' will have the same score in the added columns – the score for Kowalski's brick ideas. If
#' `summarise_for` is `"person"`, the score will be the same for all Kowalski's ideas.
#'
#' @seealso [tidyr::pivot_wider()] for converting the output to wide format by yourself.
#'
#' @export
#'
#' @examples
#' data("mtscr_creativity", package = "mtscr")
#' mtscr_score(mtscr_creativity, id, item, SemDis_MEAN)
#'
#' # one score for person-item
#' mtscr_score(mtscr_creativity, id, item, SemDis_MEAN, summarise_for = "both")
mtscr_score <- function(df, id_column, item_column, score_column, model_type = c("all_max", "all_top2"), summarise_for = "person", format = "minimal_long") {
  id_column <- rlang::ensym(id_column)
  item_column <- rlang::ensym(item_column)
  score_column <- rlang::ensym(score_column)
  df_original <- df

  # check if model_type is valid
  if (!all(model_type %in% c("all_max", "all_top2"))) {
    cli::cli_abort(
      c(
        "{.arg model_type} must be a subset of {c('all_max', 'all_top2')}.",
        "x" = "{.var {setdiff(model_type, c('all_max', 'all_top2'))[[1]]}} is not a valid model type."
      )
    )
  }

  # check if summarise_for is of length 1
  if (length(summarise_for) > 1) {
    cli::cli_abort(
      c(
        "{.arg summarise_for} must be of length 1.",
        "x" = "{.var {summarise_for}} is of length {length(summarise_for)}."
      )
    )
  }

  # check if summarise_for is valid
  if (!summarise_for %in% c("person", "item", "both")) {
    cli::cli_abort(
      c(
        "{.arg summarise_for} must be a subset of {c('person', 'item', 'both')}.",
        "x" = "{.var {summarise_for}} is not a valid value."
      )
    )
  }

  # check if format is of length 1
  if (length(format) > 1) {
    cli::cli_abort(
      c(
        "{.arg format} must be of length 1.",
        "x" = "{.var {format}} is of length {length(format)}."
      )
    )
  }

  # check if format is valid
  if (!format %in% c("minimal_long", "minimal_wide", "full")) {
    cli::cli_abort(
      c(
        "{.arg format} must be a subset of {c('minimal_long', 'minimal_wide', 'full')}.",
        "x" = "{.var {format}} is not a valid value."
      )
    )
  }

  # message if format is minimal_wide and summarise_for is not both
  if (format == "minimal_wide" && summarise_for != "both") {
    cli::cli_inform(
      c(
        "{.arg \"minimal_wide\"} works only if {.arg summarise_for = \"both\"}.",
        "i" = "{.arg \"mininal_wide\"} will have no effect."
      )
    )
  }

  # prepare
  df <- mtscr_prepare(df, !!id_column, !!item_column, !!score_column, minimal = FALSE)
  model <- mtscr_model(df, !!id_column, !!item_column, !!score_column, model_type = model_type, prepared = TRUE)

  # score
  if (length(model_type) > 1) {
    df$.all_max <- stats::predict(model[["all_max"]], df)
    df$.all_top2 <- stats::predict(model[["all_top2"]], df)
  } else if (model_type == "all_max") {
    df$.all_max <- stats::predict(model, df)
  } else if (model_type == "all_top2") {
    df$.all_top2 <- stats::predict(model, df)
  }

  df <- df |>
    dplyr::select(-".z_score", -".ordering", -".ordering_0", -".ordering_top2_0", -".max_ind", -".top2_ind")

  # summarise
  if (summarise_for == "person") {
    groups <- rlang::as_name(id_column)
  } else if (summarise_for == "item") {
    groups <- rlang::as_name(item_column)
  } else if (summarise_for == "both") {
    groups <- c(rlang::as_name(id_column), rlang::as_name(item_column))
  }

  args <- list()
  if ("all_max" %in% model_type) {
    args[[".all_max"]] <- rlang::parse_expr("max(.all_max)")
  }
  if ("all_top2" %in% model_type) {
    args[[".all_top2"]] <- rlang::parse_expr("max(.all_top2)")
  }

  df <- df |>
    dplyr::summarise(
      !!!args,
      .by = dplyr::all_of(groups)
    )

  # wide format
  if (format == "minimal_wide" && summarise_for == "both") {
    df <- df |>
      tidyr::pivot_wider(
        names_from = rlang::as_name(item_column),
        values_from = sapply(model_type, \(x) paste0(".", x), USE.NAMES = FALSE)
      )
  }

  # append
  if (format == "full") {
    df <- dplyr::left_join(df_original, df, by = groups)
  }

  return(df)
}
