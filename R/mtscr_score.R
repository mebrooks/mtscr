#' Score creativity with MTS
#'
#' @inheritParams mtscr_prepare
#' @inheritParams mtscr_model
#' @param summarise_for Character, whether to get creativity scores for each person, each item or both.
#'     Can be `"person"`, `"item"` or `"both"`.
#' @param append Logical, whether to add the scores columnd to the original data frame ('TRUE') or
#'     return a new data frame with only the scores ('FALSE'). Additional columns depend on `summarise_for`.
#'
#' @return A data frame with creativity scores. If `summarise_for` is `"person"`, the data frame
#'     will have one row per person. If `summarise_for` is `"item"`, the data frame will have one
#'     row per item. If `summarise_for` is `"both"`, the data frame will have one row per person-item.
#'     If `append` is `TRUE`, the original data frame will be returned with the scores columns added.
#'     By default (`append = FALSE`), only the scores columns and id and/or item columns
#'     (depending on `summarise_for` value) will be returned.
#'
#' @export
#'
#' @examples
#' data("mtscr_creativity", package = "mtscr")
#' mtscr_score(mtscr_creativity, id, item, SemDis_MEAN)
#' mtscr_score(mtscr_creativity, id, item, SemDis_MEAN, summarise_for = "both") # one score for person-item
mtscr_score <- function(df, id_column, item_column, score_column, model_type = c("all_max", "all_top2"), summarise_for = "person", append = FALSE) {
    id_column <- rlang::ensym(id_column)
    item_column <- rlang::ensym(item_column)
    score_column <- rlang::ensym(score_column)
    df_original <- df

    if (!all(model_type %in% c("all_max", "all_top2"))) {
        cli::cli_abort(
            c(
                "{.arg model_type} must be a subset of {c('all_max', 'all_top2')}.",
                "x" = "{.var {setdiff(model_type, c('all_max', 'all_top2'))[[1]]}} is not a valid model type."
            )
        )
    }

    # prepare
    df <- mtscr_prepare(df, !!id_column, !!item_column, !!score_column, minimal = FALSE)
    model <- mtscr_model(df, !!id_column, !!item_column, !!score_column, model_type = model_type, prepared = TRUE)

    # score
    if (length(model_type) > 1) {
        df$.all_max <- predict(model[["all_max"]], df)
        df$.all_top2 <- predict(model[["all_top2"]], df)
    } else if (model_type == "all_max") {
        df$.all_max <- predict(model, df)
    } else if (model_type == "all_top2") {
        df$.all_top2 <- predict(model, df)
    }

    df <- dplyr::select(df, -.z_score, -.ordering, -.ordering_0, -.ordering_top2_0, -.max_ind, -.top2_ind)

    # summarise
    if (summarise_for == "person") {
        groups <- rlang::as_name(id_column)
    } else if (summarise_for == "item") {
        groups <- rlang::as_name(item_column)
    } else if (summarise_for == "both") {
        groups <- c(rlang::as_name(id_column), rlang::as_name(item_column))
    } else {
        cli::cli_abort(
            c(
                "{.arg summarise_for} must be a subset of {c('person', 'item', 'both')}.",
                "x" = "{.var summarise_for} is not a valid value."
            )
        )
    }

    args <- list()
    if ("all_max" %in% model_type) {
        args[[".all_max"]] <- rlang::expr(max(.all_max))
    }
    if ("all_top2" %in% model_type) {
        args[[".all_top2"]] <- rlang::expr(max(.all_top2))
    }

    df <- df |>
        dplyr::summarise(
            !!!args,
            .by = dplyr::all_of(groups)
        )

    # append
    if (append) {
        df <- dplyr::left_join(df_original, df, by = groups)
    }

    return(df)
}
