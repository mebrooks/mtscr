#' Score creativity with MTS
#'
#' @inheritParams mtscr_prepare
#' @inheritParams mtscr_model
#'
#' @return A data frame with the creativity scores in the columns `.all_max` and `.all_top2`.
#'
#' @export
#'
#' @examples
#' data("mtscr_creativity", package = "mtscr")
#' mtscr_score(mtscr_creativity, id, item, SemDis_MEAN)
mtscr_score <- function(df, id_column, item_column, score_column, model_type = c("all_max", "all_top2")) {
    id_column <- rlang::ensym(id_column)
    item_column <- rlang::ensym(item_column)
    score_column <- rlang::ensym(score_column)

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
    if (!is.list(model)) {
        model <- list(model_type = model) # make it a named list for compatibility when predicting
    }

    # score
    if ("all_max" %in% model_type) {
        df$.all_max <- predict(model[["all_max"]], df)
    }
    if ("all_top2" %in% model_type) {
        df$.all_top2 <- predict(model[["all_top2"]], df)
    }
    df <- dplyr::select(df, -.z_score, -.ordering, -.ordering_0, -.ordering_top2_0, -.max_ind, -.top2_ind)

    return(df)
}
