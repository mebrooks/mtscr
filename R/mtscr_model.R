#' Create MTS model
#'
#' Create MTS model for creativity analysis.
#'
#' @inheritParams mtscr_prepare
#' @param model_type Character vector, the type of model to fit. Can be `all_max`, `all_top2` or both.
#' @param prepared Logical, is the data already prepared with `mtscr_prepare()`?
#'
#' @return The return value depends on `model_type`; if `model_type` is a single value,
#'     the function returns a `glmmTMB` model object. If `model_type` is a vector of values,
#'     the function returns a named list of `glmmTMB` model objects.
#'
#' @export
#'
#' @examples
#' data("mtscr_creativity", package = "mtscr")
#'
#' mtscr_model(mtscr_creativity, id, item, SemDis_MEAN, model_type = "all_max") |>
#'     broom.mixed::tidy() # tidy print, can use `summary()`
#'
#' mtscr_model(mtscr_creativity, id, item, SemDis_MEAN, model_type = c("all_max", "all_top2")) |>
#'     lapply(broom.mixed::tidy)
#'
#' # you can prepare data first
#' data <- mtscr_prepare(mtscr_creativity, id, item, SemDis_MEAN)
#' mtscr_model(data, id, item, SemDis_MEAN, model_type = "all_max", prepared = TRUE)
#'
#' # predict with model
#' data <- mtscr_prepare(mtscr_creativity, id, item, SemDis_MEAN, minimal = TRUE)
#' model <- mtscr_model(data, id, item, SemDis_MEAN, prepared = TRUE)
#' data$creativity_score <- predict(model[["all_max"]], data)
mtscr_model <- function(df, id_column, item_column, score_column, model_type = c("all_max", "all_top2"), prepared = FALSE) {
    id_column <- rlang::ensym(id_column)
    item_column <- rlang::ensym(item_column)
    score_column <- rlang::ensym(score_column)

    # check if model_type is valid
    if (!all(model_type %in% c("all_max", "all_top2"))) {
        cli::cli_abort(
            c(
                "{.arg model_type} must be a subset of {c('all_max', 'all_top2')}.",
                "x" = "{.var {setdiff(model_type, c('all_max', 'all_top2'))[[1]]}} is not a valid model type."
            )
        )
    }

    # prepare
    if (!prepared) {
        df <- mtscr_prepare(df, !!id_column, !!item_column, !!score_column, minimal = TRUE)
    }

    # create formulas
    formulas <- vector("list", length(model_type))
    i <- 1
    if ("all_max" %in% model_type) {
        formulas[[i]] <- as.formula(
            paste0(
                ".z_score ~ -1 + ",
                rlang::as_name(item_column),
                " + ",
                rlang::as_name(item_column),
                ":.ordering_0 + (.ordering_0 | ",
                rlang::as_name(id_column),
                ")"
            )
        )
        names(formulas)[i] <- "all_max"
        i <- i + 1
    }

    if ("all_top2" %in% model_type) {
        formulas[[i]] <- as.formula(
            paste0(
                ".z_score ~ -1 + ",
                rlang::as_name(item_column),
                " + ",
                rlang::as_name(item_column),
                ":.ordering_top2_0 + (.ordering_top2_0 | ",
                rlang::as_name(id_column),
                ")"
            )
        )
        names(formulas)[i] <- "all_top2"
        i <- i + 1
    }

    # models
    models <- lapply(formulas, function(formula) {
        glmmTMB::glmmTMB(
            formula,
            data = df,
            family = gaussian()
        )
    })

    if (length(model_type) == 1) {
        return(models[[1]])
    } else {
        return(models)
    }
}
