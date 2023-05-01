#' Create MTS model
#'
#' Create MTS model for creativity analysis.
#'
#' @inheritParams mtscr_prepare
#' @param prepared Logical, is the data already prepared with `mtscr_prepare()`?
#'
#' @return A `glmmTMB` model.
#'
#' @export
#'
#' @examples
#' data("mtscr_creativity", package = "mtscr")
#' mtscr_model(mtscr_creativity, id, item, SemDis_MEAN) |>
#'     broom.mixed::tidy() # tidy print, can use `summary()`
mtscr_model <- function(df, id_column, item_column, score_column, prepared = FALSE) {
    id_column <- rlang::ensym(id_column)
    item_column <- rlang::ensym(item_column)
    score_column <- rlang::ensym(score_column)

    # prepare
    if (!prepared) {
        df <- mtscr_prepare(df, !!id_column, !!item_column, !!score_column, minimal = TRUE)
    }
    
    df <- df |>
        dplyr::mutate(
            .ordering_0 = .ordering - 1
        )

    # create formula
    formula <- as.formula(paste0(
        ".z_score ~ -1 + ",
        rlang::as_name(item_column),
        " + ",
        rlang::as_name(item_column),
        ":.ordering_0 + (.ordering_0|",
        rlang::as_name(id_column),
        ")"
    ))

    # model
    glmmTMB::glmmTMB(
        formula,
        data = df,
        family = gaussian()
    )
}
