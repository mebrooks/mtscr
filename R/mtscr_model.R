#' Create MTS model
#' 
#' Create MTS model for creativity analysis.
#' 
#' @inheritParams mtscr_prepare
mtscr_model <- function(df, .id_column, .item_column, .value_column) {
    .id_column <- rlang::enquo(.id_column)
    .item_column <- rlang::enquo(.item_column)
    .value_column <- rlang::enquo(.value_column)

    # prepare if not prepared
    if (!all(c(".max_ind", ".top2_ind", ".ordering") %in% names(df))) {
        df <- mtscr_prepare(df, .id_column, .item_column, .value_column)
    }

    # create formula


    # create model


    # return model
    return(model)
}