#' @import readr

mtscr_creativity <- readr::read_csv("data-raw/study2.csv") |>
  dplyr::select(-response_nofill, -item_nofill) |>
  dplyr::filter(
    item %in% c(
      "belt", "brick", "broom",
      "bucket", "candle", "clock",
      "comb", "knife", "lamp",
      "pencil", "pillow",
      "purse", "sock"
    ),
    any(is.na(SemDis_MEAN))
  )

usethis::use_data(mtscr_creativity, overwrite = TRUE)
