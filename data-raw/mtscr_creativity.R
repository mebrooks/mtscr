mtscr_creativity <- readr::read_csv("data-raw/study2.csv") |>
  dplyr::select(-response_nofill, -item_nofill)

usethis::use_data(mtscr_creativity, overwrite = TRUE)
