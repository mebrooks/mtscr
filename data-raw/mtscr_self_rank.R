mtscr_self_rank <- readr::read_csv("data-raw/mtscr_self_rank.csv") |>
  tibble::as_tibble()

usethis::use_data(mtscr_self_rank, overwrite = TRUE)
