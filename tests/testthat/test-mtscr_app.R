# Test that function runs
testthat::test_that("mtscr_app runs", {
  # call function
  expect_no_error(mtscr_app(debug_mode = 1))
})

# Test that all needed packages are checked
needed_packages <- c("DT", "broom.mixed", "datamods", "writexl")
returned_message <- mtscr_app(debug_mode = 1)

testthat::test_that("all needed packages are checked", {
  for (pkg in needed_packages) {
    expect_match(returned_message, pkg, all = FALSE)
  }
})

# Test that error is thrown if needed packages are not installed
testthat::test_that("error is thrown if needed packages are not installed", {
    expect_error(mtscr_app(debug_mode = 2))
})
