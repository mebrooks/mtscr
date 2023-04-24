# Create a test data frame
set.seed(1234)

df <- data.frame(
  id = rep(1:2, each = 9),
  item = rep(letters[1:3], 2, each = 3),
  score = runif(18, 0, 1)
)

# Test that the function returns a tibble
test_that("mtscr_prepare returns a tibble", {
  expect_true(tibble::is_tibble(mtscr_prepare(df, id, item, score)))
})

# Test that the function adds the expected columns
test_that("mtscr_prepare adds the expected columns", {
  result <- mtscr_prepare(df, id, item, score)
  expect_named(result, c(names(df), ".z_score", ".max_ind", ".top2_ind", ".ordering"), ignore.order = TRUE)
})

# Test that the function returns the expected number of rows
test_that("mtscr_prepare returns the expected number of rows", {
  result <- mtscr_prepare(df, id, item, score)
  expect_equal(nrow(result), nrow(df))
})

# Test that the function returns the expected values
test_that("mtscr_prepare returns the expected values", {
  result <- mtscr_prepare(df, id, item, score, preserve_existing = FALSE)
  expected <- tibble::tibble(
    ".max_ind" = rep(c(0, 1, 1), 6),
    ".top2_ind" = rep(c(0, 0, 1), 6),
    ".ordering" = rep(1:3, 6)
  )
  expect_equal(result, expected)
})

# create a test data frame with some NA values

test_that("function correctly removes rows with NA values", {
  df_na <- data.frame(id = c(1, 1, 2, 2), item = c("apple", "banana", "car", "bike"), value = c(3, 2, NA_integer_, 4))
  # check that message was displayed
  expect_message(mtscr_prepare(df_na, id, item, value), regexp = "The value column contains NA values. Rows with NA creativity scores have been deleted.")

  # check that rows with NA values were removed
  expect_equal(nrow(suppressMessages(mtscr_prepare(df_na, id, item, value))), 3)
})

test_that(".value_column must be numeric", {
  # create a test data frame with a non-numeric value column
  df_string_scores <- data.frame(id = c(1, 2), item = c("apple", "banana"), value = c("red", "yellow"))

  # call function with test data frame and non-numeric value column
  expect_error(mtscr_prepare(df_string_scores, id, item, value), regexp = ".value_column must be numeric.")
})

# Test that preserve_existing argument works as expected
test_that("preserve_existing argument works as expected", {
  # call function with preserve_existing = TRUE
  res1 <- mtscr_prepare(df, id, item, score, preserve_existing = TRUE)

  # call function with preserve_existing = FALSE
  res2 <- mtscr_prepare(df, id, item, score, preserve_existing = FALSE)

  # check that res1 has additional columns
  expect_true(".max_ind" %in% colnames(res1))
  expect_true(".top2_ind" %in% colnames(res1))
  expect_true(".ordering" %in% colnames(res1))

  # check that res2 has only the additional columns
  expect_equal(ncol(res2), 3)
  expect_true(".max_ind" %in% colnames(res2))
  expect_true(".top2_ind" %in% colnames(res2))
  expect_true(".ordering" %in% colnames(res2))

  # check that res1 and res2 have the same values for the additional columns
  expect_equal(res1$.max_ind, res2$.max_ind)
  expect_equal(res1$.top2_ind, res2$.top2_ind)
  expect_equal(res1$.ordering, res2$.ordering)
})

# Test that groups removal works as expected
test_that("groups removal works as expected", {
  df_groupped <- dplyr::group_by(df, item)
  expect_message(mtscr_prepare(df_groupped, id, item, score), regexp = "The data frame has been ungroupped.")
  expect_false(dplyr::is_grouped_df(suppressMessages(mtscr_prepare(df_groupped, id, item, score))))
})
