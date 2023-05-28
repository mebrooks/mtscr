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
  result <- mtscr_prepare(df, id, item, score, top = 1:2)
  expect_named(result, c(names(df), ".z_score", ".ordering", ".ordering_top1", ".ordering_top2"), ignore.order = TRUE)
})

# Test that the function returns the expected number of rows
test_that("mtscr_prepare returns the expected number of rows", {
  result <- mtscr_prepare(df, id, item, score)
  expect_equal(nrow(result), nrow(df))
})

# Test that df must be a data frame
test_that("df must be a data frame", {
  # call function with a vector
  expect_error(mtscr_prepare(1:10, id, item, score), regexp = "must be a data frame.")
})

# Test that all columns exist in the data
# id_column
test_that("id_column must exist in the data", {
  # create a test data frame without the id column
  df_no_id <- df[, c("item", "score")]

  # call function with test data frame and no id column
  expect_error(mtscr_prepare(df_no_id, id, item, score), regexp = "does not exist.")
})

# item_column
test_that("item_column must exist in the data", {
  # create a test data frame without the item column
  df_no_item <- df[, c("id", "score")]

  # call function with test data frame and no item column
  expect_error(mtscr_prepare(df_no_item, id, item, score), regexp = "does not exist.")
})

# score_column
test_that("score_column must exist in the data", {
  # create a test data frame without the score column
  df_no_score <- df[, c("id", "item")]

  # call function with test data frame and no score column
  expect_error(mtscr_prepare(df_no_score, id, item, score), regexp = "does not exist.")
})

# Test that score_column must be numeric
test_that("score_column must be numeric", {
  # create a test data frame with a non-numeric value column
  df_string_scores <- data.frame(id = c(1, 2), item = c("apple", "banana"), value = c("red", "yellow"))

  # call function with test data frame and non-numeric value column
  expect_error(mtscr_prepare(df_string_scores, id, item, value), regexp = "must be numeric.")
})

# Test that minimal argument must be logical
expect_error(mtscr_prepare(df, id, item, score, minimal = "yes."), regexp = "must be logical.")

# Test that minimal argument works as expected
test_that("minimal argument works as expected", {
  # call function with minimal = TRUE
  res_minimal <- mtscr_prepare(df, id, item, score, minimal = TRUE)

  # call function with minimal = FALSE
  res_full <- mtscr_prepare(df, id, item, score, minimal = FALSE)

  # check that res_minimal has only the additional columns
  expect_equal(ncol(res_minimal), 5)
  expect_true(".z_score" %in% colnames(res_minimal))
  expect_true(".ordering" %in% colnames(res_minimal))
  expect_true(".ordering_top1" %in% colnames(res_minimal))

  # check that res_full has the additional columns
  expect_equal(ncol(res_full), ncol(df) + 3)
  expect_true(".z_score" %in% colnames(res_full))
  expect_true(".ordering" %in% colnames(res_full))
  expect_true(".ordering_top1" %in% colnames(res_full))

  # check that res_minimal and res_full have the same values for the additional columns
  expect_equal(res_minimal$.z_score, res_full$.z_score)
  expect_equal(res_minimal$.ordering, res_full$.ordering)
  expect_equal(res_minimal$.ordering_top1, res_full$.ordering_top1)
})

# Test that top argument must be integer
test_that("top argument must be an integer", {
  # call function with top = "yes."
  expect_error(mtscr_prepare(df, id, item, score, top = "yes."), regexp = "must be an integer")

  # call function with top = 1.5
  expect_error(mtscr_prepare(df, id, item, score, top = 1.5), regexp = "must be an integer")

  # call function with top = -2
  expect_error(mtscr_prepare(df, id, item, score, top = -2), regexp = "positive integer")
})