# Create a test data frame
df <- data.frame(
  id = rep(1:2, each = 5),
  item = rep(letters[1:5], 2),
  score = runif(10, 0, 1)
)

# Test that the function returns a data frame
test_that("mtscr_prepare returns a data frame", {
  expect_is(mtscr_prepare(df, id, item, score), "data.frame")
})

# Test that the function adds the expected columns
test_that("mtscr_prepare adds the expected columns", {
  result <- mtscr_prepare(df, id, item, score)
  expect_named(result, c(".max_ind", ".top2_ind", ".ordering"))
})

# Test that the function returns the expected number of rows
test_that("mtscr_prepare returns the expected number of rows", {
  result <- mtscr_prepare(df, id, item, score)
  expect_equal(nrow(result), nrow(df))
})

# Test that the function returns the expected values
test_that("mtscr_prepare returns the expected values", {
  result <- mtscr_prepare(df, id, item, score, preserve_existing = FALSE)
  expected <- data.frame(
    ".max_ind" = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1),
    ".top2_ind" = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    ".ordering" = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5)
  )
  expect_equal(result, expected)
})

# create a test data frame with some NA values
df <- data.frame(id = c(1, 1, 2, 2), item = c("apple", "banana", "car", "bike"), value = c(3, 2, NA, 4))

test_that("function correctly removes rows with NA values", {
  # call function with test data frame and NA value column
  res <- mtscr_prepare(df, id, item, value)

  # check that warning message was displayed
  expect_warning(mtscr_prepare(df, id, item, value), message = "The value column contains NA values. Rows with NA creativity scores have been deleted.")

  # check that rows with NA values were removed
  expect_equal(nrow(res), 3)
})

test_that(".value_column must be numeric", {
  # create a test data frame with a non-numeric value column
  df <- data.frame(id = c(1, 2), item = c("apple", "banana"), value = c("red", "yellow"))

  # call function with test data frame and non-numeric value column
  expect_error(mtscr_prepare(df, id, item, value), message = ".value_column must be numeric.")
})

df <- data.frame(
  id = c(1, 1, 2, 2, 3, 3),
  item = c("a", "b", "a", "b", "a", "b"),
  score = c(3, 2, 4, 5, 2, 1)
)

# Test preserve_existing = TRUE
df_out1 <- mtscr_prepare(df, id_column = id, item_column = item, value_column = score, preserve_existing = TRUE)
expect_equal(names(df_out1), c("id", "item", "score", ".z_score", ".max_ind", ".top2_ind", ".ordering"))

# Test preserve_existing = FALSE
df_out2 <- mtscr_prepare(df, id_column = id, item_column = item, value_column = score, preserve_existing = FALSE)
expect_equal(names(df_out2), c(".max_ind", ".top2_ind", ".ordering"))
