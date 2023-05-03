data("mtscr_creativity", package = "mtscr")

# Test that `model_type` argument throws an error when invalid values are provided
test_that("model_type argument throws an error when invalid values are provided", {
  # call function with model_type = "invalid"
  expect_error(mtscr_score(mtscr_creativity, id, item, SemDis_MEAN, model_type = "invalid"))

  # call function with model_type = c("all_max", "invalid")
  expect_error(mtscr_score(mtscr_creativity, id, item, SemDis_MEAN, model_type = c("all_max", "invalid")))
})

# Test that `summarise_for` argument works as expected
# person
test_that("summarise_for argument works as expected for person", {
  # call function with summarise_for = "person"
  res_person <- mtscr_score(mtscr_creativity, id, item, SemDis_MEAN, model_type = "all_max", summarise_for = "person")

  # check that res_person has the expected number of rows and columns
  expect_equal(ncol(res_person), 2)

  # check that res_person has the expected column names
  expect_equal(colnames(res_person), c("id", ".all_max"))
})

# item
test_that("summarise_for argument works as expected for item", {
  # call function with summarise_for = "item"
  res_item <- mtscr_score(mtscr_creativity, id, item, SemDis_MEAN, model_type = "all_top2", summarise_for = "item")

  # check that res_item has the expected number of rows and columns
  expect_equal(ncol(res_item), 2)

  # check that res_item has the expected column names
  expect_equal(colnames(res_item), c("item", ".all_top2"))
})

# both
test_that("summarise_for argument works as expected for both", {
  # call function with summarise_for = "both"
  res_both <- mtscr_score(mtscr_creativity, id, item, SemDis_MEAN, model_type = c("all_max", "all_top2"), summarise_for = "both")

  # check that res_both has the expected number of rows and columns
  expect_equal(ncol(res_both), 4)

  # check that res_both has the expected column names
  expect_equal(colnames(res_both), c("id", "item", ".all_max", ".all_top2"))
})

# invalid
test_that("summarise_for argument throws an error when invalid values are provided", {
  # call function with summarise_for = "invalid"
  expect_error(mtscr_score(mtscr_creativity, id, item, SemDis_MEAN, model_type = "all_max", summarise_for = "invalid"))

  # call function with summarise_for = c("person", "invalid")
  expect_error(mtscr_score(mtscr_creativity, id, item, SemDis_MEAN, model_type = "all_max", summarise_for = c("person", "invalid")))
})

# Test that `format` argument works as expected
# minimal_long
test_that("format argument works as expected for long", {
  # call function with format = "long"
  res_long <- mtscr_score(mtscr_creativity, id, item, SemDis_MEAN, model_type = "all_max", summarise_for = "both", format = "minimal_long")

  # check that res_long has the expected number of rows and columns
  expect_equal(ncol(res_long), 3)

  # check that res_long has the expected column names
  expect_named(res_long, c("id", "item", ".all_max"))
})
# minimal_wide, one model
test_that("format argument works as expected for wide and one model", {
  # call function with format = "wide"
  res_wide <- mtscr_score(mtscr_creativity, id, item, SemDis_MEAN, model_type = "all_top2", summarise_for = "both", format = "minimal_wide")

  # check that res_wide has the expected number of rows and columns
  expect_equal(ncol(res_wide), 14)

  # check that res_wide has the expected column names
  expect_named(res_wide, c("id", unique(mtscr_creativity$item)), ignore.order = TRUE)
})

# minimal_wide, two models
test_that("format argument works as expected for wide and two models", {
  # call function with format = "wide"
  res_wide <- mtscr_score(mtscr_creativity, id, item, SemDis_MEAN, model_type = c("all_max", "all_top2"), summarise_for = "both", format = "minimal_wide")

  # check that res_wide has the expected number of rows and columns
  expect_equal(ncol(res_wide), 27)

  # check that res_wide has the expected column names
  expect_named(
    res_wide, c(
      "id",
      paste0(".all_max_", unique(mtscr_creativity$item)),
      paste0(".all_top2_", unique(mtscr_creativity$item))
    ),
    ignore.order = TRUE
  )
})

# full
test_that("format argument works as expected for full", {
  # call function with format = "full"
  res_full <- mtscr_score(mtscr_creativity, id, item, SemDis_MEAN, summarise_for = "both", format = "full")

  # check that res_full has the expected number of rows and columns
  expect_equal(ncol(res_full), 12)

  # check that res_full has the expected column names
  expect_named(res_full, c(names(mtscr_creativity), ".all_max", ".all_top2"))
})

# Test that .all_max and .all_top2 columns are numeric
test_that(".all_max and .all_top2 columns are numeric", {
  # call function with model_type = "all_max"
  res_all_max <- mtscr_score(mtscr_creativity, id, item, SemDis_MEAN, model_type = "all_max")

  # check that .all_max column is numeric
  expect_type(res_all_max$.all_max, "double")

  # check that variance isn't 0
  expect_false(var(res_all_max$.all_max) == 0)

  # call function with model_type = "all_top2"
  res_all_top2 <- mtscr_score(mtscr_creativity, id, item, SemDis_MEAN, model_type = "all_top2")

  # check that .all_top2 column is numeric
  expect_type(res_all_top2$.all_top2, "double")

  # check that variance isn't 0
  expect_false(var(res_all_top2$.all_top2) == 0)
})

# Test that invalid summarise_for errors are thrown
test_that("invalid summarise_for error is thrown", {
  # call function with summarise_for = "invalid"
  expect_error(mtscr_score(mtscr_creativity, id, item, SemDis_MEAN, model_type = "all_max", summarise_for = "invalid"))

  # call function with summarise_for = c("person", "both")
  expect_error(mtscr_score(mtscr_creativity, id, item, SemDis_MEAN, model_type = "all_max", summarise_for = c("person", "both")))
})

# Test that invalid format errors are thrown
test_that("invalid format error is thrown", {
  # call function with format = "invalid"
  expect_error(mtscr_score(mtscr_creativity, id, item, SemDis_MEAN, model_type = "all_max", format = "invalid"))

  # call function with format = c("minimal_long", "full")
  expect_error(mtscr_score(mtscr_creativity, id, item, SemDis_MEAN, model_type = "all_max", format = c("minimal_long", "full")))
})

# Test for message if format is minimal_wide and summarise_for is not both
test_that("message is thrown if format is minimal_wide and summarise_for is not both", {
  # call function with format = "minimal_wide" and summarise_for = "person"
  expect_message(
    mtscr_score(mtscr_creativity, id, item, SemDis_MEAN, model_type = "all_max", summarise_for = "person", format = "minimal_wide"),
    regexp = "summarise_for = \"both\"")
})