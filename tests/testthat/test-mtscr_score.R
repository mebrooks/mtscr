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

# Test that `append` argument works as expected
test_that("append argument works as expected", {
  # call function with append = TRUE
  res_append <- mtscr_score(mtscr_creativity, id, item, SemDis_MEAN, append = TRUE)

  # check that res_append has the expected number of rows and columns
  expect_equal(ncol(res_append), ncol(mtscr_creativity) + 2)

  # check that res_append has the expected column names
  expect_named(res_append, c(names(mtscr_creativity), ".all_max", ".all_top2"), ignore.order = TRUE)
})