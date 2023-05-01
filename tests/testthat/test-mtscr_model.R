data("mtscr_creativity", package = "mtscr")
model <- mtscr_model(mtscr_creativity, id, item, SemDis_MEAN, model_type = "all_max")

# Test the function with the test data
test_that("mtscr_model() works as expected", {
  # Test that the function returns a glmmTMB object
  expect_s3_class(model, "glmmTMB")

  # Test that the function returns the expected number of fixed and random effects

  expect_equal(length(glmmTMB::fixef(model)), 3)
  expect_equal(length(glmmTMB::ranef(model)), 2)
})

# Test that `prepared` argument works as expected
test_that("prepared argument works as expected", {
  # call function with prepared = TRUE
  res_prepared <- mtscr_creativity |>
    mtscr_prepare(id, item, SemDis_MEAN) |>
    mtscr_model(id, item, SemDis_MEAN, model_type = "all_max", prepared = TRUE)

  # call function with prepared = FALSE
  res_unprepared <- mtscr_model(mtscr_creativity, id, item, SemDis_MEAN, model_type = "all_max", prepared = FALSE)

  # check that res_prepared and res_unprepared are the same
  expect_equal(glmmTMB::fixef(res_prepared), glmmTMB::fixef(res_unprepared))
  expect_equal(glmmTMB::ranef(res_prepared), glmmTMB::ranef(res_unprepared))
})

# Test that `model_type` argument works as expected
test_that("model_type argument works as expected", {
  # call function with model_type = "all_max"
  res_all_max <- mtscr_model(mtscr_creativity, id, item, SemDis_MEAN, model_type = "all_max")

  # call function with model_type = "all_top2"
  res_all_top2 <- mtscr_model(mtscr_creativity, id, item, SemDis_MEAN, model_type = "all_top2")

  # call function with model_type = c("all_max", "all_top2")
  res_both <- mtscr_model(mtscr_creativity, id, item, SemDis_MEAN, model_type = c("all_max", "all_top2"))

  # check that res_all_max is a glmmTMB object
  expect_s3_class(res_all_max, "glmmTMB")

  # check that res_all_top2 is a glmmTMB object
  expect_s3_class(res_all_top2, "glmmTMB")

  # check that res_both is a list
  expect_true(is.list(res_both))

  # check that res_both has two elements
  expect_equal(length(res_both), 2)

  # check that res_both has the expected names
  expect_equal(names(res_both), c("all_max", "all_top2"))
})

# Test that `model_type` argument throws an error when invalid values are provided
test_that("model_type argument throws an error when invalid values are provided", {
  # call function with model_type = "invalid"
  expect_error(mtscr_model(mtscr_creativity, id, item, SemDis_MEAN, model_type = "invalid"))

  # call function with model_type = c("all_max", "invalid")
  expect_error(mtscr_model(mtscr_creativity, id, item, SemDis_MEAN, model_type = c("all_max", "invalid")))
})