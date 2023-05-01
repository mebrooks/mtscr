data("mtscr_creativity", package = "mtscr")
model <- mtscr_model(mtscr_creativity, id, item, SemDis_MEAN)

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
    mtscr_model(id, item, SemDis_MEAN, prepared = TRUE)

  # call function with prepared = FALSE
  res_unprepared <- mtscr_model(mtscr_creativity, id, item, SemDis_MEAN, prepared = FALSE)

  # check that res_prepared and res_unprepared are the same
  expect_equal(glmmTMB::fixef(res_prepared), glmmTMB::fixef(res_unprepared))
  expect_equal(glmmTMB::ranef(res_prepared), glmmTMB::ranef(res_unprepared))
})