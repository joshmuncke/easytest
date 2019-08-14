context("test-df-tests")

test_that("no duplicates works as expected", {
  expect_identical(test_no_duplicates(mtcars), mtcars)
  expect_error(test_no_duplicates(mtcars, cyl))
})
