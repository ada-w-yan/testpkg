context("test tests")

test_that("my_add works", {
  expect_equal(my_add(1, 2), 3)
})


test_that("temp", {
  a <- list(1:10, letters)
  
  expect_output(str(a), "List of 2")
  expect_output(str(a), "int [1:10]", fixed = TRUE)
})
