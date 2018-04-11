context("test tests")

test_that("my_add works", {
  expect_equal(my_add(1, 2), 3)
})


test_that("reverse_string works", {
  expect_equal(reverse_string("abc"), "cba") 
})

test_that("teenage_capitalisation works", {
    test_str <- paste0(sample(letters, 10), collapse = "")
    teenage_capitalised_str <- teenage_capitalise(test_str)
    expect_match(test_str, teenage_capitalised_str, ignore.case = TRUE)
    expect_match(teenage_capitalise(teenage_capitalised_str), 
                 teenage_capitalised_str, 
                 ignore.case = TRUE)
})
