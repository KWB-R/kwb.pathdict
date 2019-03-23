test_that("to_dictionary_key() works", {

  fun <- kwb.pathdict:::to_dictionary_key

  expect_error(fun())
  expect_identical(fun(c(1, 15, 16)), c("p1", "pF", "p10"))
  expect_identical(fun(c(1, 15, 16), "a"), c("a1", "aF", "a10"))
  expect_identical(fun(c(1, 255), leading_zeros = TRUE), c("p01", "pFF"))
})

