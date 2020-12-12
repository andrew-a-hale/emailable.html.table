test_that("is odd tests", {
  expect_true(isOdd(1))
  expect_false(isOdd(2))
  expect_false(isOdd(1.2))
  expect_error(isOdd("1"))
})
