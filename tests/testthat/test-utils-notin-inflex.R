test_that("not in test", {
  expect_true("a" %notin% LETTERS)
  expect_false("A" %notin% LETTERS)
})
