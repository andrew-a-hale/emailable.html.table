test_that("pasteReplaceNa works", {
  expect_equal(pasteReplaceNa("a", NA, 1), "a1")
  expect_equal(pasteReplaceNa("a", "b"), "ab")
  expect_equal(pasteReplaceNa(NA, NA), "")
  expect_equal(pasteReplaceNa(NA, 1, NA), "1")
})
