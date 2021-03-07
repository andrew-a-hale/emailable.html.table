test_that("dfToHtmlTable works", {
  expect_type(dfToHtmlTable(mtcars), "character")
  expect_type(dfToHtmlTable(mtcars[0, ]), "character")
  expect_type(
    dfToHtmlTable(
      mtcars,
      colToCollapse = "carb", title = "mtcars by carb", subtitle = "collapse",
      footnotes = c("by: wrong", "note:good")
    ),
    "character"
  )
})

test_that("dfToHtmlTable with colToCollapse works", {
  expect_type(dfToHtmlTable(iris, colToCollapse = "Species"), "character")
  expect_type(dfToHtmlTable(mtcars, colToCollapse = "carb"), "character")
})
