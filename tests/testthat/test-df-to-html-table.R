test_that("dfToHtmlTable works", {
  expect_known_output(
    print(dfToHtmlTable(mtcars)),
    "~/code/r.packages/emailable.html.table/tests/test-reports/1.html"
  )
  expect_known_output(
    print(dfToHtmlTable(iris[0, ])),
    "~/code/r.packages/emailable.html.table/tests/test-reports/2.html"
  )
  expect_known_output(
    print(
      dfToHtmlTable(
        mtcars, colToCollapse = "carb", title = "mtcars by carb", subtitle = "collapse",
        footnotes = c("by: wrong", "note:good")
      )
    ),
    "~/code/r.packages/emailable.html.table/tests/test-reports/5.html"
  )
})

test_that("dfToHtmlTable with colToCollapse works", {
  expect_known_output(
    print(dfToHtmlTable(iris, colToCollapse = "Species")),
    "~/code/r.packages/emailable.html.table/tests/test-reports/3.html"
  )
  expect_known_output(
    print(dfToHtmlTable(mtcars, colToCollapse = "carb")),
    "~/code/r.packages/emailable.html.table/tests/test-reports/4.html"
  )
})
