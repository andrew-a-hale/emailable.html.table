test_that("dfToHtmlTable works", {
  expect_known_output(
    print(dfToHtmlTable(mtcars)),
    "~/emailable.html.table/tests/test-reports/1.html"
  )
  expect_known_output(
    print(dfToHtmlTable(iris[0, ])),
    "~/emailable.html.table/tests/test-reports/2.html"
  )
})

test_that("dfToHtmlTable with colToCollapse works", {
  expect_known_output(
    print(dfToHtmlTable(iris, colToCollapse = "Species")),
    "~/emailable.html.table//tests/test-reports/3.html"
  )
  expect_known_output(
    print(dfToHtmlTable(mtcars, colToCollapse = "carb")),
    "~/emailable.html.table//tests/test-reports/4.html"
  )
})

