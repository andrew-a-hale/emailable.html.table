testHeader <- tags$tr(list(tags$th("col1"), tags$th("col2")))
testRows <- list(
  tags$tr(list(tags$td("a"), tags$td("b"))),
  tags$tr(list(tags$td("a"), tags$td("b"))),
  tags$tr(list(tags$td("a"), tags$td("d"))),
  tags$tr(list(tags$td("c"), tags$td("d")))
)
test <- tags$table(testHeader, testRows)

header <- tags$tr(list(tags$th("col1"), tags$th("col2")))
rows <- list(
  tags$tr(list(tags$td("a", rowspan = 3L, style = "background:#cccccc;"), tags$td("b"))),
  tags$tr(list(tags$td("b"))),
  tags$tr(list(tags$td("d"))),
  tags$tr(list(tags$td("c", rowspan = 1L, style = "background:#cccccc;"), tags$td("d")))
)
desired_outcome <- tags$table(header, rows)

test_that("rowCollapse works", {
  expect_equal(as.character(desired_outcome), as.character(rowCollapse(test, "col1", "#cccccc")))
})
