testHeader <- htmltools::tags$tr(list(col1 = htmltools::tags$th("col1"), col2 = htmltools::tags$th("col2")))
testRows <- list(
  htmltools::tags$tr(list(htmltools::tags$td(1), htmltools::tags$td("b"))),
  htmltools::tags$tr(list(htmltools::tags$td(1), htmltools::tags$td("b"))),
  htmltools::tags$tr(list(htmltools::tags$td(1), htmltools::tags$td("d"))),
  htmltools::tags$tr(list(htmltools::tags$td(2), htmltools::tags$td("d")))
)
test <- htmltools::tags$table(testHeader, testRows)

header <- htmltools::tags$tr(list(col1 = htmltools::tags$th("col1"), col2 = htmltools::tags$th("col2")))
rows <- list(
  htmltools::tags$tr(list(htmltools::tags$td(1, rowspan = 3L, style = "background:#cccccc;"), htmltools::tags$td("b"))),
  htmltools::tags$tr(list(htmltools::tags$td("b"))),
  htmltools::tags$tr(list(htmltools::tags$td("d"))),
  htmltools::tags$tr(list(htmltools::tags$td(2, rowspan = 1L, style = "background:#cccccc;"), htmltools::tags$td("d")))
)
desired_outcome <- htmltools::tags$table(header, rows)

test_that("rowCollapse works", {
  expect_equal(as.character(desired_outcome), as.character(rowCollapse(test, "col1", "#cccccc")))
})
