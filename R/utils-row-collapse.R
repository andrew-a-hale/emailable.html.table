#' Collapse rows in HTML table
#'
#' @param htmlTable html table made with htmltools::tags
#' @param colToCollapse column name to collapse
#' @param strippedBgColour collapsed row background colour
#'
#' @return
#' @export
rowCollapse <- function(htmlTable, colToCollapse, strippedBgColour) {
  ind <- which(names(htmlTable$children[[1]]$children[[1]]) == colToCollapse)

  rows <- htmlTable$children[[2]]
  targets <- c()
  target <- NULL

  spans <- table(unlist(Map(function(.x) .x$children[[1]][[ind]]$children[[1]], rows)))

  for (i in seq_along(rows)) {
    if (
      !identical(target, rows[[i]]$children[[1]][[ind]]$children[[1]]) ||
        identical(i, 1)
    ) {
      target <- rows[[i]]$children[[1]][[ind]]$children[[1]]
      span <- spans[[target]]
    }
    if (target %notin% targets) {
      rows[[i]]$children[[1]][[ind]]$attribs <- append(
        list(rowspan = span),
        list(
          style = paste0(
            "background:", strippedBgColour, ";",
            rows[[i]]$children[[1]][[ind]]$attribs$style
          )
        )
      )
      targets <- append(targets, as.character(target))
    } else {
      rows[[i]]$children[[1]][[ind]] <- list()
    }
  }

  htmlTable$children[[2]] <- rows
  htmlTable
}
