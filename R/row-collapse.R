#' Collapse rows in HTML table
#'
#' @param htmlTable html table made with htmltools::tags
#' @param colToCollapse column name to collapse
#' @param strippedBgColour collapsed row background colour
#'
#' @return
#' @export
#'
#' @examples
rowCollapse <- function(htmlTable, colToCollapse, strippedBgColour) {

  ind <- purrr::detect_index(
    htmlTable[["children"]][[1]][["children"]][[1]],
    ~identical(.x[["children"]][[1]], colToCollapse)
  )

  rows <- htmlTable[["children"]][[2]]
  targets <- c()
  target <- NULL

  map(rows, ~.x[["children"]][[1]][[ind]][["children"]][[1]])
  for (i in 1:length(rows)) {
    if (!identical(target, rows[[i]][["children"]][[1]][[ind]][["children"]][[1]]) || i == 1) {
      target <- rows[[i]][["children"]][[1]][[ind]][["children"]][[1]]
      span <- length(
        keep(
          rows,
          ~identical(target, .x[["children"]][[1]][[ind]][["children"]][[1]])
        )
      )
    }
    if (span > 1 && target %notin% targets) {
      rows[[i]][["children"]][[1]][[ind]][["attribs"]] <- append(
        list(rowspan = span),
        list(
          style = paste0(
            "background:", strippedBgColour, ";",
            rows[[i]][["children"]][[1]][[ind]][["attribs"]][["style"]]
          )
        )
      )
      targets <- append(targets, as.character(target))
    }
    else if (target %notin% targets) {
      targets <- append(targets, as.character(target))
    } else {
      rows[[i]][["children"]][[1]][[ind]] <- list()
    }
  }

  htmlTable[["children"]][[2]] <- purrr::compact(rows)
  htmlTable

}
