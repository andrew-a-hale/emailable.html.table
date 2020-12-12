# features
#   alignment -- done
#   row and header background -- done
#   font-family -- done
#   font-size-header
#   font-size-cells
#   font-weight-header
#   nowrap for a specified column
#   spanner column
#   collapse by column
#   title, subtitle, notes
#   simple borders
#   total row for numerics


#' Dataframe to HTML Table
#'
#' Simple HTML Table intended to be emailed.
#'
#' @param df A data.frame
#'
#' @return
#' @export
#'
#' @examples
#' htmltools::html_print(dfToHtmlTable(head(iris)))
#' htmltools::html_print(dfToHtmlTable(iris[0, ]))
dfToHtmlTable <- function(
  df, align = "c", width = "100%", font = "\"Sintony\", Arial, sans-serif",
  headerBgColour = "#15679f", headerFontColour = "#ffffff", extraHeaderCss = NA,
  strippedBgColour = "#cccccc", strippedFontColour = "#000000", extraRowCss = NA,
  highlightRowColour = NULL, highlightRows = NULL,
  borderStyle = NULL
) {
  stopifnot(
    ncol(df) > 1,
    length(names(df)) > 0,
    stringr::str_length(align) == ncol(df) || stringr::str_length(align) == 1,
    length(align) == 1,
    grep("[lcr]", align, invert = TRUE) == integer(0) # all characters in pattern
  )

  # set up ------------------------------------------------------------------
  rs <- nrow(df)
  cs <- ncol(df)
  ns <- names(df)
  as <- stringr::str_length(align)

  # parse alignment ---------------------------------------------------------
  ## recycle last character to make sure all columns have an alignment
  if (as < cs) {
    align <- stringr::str_pad(align, cs, side = "right", pad = substr(align, as, as))
  }

  alignment <- strsplit(align, "") %>%
    purrr::pluck(1) %>%
    stringr::str_replace_all(c("l" = "left", "r" = "right", "c" = "center")) %>%
    purrr::set_names(ns)

  # make header ---------------------------------------------------------
  header <- purrr::map(
    ns,
    ~htmltools::tags$th(
      .x,
      style = stringr::str_glue(
        "background:{headerBgColour};",
        "color:{headerFontColour};",
        "text-align:{alignment[[.x]]}",
        "{extraHeaderCss}"
      )
    )
  )

  # make rows ---------------------------------------------------------
  if (rs == 0) {
    rows <- htmltools::tags$tr(
      htmltools::tags$td(
        "No Data Available",
        colspan = cs, style = "text-align:center;"
      )
    )
  } else {
    rows <- purrr::map(
      seq.int(1, rs),
      function(.r) {
        htmltools::tags$tr(
          purrr::map(
            ns,
            function(.c) {
              htmltools::tags$td(
                df[.r, .c],
                style = stringr::str_glue(
                  "text-align:{alignment[[.c]]};"
                )
              )
            }
          ),
          style = stringr::str_glue(
            "background:{bg};",
            # bg ternary
            bg = if (isOdd(.r) && .r %notin% highlightRows)
              strippedBgColour
            else if (.r %in% highlightRows)
              highlightRowColour
            else "#ffffff",
            "color:{font}",
            # font ternary
            font = if (isOdd(.r) && .r %notin% highlightRows)
              strippedFontColour
            else if (.r %in% highlightRows)
              "#ffffff"
            else "#000000",
            "{extraRowCss}"
          )
        )
      }
    )
  }

  # make table ---------------------------------------------------------
  table <- htmltools::tags$table(
    style = stringr::str_glue(
      "border-collapse:collapse;",
      "width:{width};",
      "font-family:{font}"
    ),
    header,
    rows
  )

  # return object ---------------------------------------------------------
  table

}
