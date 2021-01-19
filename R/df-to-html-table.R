# features
#   alignment -- done
#   row and header background -- done
#   font-family -- done
#   font-size-header -- done
#   font-size-cells -- done
#   font-weight-header -- done
#   nowrap for a specified column -- done
#   spanner column
#   collapse by column -- done
#   title, subtitle, footnotes
#   simple borders -- done
#   total row for numerics


#' Dataframe to HTML Table
#'
#' Simple HTML Table intended to be emailed.
#'
#' @param df A data.frame
#' @param align
#' @param width
#' @param font
#' @param headerBgColour
#' @param headerFontColour
#' @param extraHeaderCss
#' @param strippedBgColour
#' @param strippedFontColour
#' @param extraRowCss
#' @param highlightRowColour
#' @param highlightRows
#' @param borderStyle
#' @param borderLocation
#' @param colToCollapse
#' @param headerFontSize
#' @param headerFontWeight
#' @param cellFontSize
#' @param nowrapCols
#' @param title
#' @param subtitle
#' @param footnotes
#'
#' @return
#' @export
#'
#' @examples
#' htmltools::html_print(dfToHtmlTable(head(mtcars)))
#' htmltools::html_print(dfToHtmlTable(iris[0, ]))
#' htmltools::html_print(dfToHtmlTable(iris, colToCollapse = "Species"))
#' htmltools::html_print(dfToHtmlTable(mtcars, colToCollapse = "carb"))
dfToHtmlTable <- function(df,
                          align = "c",
                          width = "100%",
                          font = "Arial, sans-serif",
                          headerBgColour = "#15679f",
                          headerFontColour = "#ffffff",
                          headerFontSize = "12px",
                          headerFontWeight = "bold",
                          strippedBgColour = "#cccccc",
                          strippedFontColour = "#000000",
                          cellFontSize = "12px",
                          highlightRowColour = NULL,
                          highlightRows = NULL,
                          borderStyle = "1px solid black",
                          borderLocation = "all",
                          colToCollapse = NULL,
                          extraHeaderCss = NA,
                          extraRowCss = NA,
                          nowrapCols = NULL,
                          title = NULL,
                          subtitle = NULL,
                          footnotes = NULL) {
  stopifnot(
    ncol(df) > 1,
    length(names(df)) > 0,
    length(align) == 1,
    grep("[lcr]", align, invert = TRUE) == integer(0), # all characters in pattern
    colToCollapse %in% names(df) || missing(colToCollapse),
    nowrapCols %in% names(df) || missing(nowrapCols),
    !missing(title) && !missing(subtitle) || missing(subtitle), # must have a title if subtitle exists
    borderLocation %in% c("all", "row", "col"),
    headerFontWeight %in% c("normal", "bold") ||
      (is.numeric(headerFontWeight) && between(headerFontWeight, 100, 900))
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
    pluck(1) %>%
    stringr::str_replace_all(c("l" = "left", "r" = "right", "c" = "center")) %>%
    purrr::set_names(ns)

  # row collapse ------------------------------------------------------------
  ## row collapse orders by the specified column
  ## need to sanitise the specified column in case they are numeric, or factors
  if (!missing(colToCollapse)) {
    df <- data.table::setorderv(df, c(colToCollapse))
    df[[colToCollapse]] <- as.character(df[[colToCollapse]])
  }

  # borders -----------------------------------------------------------------
  if (identical(borderLocation, "all")) {
    borderCss <- str_glue("border:{borderStyle};")
  }
  else if (identical(borderLocation, "rows")) {
    borderCss <- str_glue(
      "border-top:{borderStyle};",
      "border-bottom:{borderStyle};"
    )
  }
  else if (identical(borderLocation == "cols")) {
    borderCss <- str_glue(
      "border-left:{borderStyle};",
      "border-right:{borderStyle};"
    )
  }
  else {
    borderCss <- NA
  }

  # make header ---------------------------------------------------------
  header <- tags$tr(
    map(
      ns,
      ~ tags$th(
        .x,
        style = str_glue(
          "background:{headerBgColour};",
          "color:{headerFontColour};",
          "text-align:{alignment[[.x]]};",
          "font-size:{headerFontSize};",
          "font-weight:{headerFontWeight};",
          "white-space:{ws};",
          "{borderCss}",
          "{extraHeaderCss}",
          .na = "",
          ws = if (.x %in% nowrapCols) "nowrap" else "normal"
        )
      )
    )
  )

  # make rows ---------------------------------------------------------
  if (rs == 0) {
    rows <- tags$tr(
      tags$td(
        "No Data Available",
        colspan = cs,
        style = str_glue(
          "text-align:center;",
          "font-size:{cellFontSize};",
          "{borderCss}"
        )
      )
    )
  } else {
    rows <- map(
      seq.int(1, rs),
      function(.r) {
        tags$tr(
          map(
            ns,
            function(.c) {
              tags$td(
                df[.r, .c],
                style = str_glue(
                  "text-align:{alignment[[.c]]};",
                  "{borderCss}"
                )
              )
            }
          ),
          style = str_glue(
            "font-size:{cellFontSize};",
            "background:{bg};",
            # bg ternary
            bg = if (isOdd(.r) && .r %notin% highlightRows) {
              strippedBgColour
            } else if (.r %in% highlightRows) {
              highlightRowColour
            } else {
              "#ffffff"
            },
            "color:{font};",
            # font ternary
            font = if (isOdd(.r) && .r %notin% highlightRows) {
              strippedFontColour
            } else if (.r %in% highlightRows) {
              "#ffffff"
            } else {
              "#000000"
            },
            "{extraRowCss}",
            .na = ""
          )
        )
      }
    )
  }

  # make table ---------------------------------------------------------
  table <- tags$table(
    style = str_glue(
      "border-collapse:collapse;",
      "width:{width};",
      "font-family:{font}",
      .na = ""
    ),
    header,
    rows
  )

  # return object ---------------------------------------------------------
  ## collapse rows
  if (!missing(colToCollapse)) {
    table <- rowCollapse(table, colToCollapse, strippedBgColour = "#cccccc")
  }

  ## add title
  if (!missing(title)) {

  }

  ## add subtitle
  if (!missing(subtitle)) {

  }

  ## add foot notes
  if (!missing(footnotes)) {

  }

  table
}
