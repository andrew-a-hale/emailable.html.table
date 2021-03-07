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
#   title, subtitle, footnotes -- done
#   simple borders -- done
#   total row for numerics


#' Dataframe to HTML Table
#'
#' Simple HTML Table intended to be emailed.
#'
#' @param df A data.frame
#' @param align string to determine alignment eg. "lcr". last character to used to fill remainder
#' @param width table width
#' @param font font family
#' @param headerBgColour header bg colour
#' @param headerFontColour header font colour
#' @param extraHeaderCss extra header css
#' @param strippedBgColour stripped row bg colour
#' @param strippedFontColour stripped row font colour
#' @param extraRowCss extra row css
#' @param highlightRowColour highlight row colour
#' @param highlightRows highlight row indices (positions)
#' @param borderStyle css for border style, eg. "1px solid black"
#' @param borderLocation border location, can be "all", "row", or "col"
#' @param colToCollapse col to collapse on. this applies row spans and orders by the column
#' so that the value does not repeat
#' @param headerFontSize header font size
#' @param headerFontWeight header font weight
#' @param cellFontSize cell font size
#' @param nowrapCols cols to apply nowrap to. see css white-space nowrap.
#' @param title table title
#' @param subtitle table subtitle, requires title
#' @param footnotes table foot notes
#' @param titleAlignment title alignment
#' @param tableLineHeight table line height
#'
#' @return
#' @export
#'
#' @examples
#' htmltools::html_print(dfToHtmlTable(head(mtcars)))
#' htmltools::html_print(dfToHtmlTable(iris[0, ]))
#' htmltools::html_print(dfToHtmlTable(iris, colToCollapse = "Species"))
#' htmltools::html_print(dfToHtmlTable(mtcars, colToCollapse = "carb"))
#' htmltools::html_print(dfToHtmlTable(mtcars, colToCollapse = "carb", tableLineHeight = 1))
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
                          extraHeaderCss = NULL,
                          extraRowCss = NULL,
                          nowrapCols = NULL,
                          title = NULL,
                          subtitle = NULL,
                          footnotes = NULL,
                          titleAlignment = "left",
                          tableLineHeight = 1.25) {
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
      (is.numeric(headerFontWeight) && headerFontWeight >= 100 && headerFontWeight <= 900)
  )

  # set up ------------------------------------------------------------------
  rs <- nrow(df)
  cs <- ncol(df)
  ns <- names(df)
  as <- nchar(align)

  # parse alignment ---------------------------------------------------------
  ## recycle last character to make sure all columns have an alignment
  if (as < cs) {
    align <- paste0(align, strrep(substr(align, as, as), cs - as))
  }

  alignment <- setNames(
    gsub(
      "l", "left",
      gsub(
        "c", "center",
        gsub(
          "r", "right",
          strsplit(align, "")[[1]]
        )
      )
    ),
    ns
  )

  # row collapse ------------------------------------------------------------
  ## row collapse orders by the specified column
  ## need to sanitise the specified column in case they are numeric, or factors
  if (!missing(colToCollapse)) {
    df <- df[order(df[[colToCollapse]]), ]
    df[[colToCollapse]] <- as.character(df[[colToCollapse]])
  }

  # borders -----------------------------------------------------------------
  if (identical(borderLocation, "all")) {
    borderCss <- paste0("border:", borderStyle)
  }
  else if (identical(borderLocation, "rows")) {
    borderCss <- paste0(
      "border-top:", borderStyle, ";",
      "border-bottom:", borderStyle, ";"
    )
  }
  else if (identical(borderLocation, "cols")) {
    borderCss <- paste0(
      "border-left:", borderStyle, ";",
      "border-right:", borderStyle, ";"
    )
  }
  else {
    borderCss <- NULL
  }

  # make header ---------------------------------------------------------
  header <- htmltools::tags$tr(
    Map(
      function(.x) {
        htmltools::tags$th(
          .x,
          style = paste0(
            "background:", headerBgColour, ";",
            "color:", headerFontColour, ";",
            "text-align:", alignment[[.x]], ";",
            "font-size:", headerFontSize, ";",
            "font-weight:", headerFontWeight, ";",
            "white-space:", if (.x %in% nowrapCols) "nowrap" else "normal", ";",
            borderCss, ";",
            extraHeaderCss, ";"
          )
        )
      },
      ns
    )
  )

  # make rows ---------------------------------------------------------
  if (rs == 0) {
    rows <- htmltools::tags$tr(
      htmltools::tags$td(
        "No Data Available",
        colspan = cs,
        style = paste0(
          "text-align:center;",
          "font-size:", cellFontSize, ";",
          borderCss, ";"
        )
      )
    )
  } else {
    rows <- Map(
      function(.r) {
        htmltools::tags$tr(
          Map(
            function(.c) {
              htmltools::tags$td(
                df[.r, .c],
                style = paste0(
                  "text-align:", alignment[[.c]], ";",
                  borderCss, ";"
                )
              )
            },
            ns
          ),
          style = paste0(
            "font-size:", cellFontSize, ";",
            "background:",
            # bg ternary
            bg = if (isOdd(.r) && .r %notin% highlightRows) {
              strippedBgColour
            } else if (.r %in% highlightRows) {
              highlightRowColour
            } else {
              "#ffffff"
            }, ";",
            "color:",
            # font ternary
            font = if (isOdd(.r) && .r %notin% highlightRows) {
              strippedFontColour
            } else if (.r %in% highlightRows) {
              "#ffffff"
            } else {
              "#000000"
            }, ";",
            extraRowCss, ";"
          )
        )
      },
      seq_len(rs)
    )
  }

  # make table ---------------------------------------------------------
  table <- htmltools::tags$table(
    style = paste0(
      "border-collapse:collapse;",
      "width:", width, ";",
      "font-family:", font, ";",
      "line-height:", tableLineHeight, ";"
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
    title <- htmltools::tags$h3(
      style = paste0(
        "text-align:", titleAlignment, ";",
        "margin-top: 0px;",
        "margin-bottom: 5px;"
      ),
      title
    )
  }

  ## add subtitle
  if (!missing(subtitle)) {
    subtitle <- htmltools::tags$h4(
      style = paste0(
        "text-align:", titleAlignment, ";",
        "margin-top: 0px;",
        "margin-bottom: 5px;"
      ),
      subtitle
    )
  }

  ## add foot notes
  if (!missing(footnotes)) {
    footnotes <- htmltools::tagList(
      Map(
        function(.x) {
          htmltools::tags$p(
            style = paste0(
              "margin-top: 0px;",
              "margin-bottom: 0px;",
              "font-size: 12px"
            ),
            .x
          )
        },
        footnotes
      )
    )
  }
  htmltools::HTML(as.character(htmltools::tagList(
    title, subtitle, table, footnotes
  )))
}
