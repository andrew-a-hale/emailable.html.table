#' Paste Replace NA
#'
#' Similar to base r paste0 but remove args that are NA
#'
#' @param ... args to turn into a string
#'
#' @return
#' @export
#'
#' @examples
#' pasteReplaceNa("a", NA, 1)
#' pasteReplaceNa("a", "b")
pasteReplaceNa <- function(...) {
  v <- list(...)
  paste0(v[!is.na(v)], collapse = "")
}
