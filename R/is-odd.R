#' Is Odd Test
#'
#' @param x numeric
#'
#' @return
#' @export
#'
#' @examples
#' isOdd(1)
#' isOdd(2)
#' isOdd(1.2)
isOdd <- function(x) {
  x %% 2 == 1
}
