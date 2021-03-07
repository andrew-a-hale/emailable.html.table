#' Not In operator
#'
#' @name %notin%
#' @rdname notin
#' @keywords internal
#' @export
#' @examples
#' "a" %notin% LETTERS
#' "A" %notin% LETTERS
#' 1 %notin% NULL
`%notin%` <- Negate(`%in%`)
