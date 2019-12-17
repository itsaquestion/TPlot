# library(directlabels)
#' ggvline
#'
#' @param x
#' @param col
#'
#' @return
#' @export
#' @import directlabels
#'
#' @examples
ggvline = function(x = 0, col = "gray50", ...) {
  geom_vline(xintercept = x, col = col, ...)
}

#' gghline
#'
#' @param y
#' @param col
#'
#' @return
#' @export
#'
#' @examples
gghline = function(y = 0, col = "gray50", ...) {
  geom_hline(yintercept = y, col = col, ...)
}
