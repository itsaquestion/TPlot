#' ggVector 
#'
#' plot a vector, or a data.frame of vectors
#'
#' @param x  a vector, or a data.frame of vectors
#' @param type "l" for line, "p" for points, "b" for boths
#'
#' @return a ggplot object
#' @import reshape2
#' @import tibble
#'
#' @export
#'
#' @examples
ggVector = function(x, type = "l") {
  if (is.vector(x)) {
    p = ggplot(mapping = aes(x = seq_along(x), y = x)) + xlab("index") + ylab("x")
    
  } else if (is.data.frame(x)) {
    df = add_column(index = 1:nrow(x), x)
    
    df_m = reshape2::melt(df, "index")
    
    p = ggplot(df_m, aes(x = index, y = value, group = variable, color = variable))
  }
  
  if (type == "l") {
    p = p + geom_line()
  } else if (type == "p") {
    p = p + geom_point()
  } else if (type == "b") {
    p = p + geom_point() + geom_line()
  } else {
    p = p + geom_line()
  }
  p
}