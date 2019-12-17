#' gghist
#'
#' @param x
#' @param bins
#' @param density
#' @param color
#'
#' @return
#' @export
#'
#' @examples
gghist = function(x,bins = 30,density=F,color = "grey95",...){
  df = data.frame(x)
  if(density){
    ggplot(df, aes(x=x),...) +
      geom_histogram(aes(y=..density..),color="black", fill=color,bins=bins)+
      geom_density()
  }else{
    ggplot(df, aes(x=x),...) +
      geom_histogram(color="black", fill=color,bins=bins)
  }
  
}
