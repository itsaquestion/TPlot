#' toWeekly
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
toWeekly = function(x){
  ret = Reduce(cbind,lapply(x,function(y){to.weekly(y)[,4]}))
  names(ret) = names(x)
  ret
}