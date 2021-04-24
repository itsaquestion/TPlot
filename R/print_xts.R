#' print.xts
#'
#' @param x 
#' @param n 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
print.xts = function(x,n = 5, ...){
  
  if(n < (nrow(x)/2)){
    a = as.data.frame(head(x,n))
    #names(a) = names(x)
    
    b = as.data.frame(tail(x,n))
    #names(b) = names(x)
    
    print(a, ...)
    cat('... ...\n')
    print(b, ...)
    
  }else{
    print(as.data.frame(x),...)
  }
  
}