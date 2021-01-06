
#' makeReflectionLine
#'
#' get reflection line data
#' @param fun a function 
#' @param x_start_point x start point 
#' @param steps how many steps 
#'
#' @return a data.frame of reflection line
#' @export
#'
#' @examples
makeReflectionLine = function(fun, x_start_point, steps = 10){
  assertFunction(fun)
  assertNumber(x_start_point)
  assertNumber(steps,lower = 1)
  
  df = data.frame(x=double(),y=double())
  
  x = x_start_point
  y = 0 
  
  df = rbind(df,data.frame(x=x,y=0))
  
  for( i in 1:steps){
    
    if(i %% 2 == 1){
      y = fun(x)
      
      df = rbind(df,data.frame(x,y))
      
    }else{
      x = y
      y = y
      
      df = rbind(df,data.frame(x,y))
    }
    
  }
  df
}





makeArrows = function(df,n=4){
  breaks = data.frame(xstart=double(),xend=double(),ystart=double(),yend=double())
  
  for(i in 1:n){
    start_point= df[i,]
    end_point =  df[i+1,]
    breaks = rbind(breaks,data.frame(xstart=start_point$x,
                                     xend=end_point$x,
                                     ystart=start_point$y,
                                     yend=end_point$y))
    
  }
  breaks
  
}

#' addReflectionPath
#' 
#' @param p ggplot object
#' @param fun a function
#' @param x_start_point x start point 
#' @param steps reflect times
#' @param col line color
#' @param add_arrow add arrows?
#' @param n_arrows number of arrows
#' @param arrow_size size of arrows
#'
#' @return ggplot object
#' @export
#' @import checkmate
#' @import ggplot2
#'
#' @examples
addReflectionPath = function(p,fun, x_start_point, steps = 10, col="red",
                             add_arrow = F,n_arrows = 5,arrow_size=0.3){
  
  assertClass(p,"gg")
  assertFunction(fun)
  assertNumber(x_start_point)
  assertNumber(steps,lower = 1)
  assertFlag(add_arrow)
  assertNumber(n_arrows,lower = 1)
  assertNumber(arrow_size,lower = 0)
  
  
  path = makeReflectionLine(fun,x_start_point,steps)
  p = p + geom_path(data=path,aes(x=x,y=y),col=col)
  
  if(add_arrow){
    breaks = makeArrows(path,n=n_arrows)
    p = p + geom_segment(data = breaks, aes(xstart, ystart, xend = xend, yend = yend), 
                         arrow = arrow(length = unit(arrow_size, "line"),type = "closed"),
                         col=col)
  }
  p
  
}