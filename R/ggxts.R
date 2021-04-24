#' ggxts
#'
#' plot xts with end values via ggplot
#' @param x the xts object
#' @param digits digits of ene values
#' @param size line width
#' @import ggplot2
#' @import checkmate
#' @import data.table
#' @importFrom magrittr `%>%`
#' @import directlabels
#' @import xts
#' @import zoo
#'
#' @return ggplot object
#' @export
#'
#'
ggxts = function(x, digits = 2, size = 0.8){
  assertClass(x, "xts")
  df = x %>% 
    as.data.table() %>% 
    melt(id.vars = "index")  %>% 
    .[,last_value:=round(last(value),digits),by=variable] 
  # 加一列last_value，用于directlabels显示最新值
  
  ggplot(df, aes(x=index,y=value,group=variable,color=variable)) + 
    geom_line(size = size) + 
    geom_dl(aes(label = last_value, color = variable),  
            method = list(dl.trans(x = x + 0.1), "last.qp")) + 
    ylab(NULL) + xlab(NULL) + 
    theme(legend.title = element_blank()) 
}
