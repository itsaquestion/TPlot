#' getWhiteboard
#'
#' theme_bw() + coord_fixed() + axis + no panel.grid.minor + no border
#' @return a ggplot object
#' @export
#'
#' @examples
getWhiteboard = function(){
  ggplot() + coord_fixed() +
    theme_bw() + theme(panel.border = element_blank(), 
                       panel.grid.minor = element_blank(), axis.line = element_blank(),
                       axis.ticks = element_blank()) + 
    geom_hline(yintercept = 0, col = "black") + 
    geom_vline(xintercept = 0, col = "black") +
    xlab("x") + ylab("y") + xlim(0, 1) + ylim(0,1)
}
