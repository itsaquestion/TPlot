context("test-TTR")


test_that("TTR", {
  
  library(xts)
  library(checkmate)
  library(purrr)
  library(ggplot2)
  library(directlabels)
  library(egg)
  library(TTR)
  
  date_1 = seq(as.Date("2000-01-01"),as.Date("2000-01-10"),"days")
  
  df_1 = data.frame(a=1:10,b=((2:11) - 0.9)) - 1
  
  x = xts::as.xts(df_1+10, order.by = date_1)
  y = xts::as.xts(df_1, order.by = date_1  + 5)
  z = x
  names(z) = c("1a","2b")
  
  a = TTR::SMA(zoo::na.trim(x$a),3)
  
  ggxts(a)
  
})




















