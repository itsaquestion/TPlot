context("test-rplot_list")


test_that("multiplication works", {

  library(xts)
  library(checkmate)
  library(purrr)
  library(ggplot2)
  library(directlabels)
  library(egg)

  date_1 = seq(as.Date("2000-01-01"),as.Date("2000-01-10"),"days")

  df_1 = data.frame(a=1:10,b=((2:11) - 0.9)) - 1
  x = xts::as.xts(df_1+10, order.by = date_1)
  y = xts::as.xts(df_1, order.by = date_1  + 5)
  z = x
  names(z) = c("1a","2b")
  
  rplot(x, y,heights = c(2,1),size = 2)

  rplot(z)

  p1 = ggxts(x)
  p2 = ggxts(y$a)

  rplot(p1,p2,heights = c(1,2),size = 10)

  #egg::ggarrange(plots = list(p1,p2),heights=NULL)

  plots = list(p2,p1)
  rplot(plots = plots,heights = c(1,2))


  rplotList(list(p1,p2),theme=theme_textbook())
  expect_class(p1,"gg")

  a = rplot(p1,p2,p1,heights = c(2,1,1))

  vlines = c("2000-01-05","2000-01-13")
  rplot(p1,p2,vlines = vlines)
  

  
})




















