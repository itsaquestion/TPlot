context("test-toWeekly")


test_that("basic", {

  date_1 = seq(as.Date("2000-01-01"),as.Date("2000-02-10"),"days")

  df_1 = data.frame(a=1:41,b=((2:42) - 0.9)) - 1
  x = xts::as.xts(df_1+10, order.by = date_1)
  y = xts::as.xts(df_1, order.by = date_1  + 5)
  z = x
  names(z) = c("1a","2b")
 
  
  #toWeekly(z)
  
  testthat::expect_equal(unique(lubridate::wday(index(toWeekly(z)))),c(1,5))
  
  
})




















