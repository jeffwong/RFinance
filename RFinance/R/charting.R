#Fetches historic data and calls quantmod's chartSeries
#Can save multiple plots to pdf file
chart = function(Symbols, month1, day1, year1, month2, day2, year2, ndays=NULL, save.file=NULL, width=4, height=3) {
  if(!is.null(ndays)) {
    date2 = as.POSIXlt(Sys.Date())
    month2 = date2$mon
    day2 = as.numeric(format(date2, format="%d"))
    year2 = 1900 + date2$year

    date1 = as.POSIXlt(Sys.Date() - ndays)
    month1 = date1$mon
    day1 = as.numeric(format(date1, format="%d"))
    year1 = 1900 + date1$year
  }
  
  if(!is.null(save.file)) {
    pdf(save.file, width=width, height=height)
  }
  par(ask=TRUE)
  sapply(Symbols, function(symbol) {
    prices = getHistoricData(symbol, month1, day1, year1, month2, day2, year2, return.type="xts")
    chartSeries(prices[[1]])
  })
  if(!is.null(save.file)) {
    dev.off()
  }
}
