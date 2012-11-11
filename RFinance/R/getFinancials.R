getFinancials = function(Symbols) {

  lapply(Symbols, function(symbol) {
    link = paste("http://www.google.com/finance?q=NASDAQ:",symbol,
      "&fstype=ii", sep="")
    financials = readHTMLTable(link)
    x = financials[[5]]
  })

}
