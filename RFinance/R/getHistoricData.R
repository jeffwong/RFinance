getHistoricData = function(Symbols, month1, day1, year1, month2, day2, year2, ndays=NULL, return.type="xts", appendSymbol=FALSE, mergeAll=FALSE) {

  #Compute Dates
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

  #Format Dates for Yahoo API
  historicPrices.suffix = paste("&a=", month1, sep='')
  historicPrices.suffix = paste(historicPrices.suffix, "&b=", day1, sep='')
  historicPrices.suffix = paste(historicPrices.suffix, "&c=", year1, sep='')
  historicPrices.suffix = paste(historicPrices.suffix, "&d=", month2, sep='')
  historicPrices.suffix = paste(historicPrices.suffix, "&e=", day2, sep='')
  historicPrices.suffix = paste(historicPrices.suffix, "&f=", year2, sep='')
  historicPrices.suffix = paste(historicPrices.suffix, "&g=d&ignore=.csv", sep='')

  #CREATE A LIST OF DATA FRAMES
  historicPrices.list = lapply(Symbols, function(symbol) {
    historicPrices.url = paste("http://ichart.finance.yahoo.com/table.csv?s=",
      symbol, sep='')
    historicPrices.url = paste(historicPrices.url, historicPrices.suffix, sep='')
    success = download.file(historicPrices.url, dest="deleteme")
    if(success != 0) {
      stop(paste("ERROR DOWNLOADING FILE from:", historicPrices.url, sep=" "))
    }
    prices = read.csv("deleteme", header=T)
    unlink("deleteme")

    if(return.type == "xts") {
      prices <- xts(as.matrix(prices[,-1]),
        as.POSIXct(prices[,1], tz=Sys.getenv("TZ")),
        src='yahoo',updated=Sys.time())
      colnames(prices) <- paste(toupper(symbol),
        c('Open','High','Low','Close','Volume','Adjusted'),
        sep='.')
    }
    if(return.type == "df") {
      if(appendSymbol == TRUE) {
        Ticker = rep(symbol, nrow(prices))
        prices = cbind(prices, Ticker)
      }
      prices$Date = as.Date(prices$Date, format="%Y-%m-%d")
      prices = prices[order(prices$Date),]
    }
    return (prices)
  })

  if(mergeAll == TRUE & return.type == "df") {
    merged = do.call(rbind, historicPrices.list)
    merged = merged[order(merged$Date),]
    if(nrow(merged) %% length(Symbols) != 0) {
      warning("Market data incomplete, some assets are missing days")
    }
    return (merged)
  } else {
    return (historicPrices.list)
  }

}

getHistoricDataAsMVResponse = function(Symbols, month1=NULL, day1=NULL,
  year1=NULL, month2=NULL, day2=NULL, year2=NULL, ndays=NULL) {

  market = getHistoricData(Symbols, month1, day1, year1, month2,
    day2, year2, ndays, return.type="df", mergeAll=TRUE)
  prices = matrix(market$Close, ncol=length(Symbols), byrow=T)
  colnames(prices) = Symbols
  return (prices)
}
