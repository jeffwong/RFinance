#Retrieves historic data and merges it into 1 data frame,
#representing the market.  Passes this data to a strategy
#function, which generates a quantity vector.
#An action is either buy, hold, or sell.  
#Computes profits at the end.

backTest = function(Symbols, historic, ndays=50, strategy=.strategy.sample, funds=100000, ...) {
  if(missing(historic)) {
    historic = getHistoricData(Symbols, ndays=ndays, return.type="df", appendSymbol=TRUE, mergeAll=TRUE)
  } else {
    #historic data retrieved from monitor.R
    #or data frame passed in by external source
    colnames(historic)[1] = "TradeTime"
    colnames(historic)[2] = "Close"
    historic = historic[order(historic$underlying, historic$TradeTime),]
  }
  #Calculate Optimal Response
  opt = OptimalTrades(historic$Close, initFunds=funds, numAssets=length(Symbols))
  #Calculate response from strategy
  response = strategy(historic, funds, ...)
  percentofmax = response$profits / opt$profits
  return (list(historic=historic, optimal=opt, mystrategy=response, percentofmax=percentofmax))
}

.strategy.sample = function(historic, funds=100000) {
  price.mean = mean(historic$Close)
  quantity = rep(0, nrow(historic))
  above.mean = which(historic$Close > price.mean)
  quantity[above.mean] = -100
  quantity[-above.mean] = 100

  return(list(quantity=quantity, profits=-(quantity%*%historic$Close)[1,1]))
}
