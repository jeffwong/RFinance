#Polls getQuote every timeStep and rbinds the resulting
#data frames into 1 large data frame
monitor.quote = function(Symbols, timeStep=1, limit=10, filename) {

  quotes = getQuote(Symbols)
  for (i in 1:(limit-1)) {
    Sys.sleep(timeStep)
    quotes = rbind(quotes, getQuote(Symbols))
  }
  underlying = rep(Symbols, limit)
  quotes = cbind(quotes, underlying)
  if(!missing(filename)) {
    write.table(quotes, file=filename, sep=",", row.names = T, col.names = T) 
  }
  return (quotes)

}

#Polls getOptionChain every timeStep and rbinds the resulting
#data frames into 1 large data frame
monitor.optionChain = function(Symbols, timeStep=1, limit=10, filename) {

  optionChains = getOptionChain(Symbols)
  for (i in 1:(limit-1)) {
    Sys.sleep(timeStep)
    optionChain2 = getOptionChain(Symbols)
    optionChains$calls = rbind(optionChains$calls, optionChain2$calls)
    optionChains$puts = rbind(optionChains$puts, optionChain2$puts)
    if(!optionChain2$symbol %in% optionChains$symbol) {
      optionChains$symbol = c(optionChains$symbol, optionChain2$symbol)
    }
  }

  if(!missing(filename)) {
    write.table(optionChains, file=filename, sep=",", row.names = T, col.names = T) 
  }

  return (optionChains)

}
