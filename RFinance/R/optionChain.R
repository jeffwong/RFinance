#Return information on an option with particular
#symbol, type and strike
#Can be extracted from the option name
getOptionInfo = function(assetname, symbol, type, strike) {
  if(!missing(assetname)) {
    symbol = .getUnderlying(assetname)
    if(.isCall(assetname)) { 
      type = "call" 
    } else { 
      type = "put" 
    }
    strike = .getStrike(assetname)
  }
  chain = getOptionChain(symbol)
  if(type == "call") {
    index = which(as.data.frame(chain$calls)$Strike == strike)
    assetname = rownames(as.data.frame(chain$calls))[index]
    return (list(assetname = assetname, data = chain$calls[index,]))
  }
  else {
    index = which(as.data.frame(chain$puts)$Strike == strike)
    assetname = rownames(as.data.frame(chain$puts))[index]
    return (list(assetname = assetname, data = chain$puts[index,]))
  }
}

.getUnderlying = function(assetname) {
  expiration.index = regexpr("[0-9]", assetname)
  underlying = substr(assetname, 1, expiration.index - 1)
  return (underlying)
}

.getStrike = function(assetname) {
  return (as.numeric(substr(assetname, nchar(assetname) - 5, nchar(assetname) - 3)))
}

.isCall = function(assetname) {
  return (regexpr("[0-9]C", assetname) != -1)
}

.isPut = function(assetname) {
  return (regexpr("[0-9]P", assetname) != -1)
}
