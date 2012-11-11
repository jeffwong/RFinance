#Checks to make sure that options have not expired
portfolio.load = function(filename="portfolio") {
  portfolio = read.csv(filename, header=T, sep=" ")
  portfolio$asset = as.character(portfolio$asset)
  portfolio$position = as.character(portfolio$position)
  portfolio$type = as.character(portfolio$type)
  portfolio$underlying = as.character(portfolio$underlying)

  options.indices = which(portfolio$type == "Option")
  today = Sys.Date()
  dates = do.call(c, sapply(portfolio$asset[options.indices], function(name) {
    date.start.index = regexpr("[A-Z][0-9]", name)[1] + 1
    date.end.index = regexpr("[0-9][CP]", name)[1]
    date = substr(name, date.start.index, date.end.index)
    year = paste("20", substr(date,1,2), "-", sep="")
    month = paste(substr(date,3,4), "-", sep="")
    day = substr(date,5,6)
    expiration = as.Date(paste(year, month, day, sep=""), format="%Y-%m-%d")
  },simplify=F))
  expired = which(dates < today)
  if(length(expired) > 0) {
    warning("Some options in your portfolio have expired.  Removing these now.")
    portfolio = portfolio[-options.indices[expired],]
  }

  return (portfolio)
}

portfolio.save = function(portfolio, filename="portfolio") {
  write.table(portfolio, file=filename, sep=" ", row.names = FALSE, col.names = TRUE)
}

#Creates an open order
portfolio.openOrder = function(portfolio, asset, quantity, position, type) {
  index = which(portfolio$asset == asset & portfolio$position == position)
  if(length(index) > 0) {
    if(position == "Long") {
      portfolio$quantity[index] = portfolio$quantity[index] + quantity
    }
    else {
      portfolio$quantity[index] = portfolio$quantity[index] - quantity
    }
  }
  else {
    portfolio = rbind(portfolio, data.frame(asset = asset, quantity = quantity,
      position = position, type = type, underlying = .portfolio.getUnderlying(asset, type)))
  }
  return (portfolio)
}

#Closes an open order
portfolio.closeOrder = function(portfolio, asset, quantity, position) {
  index = which(portfolio$asset == asset & portfolio$position == position)
  if(length(index) > 0) {
    remaining = 0
    if(position == "Long") {
      remaining = portfolio$quantity[index] - quantity
      if(remaining < 0) { stop("INVALID QUANTITY") }
    }
    else {
      remaining = portfolio$quantity[index] + quantity
      if(remaining > 0) { stop("INVALID QUANTITY") }
    }
    if(remaining == 0) { portfolio = portfolio[-index,] }
    else { portfolio$quantity[index] = remaining }
  }
  else { stop("NO ASSET FOUND") }
  return (portfolio)
}

portfolio.getRisks = function(portfolio, riskFreeRate=0) {
  symbols.unique = unique(portfolio$underlying)
  x = sapply(symbols.unique, function(symbol, portfolio, riskFreeRate) {
    indices = which(portfolio$underlying == symbol)
    assets = portfolio[indices,]
    risks = .getRisksX(assets, riskFreeRate)
    risks
  }, portfolio = portfolio, riskFreeRate = riskFreeRate)
  return (t(x))
}

portfolio.getValue = function(portfolio) {
  value = 0
  stock.indices = which(portfolio$type == "Stock")
  value = value + sum(getQuote(portfolio$asset[stock.indices])$Last * portfolio$quantity[stock.indices])
  
  option.indices = which(portfolio$type == "Option")
  option.values = sapply(option.indices, function(index, portfolio) {
    assetname = portfolio$asset[index]
    value = portfolio$quantity[index] * getOptionInfo(assetname)$data[[2]] * 100
    value
  }, portfolio = portfolio )
  return (value + sum(option.values))
}

#Gets the underlying based on the character vector asset
.portfolio.getUnderlying = function(asset, type) {
  if(type == "Option") {
    expiration.index = regexpr("[0-9]", asset)
    underlying = substr(asset, 1, expiration.index - 1)
    return (underlying)
  }
  else {
    return (asset)
  }
}

#assets is a data frame of the same underlying
.getRisksX = function(assets, riskFreeRate) {
  delta = 0
  gamma.opt = 0
  theta = 0
  rho = 0
  vega = 0
  for(i in 1:nrow(assets)) {
    if(assets$type[i] == "Stock") {
      delta = delta + assets$quantity[i]
    }
    else {
      optionChain = getOptionChain(assets$underlying[i])
      BS = blackScholes(optionChain, riskFreeRate = riskFreeRate)
      if(regexpr("[0-9]C", assets$asset[i]) != -1) {  #is call
        index = which(rownames(optionChain$calls) == assets$asset[i])
        delta = delta + assets$quantity[i] * BS$calls$delta.call[index] * 100
        gamma.opt = gamma.opt + assets$quantity[i] + BS$calls$gamma.call[index] * 100
        theta = theta + assets$quantity[i] + BS$calls$theta.call[index] * 100
        rho = rho + assets$quantity[i] + BS$calls$rho.call[index] * 100
        vega = vega + assets$quantity[i] + BS$calls$vega.call[index] * 100
      }
      else {  #is put
        index = which(rownames(optionChain$puts) == assets$asset[i])
        delta = delta + assets$quantity[i] * BS$puts$delta.put[index] * 100
        gamma.opt = gamma.opt + assets$quantity[i] + BS$puts$gamma.put[index] * 100
        theta = theta + assets$quantity[i] + BS$puts$theta.put[index] * 100
        rho = rho + assets$quantity[i] + BS$puts$rho.put[index] * 100
        vega = vega + assets$quantity[i] + BS$puts$vega.put[index] * 100
     }
    }
  }
  return (list(delta = delta, gamma = gamma.opt, vega = vega, theta = theta, rho = rho))
}
