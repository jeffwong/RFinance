#Prices a quantmod formatted option chain
#delta - sensitivity of option to movement in stockPrice
#theta - sensitivity of option to time
#rho - sensitivity of option to movement in risk free rate
#gamma - sensitivity of delta to movement in stockPrice
#vega - sensitivity of option to movement in volatility

#optionChain is a single option chain of calls and puts
blackScholes = function(optionChain, stockPrice, volatility, riskFreeRate = 0, dividend=0) {
  if(is.null(optionChain$expiration) | is.null(optionChain$Time.remaining)) {
    optionChain = .blackScholesX(optionChain)
  }
  if(missing(stockPrice)) {
    underlying = .getUnderlying(rownames(optionChain$calls)[1])
    stockPrice = getQuote(underlying)$Last
  }
  if(missing(volatility)) {
    volatility = getVolatility(optionChain, stockPrice, riskFreeRate, dividend)
  }

  calls = optionChain$calls
  puts = optionChain$puts

  m = nrow(calls)
  n = nrow(puts)

  volatility.calls = volatility$volatility.calls
  volatility.puts = volatility$volatility.puts

  time.remaining = as.double(calls$Time.remaining)
  d1.top = log(stockPrice / calls$Strike) + (riskFreeRate - dividend + volatility.calls^2/2) * time.remaining
  d1.bottom = volatility.calls * sqrt(time.remaining)
  d1 = d1.top / d1.bottom
  d2 = d1 - volatility.calls * sqrt(time.remaining)

  calls.price = exp(-dividend*time.remaining)*(pnorm(d1) * stockPrice) - (pnorm(d2) * calls$Strike * exp(-riskFreeRate*time.remaining))
  delta.call = exp(-dividend*time.remaining)*pnorm(d1)
  theta.call = -exp(-dividend*time.remaining)*(stockPrice * .pnorm.deriv(d1) * volatility.calls / (2*sqrt(time.remaining))) -
    (riskFreeRate*calls$Strike*exp(-riskFreeRate*time.remaining)*pnorm(-d2)) + 
    dividend*stockPrice*exp(-dividend*time.remaining)*pnorm(d1)
  vega.call = stockPrice * exp(-dividend*time.remaining) * .pnorm.deriv(d1) * sqrt(time.remaining)
  gamma.call = exp(-dividend*time.remaining) * .pnorm.deriv(d1) / (stockPrice * volatility.calls * sqrt(time.remaining))
  rho.call = calls$Strike * time.remaining * exp(-riskFreeRate * time.remaining) * pnorm(d2)

  time.remaining = as.double(puts$Time.remaining)
  d1.top = log(stockPrice / puts$Strike) + (riskFreeRate - dividend + volatility.puts^2/2) * time.remaining
  d1.bottom = volatility.puts * sqrt(time.remaining)
  d1 = d1.top / d1.bottom
  d2 = d1 - volatility.puts * sqrt(time.remaining)
  
  puts.price = exp(-riskFreeRate*time.remaining)*puts$Strike*pnorm(-d2) - exp(-dividend*time.remaining)*stockPrice*pnorm(-d1)
  delta.put = -exp(-dividend*time.remaining)*pnorm(-d1)
  theta.put = -exp(-dividend*time.remaining)*(stockPrice * .pnorm.deriv(d1) * volatility.puts / (2*sqrt(time.remaining))) + 
    (riskFreeRate*puts$Strike*exp(-riskFreeRate*time.remaining)*pnorm(-d2)) - 
    dividend*stockPrice*exp(-dividend*time.remaining)*pnorm(-d1)
  vega.put = stockPrice * exp(-dividend*time.remaining) * .pnorm.deriv(d1) * sqrt(time.remaining)
  gamma.put = exp(-dividend*time.remaining) * .pnorm.deriv(d1) / (stockPrice * volatility.puts * sqrt(time.remaining))
  rho.put = -puts$Strike * time.remaining * exp(-riskFreeRate * time.remaining) * pnorm(-d2)

  optionChain$calls = cbind(optionChain$calls, calls.price, delta.call, vega.call, theta.call, rho.call, gamma.call)
  optionChain$puts = cbind(optionChain$puts, puts.price, delta.put, vega.put, theta.put, rho.put, gamma.put)

  return (optionChain)
  
  #return (list(callPrice = calls.price, putPrice = puts.price, 
  #  delta.call = delta.call, delta.put = delta.put,
  #  vega.call = vega.call, vega.put = vega.put,
  #  theta.call = theta.call, theta.put = theta.put,
  #  rho.call = rho.call, rho.put = rho.put,
  #  gamma.call = gamma.call, gamma.put = gamma.put))
}

#Pre-process the option chain
#Add expiration dates and time till expiry
.blackScholesX = function(optionChain) {
  expiration = rep(as.POSIXlt(Sys.Date()), nrow(optionChain$calls))
  expiration.indices = regexpr("[0-9]",rownames(optionChain$calls))
  for (i in 1:length(expiration.indices)) {
    expiration.text = substr(rownames(optionChain$calls)[i], expiration.indices[i], expiration.indices[i] + 5)
    expiration[i] = as.POSIXlt(as.Date(expiration.text, format="%y%m%d"))
  }
  Time.remaining = rep(0, nrow(optionChain$calls))
  today = as.POSIXlt(Sys.Date())
  Time.remaining = as.numeric(as.Date(expiration) - as.Date(today))
  optionChain$calls = data.frame(optionChain$calls, expiration, Time.remaining)
  
  expiration = rep(as.POSIXlt(Sys.Date()), nrow(optionChain$puts))
  expiration.indices = regexpr("[0-9]",rownames(optionChain$puts))
  for (i in 1:length(expiration.indices)) {
    expiration.text = substr(rownames(optionChain$puts)[i], expiration.indices[i], expiration.indices[i] + 5)
    expiration[i] = as.POSIXlt(as.Date(expiration.text, format="%y%m%d"))
  }
  Time.remaining = rep(0, nrow(optionChain$puts))
  today = as.POSIXlt(Sys.Date())
  Time.remaining = as.numeric(as.Date(expiration) - as.Date(today))
  optionChain$puts = data.frame(optionChain$puts, expiration, Time.remaining)

  return (optionChain)
}

volatility.smile.plot = function(optionChain) {
  vol = getVolatility(optionChain)
  par(ask=T)
  plot(as.data.frame(optionChain$calls)$Strike, vol$volatility.calls, 
    xlab='Strike', ylab='Volatility', main='Volatility Smile for Calls')
  plot(as.data.frame(optionChain$puts)$Strike, vol$volatility.calls, 
    xlab='Strike', ylab='Volatility', main='Volatility Smile for Calls')
}

#Requires option chains from multiple months
#Retrieved from getHighFrequencyData(symbol) or getOptionChain(symbol, Exp=NULL)
volatility.surface.plot = function(fullOptionChain) {
  fullOptionChain = .blackScholesX(fullOptionChain)
  volatility = getVolatility(fullOptionChain)
  calls = fullOptionChain$calls
  puts = fullOptionChain$puts

  par(ask=T)
  scatterplot3d(calls$Strike, calls$Time.remaining, volatility$calls)
  scatterplot3d(puts$Strike, puts$Time.remaining, volatility$puts)
}

#Optimization function to determine 
#Implied Volatility
getVolatility = function(optionChain, stockPrice, riskFreeRate = 0, dividend = 0, lower=0, upper=1) {
  if(is.null(optionChain$expiration) | is.null(optionChain$Time.remaining)) {
    optionChain = .blackScholesX(optionChain)
  }
  if(missing(stockPrice)) {
    underlying = .getUnderlying(rownames(optionChain$calls)[1])
    stockPrice = getQuote(underlying)$Last
  }

  calls = optionChain$calls
  puts = optionChain$puts

  m = nrow(calls)
  n = nrow(puts)

  volatility.calls = rep(0, m)
  se.calls = rep(0,m)
  volatility.puts = rep(0, n)
  se.puts = rep(0,n)

  for(i in 1:m) {
    opt = optimize(.volatility.optimizecall.objfn, c(lower, upper), time.remaining=calls$Time.remaining[i], 
      stockPrice = stockPrice, dividend=dividend, strike=calls$Strike[i], riskFreeRate=riskFreeRate, 
      truevalue=calls$Last[i], maximum=F)
    volatility.calls[i] = opt$minimum
    se.calls[i] = opt$objective
  }
  for(i in 1:n) {
    opt = optimize(.volatility.optimizeput.objfn, interval=c(lower,upper), time.remaining=puts$Time.remaining[i], 
      stockPrice = stockPrice, dividend=dividend, strike=puts$Strike[i], riskFreeRate=riskFreeRate, 
      truevalue=puts$Last[i])
    volatility.puts[i] = opt$minimum
    se.puts[i] = opt$objective
  }

  return ( list(volatility.calls=volatility.calls,
    volatility.puts=volatility.puts,
    se.calls=se.calls,
    se.puts=se.puts
  ))
}

.volatility.optimizecall.objfn = function(volatility, time.remaining, stockPrice, dividend, strike, riskFreeRate, truevalue) {
  call.price = .priceCall(volatility, time.remaining, stockPrice, dividend, strike, riskFreeRate)
  return ((truevalue - call.price)^2)
}

.volatility.optimizeput.objfn = function(volatility, time.remaining, stockPrice, dividend, strike, riskFreeRate, truevalue) {
  put.price = .pricePut(volatility, time.remaining, stockPrice, dividend, strike, riskFreeRate)
  return ((truevalue - put.price)^2)
}

#Derivative of a N(0,1) distribution
.pnorm.deriv = function(x) {
  return (exp(-(x^2)/2) / sqrt(2*pi))
}

.priceCall = function(volatility, time.remaining, stockPrice, dividend, strike, riskFreeRate) {
  d1.top = log(stockPrice / strike) + (riskFreeRate - dividend + volatility^2/2) * time.remaining
  d1.bottom = volatility * sqrt(time.remaining)
  d1 = d1.top / d1.bottom
  d2 = d1 - volatility * sqrt(time.remaining)

  calls.price = exp(-dividend*time.remaining)*(pnorm(d1) * stockPrice) - 
    (pnorm(d2) * strike * exp(-riskFreeRate*time.remaining))
  delta.call = exp(-dividend*time.remaining)*pnorm(d1)
  theta.call = -exp(-dividend*time.remaining)*(stockPrice * .pnorm.deriv(d1) * volatility / (2*sqrt(time.remaining))) - 
    (riskFreeRate*strike*exp(-riskFreeRate*time.remaining)*pnorm(-d2)) + 
    dividend*stockPrice*exp(-dividend*time.remaining)*pnorm(d1)
  vega.call = stockPrice * exp(-dividend*time.remaining) * .pnorm.deriv(d1) * sqrt(time.remaining)
  gamma.call = exp(-dividend*time.remaining) * .pnorm.deriv(d1) / (stockPrice * volatility * sqrt(time.remaining))
  rho.call = strike * time.remaining * exp(-riskFreeRate * time.remaining) * pnorm(d2)

  calls.price
}

.pricePut = function(volatility, time.remaining, stockPrice, dividend, strike, riskFreeRate) {
  d1.top = log(stockPrice / strike) + (riskFreeRate - dividend + volatility^2/2) * time.remaining
  d1.bottom = volatility * sqrt(time.remaining)
  d1 = d1.top / d1.bottom
  d2 = d1 - volatility * sqrt(time.remaining)
  
  puts.price = exp(-riskFreeRate*time.remaining)*strike*pnorm(-d2) - exp(-dividend*time.remaining)*stockPrice*pnorm(-d1)
  delta.put = -exp(-dividend*time.remaining)*pnorm(-d1)
  theta.put = -exp(-dividend*time.remaining)*(stockPrice * .pnorm.deriv(d1) * volatility / (2*sqrt(time.remaining))) + 
    (riskFreeRate*strike*exp(-riskFreeRate*time.remaining)*pnorm(-d2)) - 
    dividend*stockPrice*exp(-dividend*time.remaining)*pnorm(-d1)
  vega.put = stockPrice * exp(-dividend*time.remaining) * .pnorm.deriv(d1) * sqrt(time.remaining)
  gamma.put = exp(-dividend*time.remaining) * .pnorm.deriv(d1) / (stockPrice * volatility * sqrt(time.remaining))
  rho.put = -strike * time.remaining * exp(-riskFreeRate * time.remaining) * pnorm(-d2) 

  puts.price
}
