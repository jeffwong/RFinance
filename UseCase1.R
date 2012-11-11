require(RFinance)
require(OptimalTrading)

ndays = 10
initFunds = 10^7

Symbols = c("GOOG", "AAPL", "NFLX", "AMZN", "V", "BA", "JPM", "MSFT",
  "SPY", "RHT", "BBBY", "CSCO", "BAC")
market = getHistoricData(Symbols, ndays=ndays, return.type="df", mergeAll=T)

#No risk considered
system.time(opt <- OptimalTrades(market$Close, initFunds=initFunds, numAssets=length(Symbols)))
OptimalTrades.Decisions.plot(opt$decisions, length(Symbols), Symbols)

#Risk considered
set.seed(100)
n = length(market$Close)
risks = abs(rnorm(n))
riskTol = initFunds

system.time(opt <- OptimalTrades(market$Close, risks, riskTol, initFunds, length(Symbols)))
OptimalTrades.Decisions.plot(opt$decisions, length(Symbols), Symbols)

#Find rules
rules = OptimalTrades.Decisions.FindAssociations(opt$decisions, length(Symbols), Symbols, parameter=list(supp=0.5, conf=0.9, target="rules"))
inspect(rules$buy.rules)
inspect(rules$sell.rules)