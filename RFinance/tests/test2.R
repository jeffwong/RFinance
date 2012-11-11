#.onLoad = function(libname, pkgname) {
#  .libPaths("~jeffwong/RLibrary")
#  require(RFinance)
#}
#.onLoad()

#require(RFinance, lib="~jeffwong/RLibrary")

require(RFinance)

#Let's load some sample data
data(portfolio)
data(watchlist)

#What is my portfolio valued at?
portfolio.getValue(portfolio)

#What risks am I exposed to?
portfolio.getRisks(portfolio)

#How are the stocks doing today?
getQuote(watchlist$symbol)

#How is the Nikkei ETF doing?
getQuote("EWJ")

#EWJ looks like a good buy right now.  Let's place an order
portfolio = portfolio.openOrder(portfolio, "EWJ", 100, "Long", "Stock")

#Need some cash, let's sell GOOG
portfolio = portfolio.closeOrder(portfolio, "GOOG", 100, "Long")

print(portfolio)

#Consider getting a put option on GOOG
getOptionInfo(symbol = "GOOG", type = "put", strike = 600)

#Compare AAPL and GOOG
chart(c("AAPL", "GOOG"), ndays=10)

#What is the implied volatility for NFLX?
nflx = getOptionChain("NFLX")
getVolatility(nflx, verbose=F)

#How would my trading strategy have performed on my watchlist?
backTest("AAPL", ndays=10)

#Let's gather some data on returns over some popular stocks
data(stats240stocks)

#I want a portfolio of these stocks and on average have a return of 10%
#What should my portfolio consist of to have minimum variance?

weights.optimal(stats240stocks, 0.1)

#Fundamental Analysis
getFinancials("GOOG")
viewFinancials(GOOG.f, "IS", "Q")

