\name{RFinance-package}
\alias{RFinance-package}
\alias{RFinance}
\docType{package}
\title{
RFinance

A package that provides tools for financial analysts
}
\description{
Maintains a virtual portfolio.  Provides functions to look up current stock prices and option prices.  Download historic data and chart an assets price with its volume.  Calculate option prices and the five greeks.  Test a strategy using a back-testing engine.  Calculate the minimum variance portfolio.

Built on top of the R package 'quantmod'
}
\details{
\tabular{ll}{
Package: \tab RFinance\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2011-03-12\cr
License: \tab What license is it under?\cr
LazyLoad: \tab yes\cr
}

getQuote - get a real time quote on a particular stock
getOptionInfo - get a real time quote on a particular option
chart - plot a stock's price over time
backTest - back test a particular trading strategy and compare its performance to the optimal one
getHistoricData - access Yahoo's API to download financial data on a set of stocks
blackScholes - calculate Black Scholes prices and 5 greeks
getVolatility - calculate implied volatility for a particular option chain
michaudResampleWeight - calculate the minimum variance portfolio that achieves a particular return rate
portfolio.openOrder, portfolio.closeOrder - buy and sell assets from your portfolio
portfolio.getRisks - show all risks associated with your portfolio
portfolio.getValue - compute the value of your portfolio

}
\author{
Jeffrey Wong

Maintainer: <jeffwong@stanford.edu>
}
\references{

}
\keyword{ package }
\seealso{
quantmod
}
\examples{

}
