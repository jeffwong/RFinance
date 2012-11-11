\name{getVolatility}
\alias{getVolatility}
\title{
Get Implied Volatility of an Option
}
\description{
Calculates the implied volatility for an option
}
\usage{
getVolatility(optionChain, stockPrice, riskFreeRate = 0, dividend = 0, lower = 0, upper = 10)
}
\arguments{
  \item{optionChain}{
An option chain retrieved by the function 'getOptionChain'
}
  \item{stockPrice}{
The underlying stock price.  If missing, RFinance will pull the current stock price.
}
  \item{riskFreeRate}{
The current risk-free interest rate.  Default 0.
}
  \item{dividend}{
The underlying dividend rate.  Default 0.
}
  \item{lower}{
Optional optimization parameters.  If specified, looks for a volatility value between lower and upper.
}
  \item{upper}{
Optional optimization parameters.  If specified, looks for a volatility value between lower and upper.
}
}
\details{
Uses R's optimize function to determine a value for volatility that will minimize
the sum of squared errors between the predicted Black Scholes option prices and 
the actual option prices provided in optionChain.
}
\value{
\item{volatility.calls }{A vector of volatilities for the call options}
\item{volatility.puts }{A vector of volatilities for the put options}
\item{se.calls }{The squared errors when computing call volatility}
\item{se.puts }{The squared errors when computing put volatility}
}
\references{
}
\author{
Jeffrey Wong
}
\note{
}

\seealso{
blackScholes
}
\examples{
#Must use a quantmod formatted option chain
aapl = getOptionChain("AAPL")
getVolatility(aapl)
}

\keyword{ optimization }
