\name{blackScholes}
\alias{blackScholes}
\title{
Black Scholes Pricing and 5 Greeks
}
\description{
Uses the Black Scholes formula to price options and calculate the five Greeks: delta, rho, gamma, theta, vega. 
}
\usage{
blackScholes(optionChain, stockPrice, volatility, riskFreeRate = 0, dividend = 0)
}
\arguments{
  \item{optionChain}{
An option chain to be priced
}
  \item{stockPrice}{

Underlying stock price.  If missing, RFinance will pull the current stock price from Yahoo Finance
}
  \item{volatility}{
Underlying volatility.  If missing, RFinance will use an optimization function to determine the implied volatility
}
  \item{riskFreeRate}{

The risk-free interest rate.  Currently 0.
}
  \item{dividend}{
The underlying dividend rate.  Default 0.
}
}
\details{
}
\value{
Returns the optionChain data frame with the five greeks and prices binded to the columns
}
\references{

}
\author{
Jeffrey Wong
}
\note{

}

\seealso{

}
\examples{
goog = getOptionChain("GOOG")
blackScholes(goog)
}

\keyword{ blackscholes }
\keyword{ options }
