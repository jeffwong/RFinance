\name{OptimalTrades}
\alias{OptimalTrades}
\title{
Compute Optimal Trading Strategy
}
\description{
Compute Optimal Trading Strategy from Historic Prices
}
\usage{
OptimalTrades(prices, risks = NULL, riskTol = 1000, initFunds = 1000, numAssets)
}
\arguments{
  \item{prices}{
A vector representing prices of different assets
}
  \item{risks}{
Optional.  The risk associated with each asset at time t.  See details for more.
}
  \item{riskTol}{
Optional.  Should be supplied if risks is supplied.  The amount of risk for each
asset that the trader is willing to be exposed to.  
}
  \item{initFunds}{
Initial funds available to the trader
}
  \item{numAssets}{
The number of unique assets represented in prices
}
}
\details{
Uses optimization to find the optimal trading strategy 
(min profits=dot(prices, quantity)) based on a dataset of historical prices.  
No short selling is allowed.  Trader is restricted to his initial funds and 
any extra funds that become available through trading (profits).

Optional: user can provide a set of risks associated with each trade.  The
objective function is then set to min dot(prices,quantity) - 
0.5 dot(quantity, risks * quantity).  This function helps to enforce
a diversified portfolio, as there is a penalty for buying large quantities of
a particular asset.  The riskTol parameter adds constraints to the program so
that the trader is never exposed to more than riskTol units of risk from any
one particular asset.  Note: this constraint is not for the total amount of risk,
but for the amount of risk per asset.
}
\value{
  \item{status }{The return status of the optimization.  0 if successful}
  \item{decisions }{The optimal decisions}
  \item{profits }{The amount of profit obtained}
}
\references{
}
\author{
Jeffrey Wong
}
\note{
}

\seealso{
OptimalTrades.Decisions.plot, OptimalTrades.Decisions.FindAssociations
}
\examples{
}
