\name{getOptimalActions}
\alias{getOptimalActions}
\title{
Optimal Trading Actions
}
\description{
Based on historic data, finds the optimal set of trading actions to maximize profits.
}
\usage{
getOptimalActions(Historic, initFunds = 1e+05)
}
\arguments{
  \item{Historic}{
Historic data, typically retrieved by RFinance's 'getHistoricData' function
}
  \item{initFunds}{
Amount of money available to trade with
}
}
\details{
Uses convex optimization to solve a linear program with the objective of maximizing returns.  Constraints are imposed to limit trading to the amount of funds available specified by initFunds.  Short selling is not supported.  
}
\value{
A list containing two components
\item{action }{A vector of BUY SELL or HOLD.  Describes the action that should be taken on that time interval}
\item{quantity }{A vector describing how much should be traded on that time interval}
}
\references{
}
\author{
Jeffrey Wong
}
\note{
}

\seealso{
backTest
}
\examples{
  #Download historic data.  appendSymbol MUST be set to TRUE to distinguish multiple time series.  mergeAll must also be set to TRUE to combine all data into 1 df
  stocks = getHistoricData("AAPL", ndays=50, return.type="df", mergeAll=TRUE, appendSymbol=TRUE)
  getOptimalActions(stocks)
}
