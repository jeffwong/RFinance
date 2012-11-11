\name{OptimalTrades.BenchmarkDecisions}
\alias{OptimalTrades.BenchmarkDecisions}
\title{
Benchmark Trading Decisions with Optimal Decisions
}
\description{
Generate a confusion matrix to count times that myDecisions
agrees with optimalDecisions
}
\usage{
OptimalTrades.BenchmarkDecisions(prices, initFunds = 1000, numAssets, myDecisions)
}
\arguments{
  \item{prices}{
A vector of prices
}
  \item{initFunds}{
Initial funds that are available to the trader
}
  \item{numAssets}{
The number of unique assets that are represented in the prices vector
}
  \item{myDecisions}{
A vector of the same length as prices describing how many contracts were traded
at period t.  
}
}
\details{
Calls OptimalTrades and determines the optimal trading strategy.  Converts the
two decision vectors into vectors of factors, indicating a "Buy", "Sell", or
"Hold".  Creates a confusion matrix based on these two vectors of factors which
shows how many times I bought when I should have bought, sold when I should have
sold, and all occurrences of when my trading strategy differed from the optimal.
}
\value{
A table representing the confusion matrix.
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
}
