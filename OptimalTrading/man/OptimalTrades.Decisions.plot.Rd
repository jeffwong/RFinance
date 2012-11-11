\name{OptimalTrades.Decisions.plot}
\alias{OptimalTrades.Decisions.plot}
\title{
Visualize BUY and SELL orders
}
\description{
Generate a heatmap representing BUY/SELL orders
}
\usage{
OptimalTrades.Decisions.plot(decisions, numAssets, symbols)
}
\arguments{
  \item{decisions}{
A vector describing the number of contracts bought or sold at period t
}
  \item{numAssets}{
The number of unique assets represented in the decisions vector
}
  \item{symbols}{
A character vector containing the names of the assets
}
}
\details{
Converts the decisions vector to a binary matrix representing a BUY order
or a SELL order.  Plot the heatmap of the matrix.  Each column is a unique asset
and the y-axis is time (recent trades at the bottom).  The color of the block
represents a buy or sell.  A value of 1 or -1 represents insignificant buys
and sells, which may be caused by numerical instability in the optimization.
Values of 2 or -2 represent significant buys and sells.
}
\value{
  \item{decisions.matrix }{The matrix of BUY/SELL orders}
  \item{decisions.clean }{The decisions vector where any value within 0.001 of 0
    is reset to 0}
}
\references{
}
\author{
Jeffrey Wong
}
\note{
}

\seealso{
heatmap_2
}
\examples{
}
