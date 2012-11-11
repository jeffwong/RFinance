\name{OptimalTrades.Prices.plot}
\alias{OptimalTrades.Prices.plot}
\title{
Plot Prices as Heatmap
}
\description{
Plot Prices as Heatmap
}
\usage{
OptimalTrades.Prices.plot(prices, numAssets, symbols)
}
\arguments{
  \item{prices}{
A vector of prices
}
  \item{numAssets}{
The number of unique assets represented in prices
}
  \item{symbols}{
A character vector containing the names of the assets
}
}
\details{
Converts the prices vector to a matrix where each column represents a different
asset.  The matrix is standardized to have mean 0 and variance 1.  Plots the
heatmap of the matrix, where colors are used to represent whether the asset has
a relatively high price or relatively low price.
}
\value{
The standardized price matrix
}
\references{
}
\author{
Jeffrey Wong
}
\note{
}

\seealso{
OptimalTrades.Decisions.plot, heatmap_2
}
\examples{
}
