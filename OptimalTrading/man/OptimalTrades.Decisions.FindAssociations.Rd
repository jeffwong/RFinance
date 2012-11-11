\name{OptimalTrades.Decisions.FindAssociations}
\alias{OptimalTrades.Decisions.FindAssociations}
\title{
Association Rule Analysis on Trading Strategy
}
\description{
Detect patterns of what assets are bought together or sold together
}
\usage{
OptimalTrades.Decisions.FindAssociations(decisions, numAssets, symbols, ...)
}
\arguments{
  \item{decisions}{
Decision vector of how many contracts are traded at period t
}
  \item{numAssets}{
The number of unique assets represented in the decisions vector
}
  \item{symbols}{
A character vector containing the names of the assets
}
  \item{\dots}{
Additional arguments that can be passed to the apriori function found in 
the 'arules' package
}
}
\details{
Objective: based on the trading strategy, either passed in by the trader or
computed by OptimalTrades, attempt to find patterns on what assets should be
bought together or sold together.

Converts the decision vector to a binary matrix with numAssets columns.
Matrix is filled with 1's whenever an asset is purchased/sold, and the resulting
matrix is passed to 'apriori'.
}
\value{
Return values are same as those found in 'apriori' from the 'arules' package.
  \item{buy.rules }{Rules describing how assets are purchased together}
  \item{sell.rules }{Rules describing how assets are sold together}
}
\references{
}
\author{
Jeffrey Wong
}
\note{
}

\section{Warning }{A warning will be raised if no rules could be detected}

\seealso{
arules, apriori
}
\examples{
}
