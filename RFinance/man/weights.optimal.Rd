\name{weights.optimal}
\alias{weights.optimal}

\title{
Optimal portfolio weights
}
\description{

Determines the optimal weights for a portfolio to achieve a target return rate
}
\usage{
weights.optimal(returns, target)
}

\arguments{
  \item{returns}{

A data frame where each column represents the return rates over time for a particular asset
}
  \item{target}{

The desired return rate
}
}
\details{

A version using bootstrapping is michaudResampledWeight
}
\value{
A vector representing the quantity of each asset the trader should purchase in order to achieve the target return rate and minimum variance
}
\references{
Professor Tze Leung Lai, Statistics, Stanford University
}
\author{
Jeffrey Wong
}
\note{
}

\seealso{
michaudResampledWeight
}
\examples{
data(stats240stocks)
weights.optimal(stats240stocks, 0.5)
}
