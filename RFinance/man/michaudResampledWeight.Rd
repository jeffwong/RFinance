\name{michaudResampledWeight}
\alias{michaudResampledWeight}
\title{
Michaud Resampled Weight vector
}
\description{
Determines the portfolio that achieves a target return rate and minimizes variance.  Uses Michaud resampling to determine the portfolio.  This function may take several minutes to calculate.   
}
\usage{
michaudResampledWeight(returns, target, num.bootstraps = 500)
}
\arguments{
  \item{returns}{
A data frame where each column represents the return rates over time for a particular asset
}
  \item{target}{
The desired return rate
}
  \item{num.bootstraps}{
Number of bootstraps to use for Michaud resampling
}
}
\details{
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
weights.optimal
}
\examples{
data(stats240stocks)
michaudResampledWeight(stats240stocks, 0.05, num.bootstraps = 2)
}
\keyword{ bootstrap }
\keyword{ minimum-variance }
