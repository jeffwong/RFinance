\name{portfolio.closeOrder}
\alias{portfolio.closeOrder}
\title{
Close an order
}
\description{
Closes an open order in a portfolio
}
\usage{
portfolio.closeOrder(portfolio, asset, quantity, position)
}
\arguments{
  \item{portfolio}{
A data frame representing the portfolio.  Usually kept in a text file and read in
}
  \item{asset}{
The name of the asset that should be closed
}
  \item{quantity}{
The number of contracts to close
}
  \item{position}{
A character vector, either "Long" or "Short".
}
}
\details{
}
\value{
Returns the updated portfolio
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
data(portfolio)
portfolio = portfolio.closeOrder(portfolio, "GOOG", 200, "Long")
portfolio = portfolio.closeOrder(portfolio, "AMZN", 200, "Short")
portfolio = portfolio.closeOrder(portfolio, "GOOG110319C00240000", 1, "Long")
}
