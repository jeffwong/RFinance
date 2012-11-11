\name{portfolio.openOrder}
\alias{portfolio.openOrder}

\title{
Portfolio: open order
}
\description{

Creates an open order for an asset and adds it to the portfolio
}
\usage{
portfolio.openOrder(portfolio, asset, quantity, position, type)
}

\arguments{
  \item{portfolio}{

A data frame representing the portfolio.  Usually kept in a text file and read in
}
  \item{asset}{

The name of the asset that should be open
}
  \item{quantity}{

The number of contracts to open
}
  \item{position}{

A character vector, either "Long" or "Short".
}
  \item{type}{

A character vector, either "Stock" or "Option"
}
}
\details{

}
\value{
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
portfolio = portfolio.openOrder(portfolio, "GOOG", 200, "Long", "Stock")
}
