\name{portfolio.load}
\alias{portfolio.load}
\title{
Load a portfolio
}
\description{
Load a portfolio from a text file
}
\usage{
portfolio.load(filename = "portfolio")
}

\arguments{
  \item{filename}{
Text file containing the portfolio.
}
}
\details{

}
\value{
}
\references{

}

\section{Warning }{A warning will be raised if the user loads a portfolio
containing expired options.  The expired options will be removed
from the portfolio.
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
}
