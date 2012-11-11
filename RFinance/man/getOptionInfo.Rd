\name{getOptionInfo}
\alias{getOptionInfo}
\title{
Option Information
}
\description{
Akin to getQuote.  Returns current information on a particular option symbol
}
\usage{
getOptionInfo(assetname, symbol, type, strike)
}
\arguments{
  \item{assetname}{
The option symbol to look up
}
  \item{symbol}{
Optional argument.  If assetname is missing, it is possible to specify the underlying symbol, option type(call or put), and the desired strike.
}
  \item{type}{
Optional argument.  If assetname is missing, it is possible to specify the underlying symbol, option type(call or put), and the desired strike.
}
  \item{strike}{
Optional argument.  If assetname is missing, it is possible to specify the underlying symbol, option type(call or put), and the desired strike.
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
getQuote
}
\examples{
getOptionInfo(symbol = "GOOG", type = "call", strike = 600)
}
