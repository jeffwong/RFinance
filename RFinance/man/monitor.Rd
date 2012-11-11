\name{monitor}
\alias{monitor}
\alias{monitor.optionChain}
\alias{monitor.quote}
\title{
Monitor Stocks and Options
}
\description{
Monitors stock information and option information throughout the trading day
}
\usage{
monitor.optionChain(Symbols, timeStep = 1, limit = 10, filename)
monitor.quote(Symbols, timeStep = 1, limit = 10, filename)
}
\arguments{
  \item{Symbols}{
A vector of symbols to monitor.  monitor.optionChain only has support for monitoring 1 option chain at a time.  
}
  \item{timeStep}{
How frequent RFinance should pull information.  Represented in number of seconds
}
  \item{limit}{
How many times RFinance should pull information
}
  \item{filename}{
An optional argument.  If filename is not missing, the data frame of stock and option information will be written to filename
}
}
\details{
}
\value{
  A data frame akin to getQuote or getOptionChain
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
monitor.quote("AAPL", timeStep = 3, limit = 3)
#monitor.optionChain can only support 1 option chain at a time due to limitations in quantmod
monitor.optionChain("AAPL", timeStep = 3, limit = 3)
}
