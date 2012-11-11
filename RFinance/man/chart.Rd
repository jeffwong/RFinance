\name{chart}
\alias{chart}
\title{
Chart stock prices
}
\description{
An extension of quantmod's chartSeries function.  User provides a set of symbols to be charted.  Prices are retrieved from Yahoo Finance and passed to quantmod's chartSeries function.  Batch charting can be accomplished by writing charts to a file.
}
\usage{
chart(Symbols, month1, day1, year1, month2, day2, year2, ndays = NULL, save.file = NULL, width = 4, height = 3)
}
\arguments{
  \item{Symbols}{
A vector of stock symbols to chart
}
  \item{month1}{
An optional argument to plot stock prices starting on year1 - month1 - day1
}
  \item{day1}{
An optional argument to plot stock prices starting on year1 - month1 - day1
}
  \item{year1}{
An optional argument to plot stock prices starting on year1 - month1 - day1
}
  \item{month2}{
An optional argument to plot stock prices ending on year2 - month2 - day2
}
  \item{day2}{
An optional argument to plot stock prices ending on year2 - month2 - day2
}
  \item{year2}{
An optional argument to plot stock prices ending on year2 - month2 - day2
}
  \item{ndays}{
An optional argument to plot stock prices starting ndays ago until now
}
  \item{save.file}{
An optional argument.  If specified, plots will be saved to save.file.
}
  \item{width}{
An optional argument to specify the width of the plot.
}
  \item{height}{
An optional argument to specify the height of the plot.
}
}
\details{

}
\value{

}
\references{
quantmod
}
\author{
Jeffrey Wong
}
\note{
}

\seealso{
chartSeries
}
\examples{
#Plot GOOG starting March 2, 2011 to March 5, 2011
chart("GOOG", 02, 02, 2011, 02, 05, 2011)
chart("AAPL", ndays=50)
}
\keyword{ chart }
\keyword{ plot }
