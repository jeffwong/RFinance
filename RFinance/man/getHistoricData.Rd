\name{getHistoricData}
\alias{getHistoricData}
\title{
Retrieve Historic Data
}
\description{
Downloads stock information from  Yahoo Finance.  Is capable of doing batch downloads to pull information on multiple symbols.
}
\usage{
getHistoricData(Symbols, month1, day1, year1, month2, day2, year2, ndays = NULL, return.type = "xts", appendSymbol = FALSE, mergeAll = FALSE)
}
\arguments{
  \item{Symbols}{
A vector of stock symbols
}
  \item{month1}{
An optional argument to download data starting on year1 - month1 - day1.  January is coded as month 0, February as month 1...
}
  \item{day1}{
An optional argument to download data starting on year1 - month1 - day1
}
  \item{year1}{
An optional argument to download data starting on year1 - month1 - day1.  Years are coded using 4 digits, i.e. 2011, not 11
}
  \item{month2}{
An optional argument to download data ending on year2 - month2 - day2.  January is coded as month 0, February as month 1...
}
  \item{day2}{
An optional argument to download data ending on year2 - month2 - day2
}
  \item{year2}{
An optional argument to download data ending on year2 - month2 - day2.  Years are coded using 4 digits, i.e. 2011, not 11
}
  \item{ndays}{
An optional argument to download data starting ndays ago until now
}
  \item{return.type}{
"df" for a regular data frame, "xts", for an extensible time series object.  xts objects are using in the "chart" function
}
  \item{appendSymbol}{
Boolean.  True if RFinance should append the stock symbol to the data frame returned by Yahoo Finance.  This is helpful for batch downloading.
}
  \item{mergeAll}{
Boolean.  getHistoricData returns a list, where each component is a data frame representing the historic data for that stock.  If mergeAll is set to true, the data frames are binded and returned as 1 data frame.
}
}
\details{
}
\value{
The return type is determined by the variable "return.type".

xts objects are used in the chart function
data frames are used in the backTest function
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
goog = getHistoricData("GOOG", nday=50, return.type="df")
getHistoricData("GOOG", 2, 10, 2011, 2, 15, 2011)
stocks = getHistoricData(c("GOOG", "AAPL"), ndays=5, return.type="df", mergeAll=TRUE, appendSymbol=TRUE)
#Stocks must be ordered a certain way for back-testing to work
stocks = stocks[order(stocks$Date, stocks$Ticker),]
}
\keyword{ historic data }
\keyword{ yahoo finance }
