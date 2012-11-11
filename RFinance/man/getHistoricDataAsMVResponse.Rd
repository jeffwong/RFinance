\name{getHistoricDataAsMVResponse}
\alias{getHistoricDataAsMVResponse}
\title{
Download Historic Data as a Multivariate Time Series
}
\description{
Download historic data from Yahoo Finance and format it as a
multivariate time series
}
\usage{
getHistoricDataAsMVResponse(Symbols, month1 = NULL, day1 = NULL, year1 = NULL, month2 = NULL, day2 = NULL, year2 = NULL, ndays = NULL)
}
\arguments{
  \item{Symbols}{
A vector of symbols to download data for
}
  \item{month1}{
Beginning month
}
  \item{day1}{
Beginning day
}
  \item{year1}{
Beginning year
}
  \item{month2}{
Ending month
}
  \item{day2}{
Ending day
}
  \item{year2}{
Ending year
}
  \item{ndays}{
Alternative to providing  exact start dates and end dates.  Downloads
data for the past 'ndays'
}
}
\details{
Similar to getHistoricData but formats the data frame so that each column
is a time series for one specific asset's PRICE variable
}
\value{
A data frame containing price data only.  Each column is a time series
for one particular asset.
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
}
