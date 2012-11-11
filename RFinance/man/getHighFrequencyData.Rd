\name{getHighFrequencyData}
\alias{getHighFrequencyData}
\title{
Download High Frequency Finance Data
}
\description{
Downloads the current financial data from Yahoo Finance
}
\usage{
getHighFrequencyData(Symbols, type = "stock")
}
\arguments{
  \item{Symbols}{
A vector of the ticker symbols to download
}
  \item{type}{
'stock' if downloading stock data, 'option' if download options data
}
}
\details{
Pulls the current financial information on Yahoo Finance.  Call
this function in a loop ending with Sys.sleep to continuously
download data.
}
\value{
if type=='stock'
\item{symbol }{The ticker symbol}
\item{price }{The current price}
\item{time }{The time of the last trade}
\item{change }{The change from the last trade}
\item{prevclose }{The previous day's closing price}
\item{open }{Today's opening price}
\item{bid }{The current bid price}
\item{bidquantity }{The size of the bid order}
\item{ask }{The current ask price}
\item{askquantity }{The size of the ask order}
\item{volume }{The number of shares traded}

if type=='option', returns a list where each item is an option chain
for a unique underlying.  The items of the list are data frames containing:

\item{underlying }{The underlying asset}
\item{type }{Either 'call' or 'put'}
\item{Strike }{The strike price of the option}
\item{Symbol }{The option symbol}
\item{Last }{The current price}
\item{Chg }{The change from the last trade}
\item{Bid }{The current bid price}
\item{Ask }{The current ask price}
\item{Vol }{The number of contracts traded}
\item{OpenInt }{The Open Int of the option}
}
\references{
}
\author{
Jeffrey Wong
}
\note{
%%  ~~further notes~~
}

\seealso{
}
\examples{
}
