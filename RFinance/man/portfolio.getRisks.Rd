\name{portfolio.getRisks}
\alias{portfolio.getRisks}
\title{
Display portfolio risks
}
\description{
Display all risks the trader is exposed to given his portfolio
}
\usage{
portfolio.getRisks(portfolio, riskFreeRate = 0)
}

\arguments{
  \item{portfolio}{

The portfolio, usually kept in a text file and read in
}
  \item{riskFreeRate}{

The risk-free interest rate.  Used in computing risks associated with options
}
}
\details{

}
\value{
A data frame showing exposure to the five greeks for each asset owned
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
tryCatch({
portfolio.getRisks(portfolio, 0)
})
}
