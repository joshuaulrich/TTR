\name{nasdaq}
\alias{nasdaq}
\docType{data}
\title{ NASDAQ Composite data from Yahoo Finance }
\description{
  Historical NASDAQ Composite Open, High, Low, Close, and Volume data for the periods
  January 2, 1985 to December 31, 2006.  From Yahoo Finance.
}
\usage{
  data(nasdaq)
}
\format{
  The format is:
  \tabular{lll}{
    Date: \tab Class \sQuote{Date} num [1:5545] \tab 5480 5481 5482 5485 5486 ...\cr
    Open: \tab num \tab 247 246 246 246 246 ...\cr
    High: \tab num \tab 247 247 246 246 246 ...\cr
    Low: \tab num \tab 246 246 246 246 246 ...\cr
    Close: \tab num \tab 246 246 246 246 246 ...\cr
    Volume: \tab num \tab 48210000 52250000 55800000 53200000 64850000 ...\cr
  }
}
\details{
  The Yahoo symbol for these data is \sQuote{^IXIC}.
}
\source{
  \url{http://finance.yahoo.com}
}
\references{
  \url{http://finance.yahoo.com/q/hp?s=\%5EIXIC}
}
\examples{
  data(nasdaq)
  plot(tail(nasdaq[,"Close"],100), type="l")
}
\keyword{ datasets }
