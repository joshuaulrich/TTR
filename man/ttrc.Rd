\docType{data}
\name{ttrc}
\alias{ttrc}
\title{Technical Trading Rule Composite data}
\format{The format is: \tabular{lll}{ Date: \tab Class 'Date' \tab 5480 5481
5482 5485 5486 ...\cr Open: \tab num \tab 3.18 3.09 3.11 3.09 3.10 ...\cr
High: \tab num \tab 3.18 3.15 3.12 3.12 3.12 ...\cr Low: \tab num \tab 3.08
3.09 3.08 3.07 3.08 ...\cr Close: \tab num \tab 3.08 3.11 3.09 3.10 3.11
...\cr Volume: \tab num \tab 1870906 3099506 2274157 2086758 2166348 ...\cr }}
\source{
  Randomly generated.
}
\description{
  Historical Open, High, Low, Close, and Volume data for
  the periods January 2, 1985 to December 31, 2006.
  Randomly generated.
}
\details{
  These data do not represent an actual security.  They are
  provided so examples do not necessitate an internet
  connection.
}
\examples{
data(ttrc)
plot(tail(ttrc[,"Close"],100), type="l")
}
\keyword{datasets}

