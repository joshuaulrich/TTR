\name{changes}
\alias{changes}
\alias{ROC}
\alias{momentum}
\title{ Rate of Change / Momentum }
\concept{returns}
\description{
  Calculate the (rate of) change of a series over \code{n} periods.
}
\usage{
  ROC(x, n=1, type=c("continuous", "discrete"), na=NA)
  momentum(x, n=1, na.pad=TRUE)
}
\arguments{
  \item{x}{ Price, volume, etc. series to use. }
  \item{n}{ Number of periods to use. }
  \item{type}{ Compounding type; either \code{"continuous"} (the default) or \code{"discrete"}. }
  \item{na.pad}{ Should periods prior to \code{n} be appended?  Default is \code{TRUE}. }
  \item{na}{ How should periods prior to \code{n} be represented?  Default is \code{NA}. }
}
\details{
  The ROC indicator provides the percentage difference of a series over two observations,
  while the momentum indicator simply provides the difference.
}
\value{
  \code{ROC} returns a vector containing the rate-of-change (or return) values.

  \code{momentum} returns a vector containing the differenced price series.
}
\author{ Josh Ulrich }
\examples{
  data(ttrc)
  roc <- ROC(ttrc[,"Close"])
  mom <- momentum(ttrc[,"Close"])
}
\keyword{ ts }
