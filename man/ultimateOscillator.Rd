\name{ultimateOscillator}
\alias{ultimateOscillator}
\title{The Ultimate Oscillator}
\usage{
  ultimateOscillator(HLC, n = c(7, 14, 28),
    wts = c(4, 2, 1))
}
\arguments{
  \item{HLC}{Object that is coercible to xts or matrix and
  contains High-Low-Close prices.}

  \item{n}{A vector of the number of periods to use for
  each average calculation.}

  \item{wts}{The weights applied to each average.}
}
\description{
  The Ultimate Oscillator is a momentum oscillator designed
  to capture momentum across three different time frames.
}
\details{
  Created by Larry Williams in 1976.
}
\examples{
data(ttrc)
ult.osc <- ultimateOscillator(ttrc[,c("High","Low","Close")])
}
\author{
  Ivan Popivanov
}
\references{
  The following site(s) were used to code/document this
  indicator:\cr
  \url{http://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:ultimate_oscillator}\cr
}
\keyword{ts}

