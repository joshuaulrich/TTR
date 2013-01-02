\name{SAR}
\alias{SAR}
\title{Parabolic Stop-and-Reverse}
\usage{
  SAR(HL, accel = c(0.02, 0.2))
}
\arguments{
  \item{HL}{Object that is coercible to xts or matrix and
  contains High-Low prices.}

  \item{accel}{accel[1]: Acceleration factor.\cr accel[2]:
  Maximum acceleration factor.}
}
\value{
  A object of the same class as \code{HL} or a vector (if
  \code{try.xts} fails) containing the Parabolic Stop and
  Reverse values.
}
\description{
  The Parabolic Stop-and-Reverse calculates a trailing
  stop.  Developed by J. Welles Wilder.
}
\details{
  The calculation for the SAR is quite complex.  See the
  URLs in the references section for calculation notes.

  The SAR assumes that you are always in the market, and
  calculates the Stop And Reverse point when you would
  close a long position and open a short position or vice
  versa.
}
\examples{
data(ttrc)
sar <- SAR(ttrc[,c("High","Low")])
}
\author{
  Joshua Ulrich
}
\references{
  The following site(s) were used to code/document this
  indicator:\cr
  \url{http://www.linnsoft.com/tour/techind/sar.htm}\cr
  \url{http://www.fmlabs.com/reference/SAR.htm}\cr
  \url{http://stockcharts.com/education/IndicatorAnalysis/indic_ParaSAR.htm}\cr
  \url{http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=87}
}
\seealso{
  See \code{\link{ATR}} and \code{\link{ADX}}, which were
  also developed by Welles Wilder.
}
\keyword{ts}

