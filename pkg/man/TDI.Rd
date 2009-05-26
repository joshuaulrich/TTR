\name{TDI}
\alias{TDI}
\title{ Trend Detection Index }
\description{
  The Trend Detection Index (TDI) attempts to identify starting and ending
  trends.  Developed by M. H. Pee.
}
\usage{
  TDI(price, n=20, multiple=2)
}
\arguments{
  \item{price}{ Price series that is coercible to xts or matrix. }
  \item{n}{ Number of periods to use. }
  \item{multiple}{ Multiple used to calculate (2). }
}
\details{
  The TDI is the (1) absolute value of the \code{n}-day sum of the
  \code{n}-day momentum, minus the quantity of (2) \code{multiple}*\code{n}-day
  sum of the absolute value of the \code{n}-day momentum, minus (3) \code{n}-day
  sum of the absolute value of the \code{n}-day momentum.

  I.e. TDI = (1) - [ (2) - (3) ]

  The direction indicator is the sum of the \code{n}-day momentum over the
  last \code{n} days.

  See URL in references section for further details.
}
\value{
  A object of the same class as \code{price} or a matrix (if \code{try.xts}
  fails) containing the columns:
  \item{ tdi }{ The Trend Detection Index. }
  \item{ di }{ The Direction Indicator. }
}
\author{ Joshua Ulrich }
\references{
  The following site(s) were used to code/document this indicator:\cr
  \url{http://www.linnsoft.com/tour/techind/tdi.htm}\cr
}
\note{
  Positive/negative TDI values signal a trend/consolidation.  A positive/
  negative direction indicator signals a up/down trend.  I.e. buy if the
  TDI and the direction indicator are positive, and sell if the TDI is
  positive while the direction indicator is negative.
}
\seealso{
  See \code{\link{aroon}}, \code{\link{CCI}}, \code{\link{ADX}},
  \code{\link{VHF}}, \code{\link{GMMA}} for other indicators that
  measure trend direction/strength.
}
\examples{
  data(ttrc)
  tdi <- TDI(ttrc[,"Close"], n=30)
}
\keyword{ ts }
