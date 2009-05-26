\name{MFI}
\alias{MFI}
\alias{moneyFlow}
\title{ Money Flow Index }
\description{
  The MFI is a ratio of positive and negative money flow over time.
}
\usage{
  MFI(HLC, volume, n=14)
}
\arguments{
  \item{HLC}{ Object that is coercible to xts or matrix and contains High-Low-Close prices.
  If only a univariate series is given, it will be used.  See details. }
  \item{volume}{ Vector or matrix of volume observations corresponding
    to \code{HLC} object. }
  \item{n}{ Number of periods to use. }
}
\details{
  Money Flow (MF) is the product of price and volume.  Positive/negative
  MF occur when today's price is higher/lower than yesterday's price.
  The MFI is calculated by dividing positive MF by negative MF for the
  past \code{n} periods.  It is then scaled between 0 and 100.

  MFI is usually calculated using the typical price, but if a univariate
  series (e.g. Close, Weighted Close, Median Price, etc.) is provided,
  it will be used instead.
}
\value{
  A object of the same class as \code{HLC} and \code{volume} or a vector
  (if \code{try.xts} fails) containing the MFI values.
}
\author{ Joshua Ulrich }
\references{
  The following site(s) were used to code/document this indicator:\cr
  \url{http://www.fmlabs.com/reference/default.htm?url=MoneyFlowIndex.htm}\cr
  \url{http://www.linnsoft.com/tour/techind/mfi.htm}\cr
  \url{http://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:money_flow_index_mfi}\cr
}
\note{
  Divergence between MFI and price can be indicative of a reversal.
  In addition, values above/below 80/20 indicate market tops/bottoms.
}
\seealso{
  See \code{\link{OBV}} and \code{\link{CMF}}.
}
\examples{
  data(ttrc)
  mfi <- MFI(ttrc[,c("High","Low","Close")], ttrc[,"Volume"])
}
\keyword{ ts }
