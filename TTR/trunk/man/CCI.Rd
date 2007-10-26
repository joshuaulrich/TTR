\name{CCI}
\alias{CCI}
\title{ Commodity Channel Index }
\concept{trend}
\description{
  The Commodity Channel Index (CCI) attempts to identify starting and ending trends.
}
\usage{
  CCI(HLC, ma=list("SMA", n=20), c=0.015)
}
\arguments{
  \item{HLC}{ High-Low-Close price series to use.  If only a univariate series is given,
    it will be used.  See details. }
  \item{ma}{ A \emph{list} whose first component is a string containing the moving average
    function name; additional parameters may also be specified as \emph{named} components. }
  \item{c}{ Constant to apply to the mean deviation. } 
}
\details{
  CCI relates the current price and the average of price over \code{n} periods. The CCI
  usually falls in a channel of -100 to 100. A basic CCI trading system is:
  Buy (sell) if CCI rises above 100 (falls below -100) and sell (buy) when it falls
  below 100 (rises above -100).

  CCI is usually calculated using the typical price, but if a univariate series
  (e.g. Close, Weighted Close, Median Price, etc.) is provided, it will be used instead.
}
\value{
  A vector containing the CCI values.
}
\author{ Josh Ulrich }
\references{
  The following site(s) were used to code/document this indicator:\cr
  \url{http://www.fmlabs.com/reference/CCI.htm}\cr
  \url{http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=42}\cr
  \url{http://www.linnsoft.com/tour/techind/cci.htm}\cr
  \url{http://stockcharts.com/education/IndicatorAnalysis/indic_CCI.html}\cr
}
\note{
  If \code{HLC} is a High-Low-Close matrix, then typical price will be calculated.
  If \code{HLC} is a vector, then those values will be used instead of the typical price.
}
\seealso{
  See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average options;
  and note Warning section.  See \code{\link{aroon}}, \code{\link{ADX}},
  \code{\link{TDI}}, \code{\link{VHF}} for other indicators that measure trend
  direction/strength. }
\examples{
  data(ttrc)
  cci <- CCI(ttrc[,c("High","Low","Close")])
}
\keyword{ ts }
