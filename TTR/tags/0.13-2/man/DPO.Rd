\name{DPO}
\alias{DPO}
\title{ De-Trended Price Oscillator }
\description{
  The Detrended Price Oscillator (DPO) removes the trend in prices - or other series -
  by subtracting a moving average of the price from the price.
}
\usage{
  DPO(x, ma=list("SMA", n=10), shift=ma$n/2+1, percent=FALSE)
}
\arguments{
  \item{x}{ Price, volume, etc. series to use. }
  \item{ma}{ A \emph{list} whose first component is a string containing the moving average
    function name; additional parameters may also be specified as \emph{named} components. }
  \item{shift}{ The number of periods to shift the moving average. }
  \item{percent}{ logical; if \code{TRUE}, the percentage difference between the slow and fast moving
    averages is returned, otherwise the difference between the respective averages is returned. }
}
\details{
  The Detrended Price shows cycles and overbought / oversold conditions. Note the calculation
  shifts the results \code{shift} periods, so the last \code{shift} periods will be zero.
}
\value{
  A vector containing the DPO values.
}
\author{ Josh Ulrich }
\references{
  The following site(s) were used to code/document this indicator:\cr
  \url{http://www.fmlabs.com/reference/DPO.htm}\cr
  \url{http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=48}\cr
}
\note{
  As stated above, the DPO can be used on any univariate series, not just price.
}
\seealso{
  See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average options; and note
  Warning section.  See \code{\link{oscillator}} for other oscillators.
}
\examples{
  data(ttrc)
  price.dpo <- DPO(ttrc[,"Close"])
  volume.dpo <- DPO(ttrc[,"Volume"])
}
\keyword{ ts }
