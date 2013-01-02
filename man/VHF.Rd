\name{VHF}
\alias{VHF}
\title{Vertical Horizontal Filter}
\usage{
  VHF(price, n = 28)
}
\arguments{
  \item{price}{Object that is coercible to xts or matrix
  and contains a Close price series, or a High-Low-Close
  price series.}

  \item{n}{Number of periods to use.}
}
\value{
  A object of the same class as \code{price} or a vector
  (if \code{try.xts} fails) containing the VHF values.
}
\description{
  The Vertical Horizontal Filter (VHF) attempts to identify
  starting and ending trends.  Developed by Adam White.
}
\details{
  The VHF is calculated by subtracting the \code{n}-period
  lowest low from the \code{n}-period highest high and
  dividing that result by the \code{n}-period rolling sum
  of the close price changes.
}
\note{
  If Close prices are given, the function calculates the
  max/min using only those prices (the default).  If HLC
  prices are given, the function calculates the max/min
  using the high/low prices (added for flexibility).
}
\examples{
data(ttrc)
vhf.close <- VHF(ttrc[,"Close"])
vhf.hilow <- VHF(ttrc[,c("High","Low","Close")])
}
\author{
  Joshua Ulrich
}
\references{
  The following site(s) were used to code/document this
  indicator:\cr
  \url{http://www.equis.com/Customer/Resources/TAAZ?c=3&p=119}\cr
}
\seealso{
  See \code{\link{aroon}}, \code{\link{CCI}},
  \code{\link{ADX}}, \code{\link{TDI}}, \code{\link{GMMA}}
  for other indicators that measure trend
  direction/strength.
}
\keyword{ts}

