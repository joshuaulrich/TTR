\name{williamsAD}
\alias{williamsAD}
\title{ Williams Accumulation / Distribution }
\description{
  The Williams Accumulation / Distribution (AD) line is a measure of market momentum.
  Developed by Larry Williams.
}
\usage{
  williamsAD(HLC)
}
\arguments{
  \item{HLC}{ Object that is coercible to xts or matrix and contains High-Low-Close prices. }
}
\details{
  The Williams AD line differs from OBV and chaikinAD in that it doesn't take volume into
  account.
}
\value{
  A object of the same class as \code{HLC} or a vector (if \code{try.xts}
  fails) containing the accumulation / distribution values.
}
\author{ Joshua Ulrich }
\references{
  The following site(s) were used to code/document this indicator:\cr
  \url{http://www.fmlabs.com/reference/WilliamsAD.htm}\cr
  \url{http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=125}\cr
}
\note{
  The Accumulation/Distribution Line is interpreted by looking for a divergence in the
  direction of the indicator relative to price.
}
\seealso{
  See \code{\link{OBV}}, \code{\link{chaikinAD}}, and \code{\link{ATR}}.
}
\examples{
  data(ttrc)
  ad <- williamsAD(ttrc[,c("High","Low","Close")])
}
\keyword{ ts }
