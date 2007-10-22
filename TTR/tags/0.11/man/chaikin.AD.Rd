\name{chaikin.AD}
\alias{chaikin.AD}
\title{ Chaikin Accumulation / Distribution }
\description{
  The Chaikin Accumulation / Distribution (AD) line is a measure of the money flowing into or
  out of a security.  It is similar to On Balance Volume (OBV).  Developed by Marc Chaikin.
}
\usage{
  chaikin.AD(HLC, volume)
}
\arguments{
  \item{HLC}{ Object able to be coerced to a matrix, which contains High-Low-Close prices. }
  \item{volume}{ Vector or matrix of volume observations corresponding to the \code{HLC} object. }
}
\details{
  The AD line is similar to OBV; the difference is that OBV sums volume multiplied by +/- 1
  if the close is higher/lower than the previous close, while the AD line multiplies volume
  by the close location value (CLV).
}
\value{
  A vector containing the accumulation / distribution values.
}
\author{ Josh Ulrich }
\references{
  The following site(s) were used to code/document this indicator:\cr
  \url{http://www.fmlabs.com/reference/AccumDist.htm}\cr
  \url{http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=27}\cr
  \url{http://www.linnsoft.com/tour/techind/acc_dis.htm}\cr
  \url{http://stockcharts.com/education/IndicatorAnalysis/indic_AccumDistLine.html}\cr
}
\note{
  The Accumulation/Distribution Line is interpreted by looking for a divergence in the
  direction of the indicator relative to price.
}
\seealso{
  See \code{\link{OBV}}, and \code{\link{CLV}}.
}
\examples{
  data(nasdaq)
  ad <- chaikin.AD(nasdaq[,c("High","Low","Close")], nasdaq[,"Volume"])
}
\keyword{ ts }