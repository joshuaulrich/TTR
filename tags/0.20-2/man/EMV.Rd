\name{EMV}
\alias{EMV}
\title{ Arms' Ease of Movement Value }
\description{
  Arms' Ease of Movement Value (EMV) emphasizes days where the security moves
  easily and minimizes days where the security does not move easily.  Developed
  by Richard W. Arms, Jr.
}
\usage{
  EMV(HL, volume, n=9, maType, vol.divisor=10000, \dots)
}
\arguments{
  \item{HL}{ Object that is coercible to xts or matrix and contains High-Low prices. }
  \item{volume}{ Vector or matrix of volume observations corresponding to the \code{HL} object. }
  \item{n}{ Number of periods for moving average. }
  \item{maType}{ A function or a string naming the function to be called. }
  \item{vol.divisor}{ An increment to make the results larger and easier to work with. }
  \item{\dots}{ Other arguments to be passed to the \code{maType} function. }
}
\details{
  The EMV is calculated by dividing the midpoint ([high + low]/2) move by the
  'Box Ratio' (volume divided by the high minus low).
}
\value{
  A object of the same class as \code{HL} and \code{volume} or a matrix
  (if \code{try.xts} fails) containing the columns:
  \item{ emv }{ The ease of movement values. }
  \item{ emvMA }{ The smoothed (as specified by \code{ma}) ease of movement values. }
}
\author{ Joshua Ulrich }
\references{
  The following site(s) were used to code/document this indicator:\cr
  \url{http://www.fmlabs.com/reference/ArmsEMV.htm}\cr
  \url{http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=51}\cr
  \url{http://linnsoft.com/tour/techind/arms.htm}\cr
}
\note{
  A buy/sell signal is generated when the EMV crosses above/below zero.  When the EMV
  hovers around zero, there are small price movements and/or high volume, and the
  price is not moving easily.
}
\seealso{
  See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average options; and note
  Warning section.
}
\examples{
  data(ttrc)
  emv <- EMV(ttrc[,c("High","Low")], ttrc[,"Volume"])
}
\keyword{ ts }
