\name{CMO}
\alias{CMO}
\title{ Chande Momentum Oscillator }
\description{
  The Chande Momentum Oscillator (CMO) is a modified RSI.  Developed by Tushar S. Chande.
}
\usage{
  CMO(x, n=14)
}
\arguments{
  \item{x}{ Price, volume, etc. series to use. }
  \item{n}{ Number of periods to use. }
}
\details{
  The CMO divides the total movement by the net movement ([up - down] / [up + down]), where
  RSI divides the upward movement by the net movement (up / [up + down]).
}
\value{
  A vector containing Chande Momentum Oscillator values.
}
\author{ Josh Ulrich }
\references{
  The following site(s) were used to code/document this indicator:\cr
  \url{http://www.fmlabs.com/reference/CMO.htm}\cr
}
\seealso{
  See \code{\link{RSI}}.
}
\note{
  There are several ways to interpret the CMO:\cr
  (1) Values over/under +/- 50 indicate overbought/oversold conditions.\cr
  (2) High CMO values indicate strong trends.\cr
  (3) When the CMO crosses above/below a moving average of the CMO, it is a buy/sell signal.\cr
}
\examples{
  data(ttrc)
  cmo <- CMO(ttrc[,"Close"])
}
\keyword{ ts }
