\name{CMO}
\alias{CMO}
\title{Chande Momentum Oscillator}
\usage{
  CMO(x, n = 14)
}
\arguments{
  \item{x}{Price, volume, etc. series that is coercible to
  xts or matrix.}

  \item{n}{Number of periods to use.}
}
\value{
  A object of the same class as \code{x} or a vector (if
  \code{try.xts} fails) containing Chande Momentum
  Oscillator values.
}
\description{
  The Chande Momentum Oscillator (CMO) is a modified RSI.
  Developed by Tushar S. Chande.
}
\details{
  The CMO divides the total movement by the net movement
  ([up - down] / [up + down]), where RSI divides the upward
  movement by the net movement (up / [up + down]).
}
\note{
  There are several ways to interpret the CMO: \enumerate{
  \item Values over/under +/- 50 indicate
  overbought/oversold conditions.  \item High CMO values
  indicate strong trends.  \item When the CMO crosses
  above/below a moving average of the CMO, it is a buy/sell
  signal. }
}
\examples{
data(ttrc)
cmo <- CMO(ttrc[,"Close"])
}
\author{
  Joshua Ulrich
}
\references{
  The following site(s) were used to code/document this
  indicator:\cr
  \url{http://www.fmlabs.com/reference/CMO.htm}\cr
}
\seealso{
  See \code{\link{RSI}}.
}
\keyword{ts}

