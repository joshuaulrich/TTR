\name{GMMA}
\alias{GMMA}
\alias{guppy}
\alias{Guppy}
\title{Guppy Multiple Moving Averages}
\usage{
  GMMA(x, short = c(3, 5, 8, 10, 12, 15),
    long = c(30, 35, 40, 45, 50, 60), maType)
}
\arguments{
  \item{x}{Price, volume, etc. series that is coercible to
  xts or matrix.}

  \item{short}{Vector of short-term periods.}

  \item{long}{Vector of long-term periods.}

  \item{maType}{Either: \enumerate{ \item A function or a
  string naming the function to be called.  \item A
  \emph{list} with the first component like (1) above, and
  additional parameters specified as \emph{named}
  components.  See Examples. }}
}
\value{
  A object of the same class as \code{x} or \code{price} or
  a vector (if \code{try.xts} fails) containing the Guppy
  Multiple Moving Average.
}
\description{
  Calculate the Guppy Multiple Moving Average of a series.
}
\details{
  The Guppy Multiple Moving Average signals a changing
  trend when the \code{short} and \code{long} groups of
  moving averages intersect.  An up/down trend exists when
  the short/long-term moving averages are greater than the
  long/short-term averages.
}
\examples{
data(ttrc)
gmma <- GMMA(ttrc[,"Close"])
}
\author{
  Joshua Ulrich
}
\references{
  The following site(s) were used to code/document this
  indicator:\cr
  \url{http://www.investopedia.com/terms/g/guppy-multiple-moving-average.asp}\cr
}
\seealso{
  See \code{\link{aroon}}, \code{\link{CCI}},
  \code{\link{ADX}}, \code{\link{VHF}}, \code{\link{TDI}}
  for other indicators that measure trend
  direction/strength.
}
\keyword{ts}

