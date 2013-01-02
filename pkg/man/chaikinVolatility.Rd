\name{chaikinVolatility}
\alias{chaikinVolatility}
\title{Chaikin Volatility}
\usage{
  chaikinVolatility(HL, n = 10, maType, ...)
}
\arguments{
  \item{HL}{Object that is coercible to xts or matrix and
  contains High-Low prices.}

  \item{n}{Number of periods for moving average.}

  \item{maType}{A function or a string naming the function
  to be called.}

  \item{\dots}{Other arguments to be passed to the
  \code{maType} function.}
}
\value{
  A object of the same class as \code{HL} or a vector (if
  \code{try.xts} fails) containing the Chaikin Volatility
  values.
}
\description{
  Chaikin Volatility measures the rate of change of the
  security's trading range.  Developed by Marc Chaikin.
}
\details{
  The Chaikin Volatility indicator defines volatility as an
  increase in the difference between the high and low.
}
\note{
  A rapid increase in Chaikin Volatility indicates an
  approaching bottom. A slow decrease in Chaikin Volatility
  indicates an approaching top.
}
\examples{
data(ttrc)
volatility <- chaikinVolatility(ttrc[,c("High","Low")])
}
\author{
  Joshua Ulrich
}
\references{
  The following site(s) were used to code/document this
  indicator:\cr
  \url{http://www.fmlabs.com/reference/ChaikinVolatility.htm}\cr
  \url{http://www.equis.com/Customer/Resources/TAAZ/Default.aspx?c=3&p=120}\cr
}
\seealso{
  See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving
  average options; and note Warning section.  See
  \code{\link{TR}} for another volatility measure.
}
\keyword{ts}

