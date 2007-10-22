\name{chaikin.volatility}
\alias{chaikin.volatility}
\title{ Chaikin Volatility }
\description{
  Chaikin Volatility measures the rate of change of the security's trading range.
  Developed by Marc Chaikin.
}
\usage{
  chaikin.volatility(HL, ma = list("EMA", n=10))
}
\arguments{
  \item{HL}{ Object able to be coerced to a matrix, which contains High-Low prices. }
  \item{ma}{ A _list_ whose first component is a string containing the moving average
    function name; additional parameters may also be specified as _named_ components. }
}
\details{
  The Chaikin Volatility indicator defines volatility as an increase in the difference
  between the high and low.
}
\value{
  A vector containing the Chaikin Volatility values.
}
\author{ Josh Ulrich }
\references{
  The following site(s) were used to code/document this indicator:\cr
  \url{http://www.fmlabs.com/reference/ChaikinVolatility.htm}\cr
  \url{http://www.equis.com/Customer/Resources/TAAZ/Default.aspx?c=3&p=120}\cr
}
\note{
  A rapid increase in Chaikin Volatility indicates an approaching bottom.
  A slow decrease in Chaikin Volatility indicates an approaching top.
}
\seealso{
  See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average options; and note
  Warning section.  See \code{\link{TR}} for another volatility measure.
}
\examples{
  data(nasdaq)
  volatility <- chaikin.volatility(nasdaq[,c("High","Low")])
}
\keyword{ ts }