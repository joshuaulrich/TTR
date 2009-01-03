\name{bollingerBands}
\alias{bollingerBands}
\title{ Bollinger Bands }
\description{
  Bollinger Bands are a way to compare a security's volatility and price levels over
  a period of time.  Developed by John Bollinger.
}
\usage{
  bollingerBands(HLC, ma = list("SMA", n=20), sd = list(FUN="sd", n=2))
}
\arguments{
  \item{HLC}{ High-Low-Close price series to use.  If only a univariate series is given,
    it will be used.  See details. }
  \item{ma}{ A \emph{list} whose first component is a string containing the moving average
    function name; additional parameters may also be specified as \emph{named} components. }
  \item{sd}{ A \emph{list} of standard deviation input. }
}
\details{
  Bollinger Bands consist of three lines:

  The middle band is generally a 20-period SMA of the typical price ([high + low + close]/3).
  The upper and lower bands are \code{sd$n} standard deviations (generally 2) above and
  below the MA.

  The middle band is usually calculated using the typical price, but if a univariate series
  (e.g. Close, Weighted Close, Median Price, etc.) is provided, it will be used instead.
}
\value{
  A matrix containing the columns:
  \item{ dn }{ The lower Bollinger Band. }
  \item{ ma }{ The middle Moving Average (see notes). }
  \item{ up }{ The upper Bollinger Band. }
  \item{ pct.b }{ The \%B calculation. }
}
\author{ Josh Ulrich }
\references{
  The following site(s) were used to code/document this indicator:\cr
  \url{http://www.fmlabs.com/reference/Bollinger.htm}\cr
  \url{http://www.fmlabs.com/reference/BollingerWidth.htm}\cr
  \url{http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=36}\cr
  \url{http://www.linnsoft.com/tour/techind/bb.htm}\cr
  \url{http://stockcharts.com/education/IndicatorAnalysis/indic_Bbands.html}\cr
  \url{http://stockcharts.com/education/IndicatorAnalysis/indic_BBWidth.htm}\cr
}
\note{
  Using any moving average other than SMA will result in inconsistencies between the
  moving average calculation and the standard deviation calculation.  Since, by
  definition, a rolling standard deviation uses a simple moving average.
}
\seealso{
  See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average options; and note
  Warning section.
}
\examples{
  ## The examples below show the differences between using a High-Low-Close series, and
  ## just a close series when calculating Bollinger Bands.
  data(ttrc)
  bbands.HLC   <- bollingerBands( ttrc[,c("High","Low","Close")] )
  bbands.close <- bollingerBands( ttrc[,"Close"] )
}
\keyword{ ts }
