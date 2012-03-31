\name{bollingerBands}
\alias{bollingerBands}
\alias{BBands}
\title{ Bollinger Bands }
\description{
  Bollinger Bands are a way to compare a security's volatility and price levels over
  a period of time.  Developed by John Bollinger.
}
\usage{
  BBands(HLC, n=20, maType, sd=2, \dots)
}
\arguments{
  \item{HLC}{ Object that is coercible to xts or matrix and contains High-Low-Close prices.
   If only a univariate series is given, it will be used.  See details. }
  \item{n}{ Number of periods for moving average. }
  \item{maType}{ A function or a string naming the function to be called. }
  \item{sd}{ The number of standard deviations to use. }
  \item{\dots}{ Other arguments to be passed to the \code{maType} function. }
}
\details{
  Bollinger Bands consist of three lines:

  The middle band is generally a 20-period SMA of the typical price ([high + low + close]/3).
  The upper and lower bands are \code{sd} standard deviations (generally 2) above and
  below the MA.

  The middle band is usually calculated using the typical price, but if a univariate series
  (e.g. Close, Weighted Close, Median Price, etc.) is provided, it will be used instead.
}
\value{
  A object of the same class as \code{HLC} or a matrix (if \code{try.xts}
  fails) containing the columns:
  \item{ dn }{ The lower Bollinger Band. }
  \item{ ma }{ The middle Moving Average (see notes). }
  \item{ up }{ The upper Bollinger Band. }
  \item{ pctB }{ The \%B calculation. }
}
\author{ Joshua Ulrich }
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
  ## The examples below show the differences between using a
  ## High-Low-Close series, and just a close series when
  ## calculating Bollinger Bands.
  data(ttrc)
  bbands.HLC <- BBands( ttrc[,c("High","Low","Close")] )
  bbands.close <- BBands( ttrc[,"Close"] )
}
\keyword{ ts }
