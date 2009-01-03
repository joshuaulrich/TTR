\name{TTR-package}
\alias{TTR-package}
\alias{TTR}
\docType{package}
\title{
  Functions to create Technical Trading Rules (TTR).
}
\description{
  This package contains many of the most popular technical analysis functions, as well
  as functions to retrieve U.S. stock symbols, and data from the internet.

  First on the wishlist is a plotting function that allows the charting of price, volume,
  indicators, etc. on one device.  A close second is a better functionality for summarizing
  the results of technical trading rules.
}
\details{
  \tabular{ll}{
    Package: \tab TTR\cr
    Type: \tab Package\cr
    Version: \tab 0.11\cr
    Date: \tab 2006-12-31\cr
    License: \tab GPL Version 2 or later.\cr
  }

  Users will probably be most interested in the following functions:\cr
  \code{\link{bbands}}\cr
  \code{\link{Changes}}\cr
  \code{\link{DX}}\cr
  \code{\link{MovingAverages}}\cr
  \code{\link{Oscillators}}\cr
  \code{\link{RSI}}\cr
  \code{\link{stoch}}\cr
  \code{\link{WebData}}\cr
}
\author{
Josh Ulrich

Maintainer: Josh Ulrich
}
\references{
  The following site(s) were used to code/document this package:\cr
  \url{http://www.fmlabs.com/reference/default.htm}\cr
  \url{http://www.equis.com/Customer/Resources/TAAZ/?p=0}\cr
  \url{http://www.linnsoft.com/tour/technicalindicators.htm}\cr
  \url{http://stockcharts.com/education/IndicatorAnalysis/}\cr
}
\keyword{ package }
%--\seealso{
%--~~ Optional links to other man pages, e.g. ~~
%--~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
%--}
\examples{
  data(nasdaq)

  # Bollinger Bands
  bbands <- bbands( nasdaq[,c("High","Low","Close")] )

  # Directional Movement Index
  dmi.adx <- DX(nasdaq[,c("High","Low","Close")])

  # Moving Averages
  ema <- EMA(nasdaq[,"Close"], n=20)
  sma <- SMA(nasdaq[,"Close"], n=20)

  # MACD
  macd <- MACD( nasdaq[,"Close"] )

  # RSI
  rsi <- RSI(nasdaq[,"Close"])

  # Stoichastics
  stoch.osc <- stoch(nasdaq[,c("High","Low","Close")])

  ### Note: you must have a working internet connection
  ### for the examples below to work!

  # Fetch U.S. symbols from the internet
  nyse.symbols <- get.symbols("NYSE")

  # Fetch Yahoo! Finance data from the internet
  ibm <- yahoo.data("IBM", 19990404, 20050607)
}