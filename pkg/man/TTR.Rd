\name{TTR}
\alias{TTR}
\docType{package}
\title{
  Functions to create Technical Trading Rules (TTR)
}
\description{
  This package contains many of the most popular technical analysis functions, as well
  as functions to retrieve U.S. stock symbols, and data from Yahoo Finance.
}
\details{
  \tabular{ll}{
    Package: \tab TTR\cr
    Type: \tab Package\cr
    Version: \tab 0.20-1\cr
    Date: \tab 2009-08-27\cr
    License: \tab GPL Version 3 or later.\cr
  }

  Users will probably be most interested in the following functions:\cr
  \code{\link{ADX}}\cr
  \code{\link{BBands}}\cr
  \code{\link{changes}}\cr
  \code{\link{MovingAverages}}\cr
  \code{\link{MACD}}\cr
  \code{\link{RSI}}\cr
  \code{\link{runFun}}\cr
  \code{\link{stoch}}\cr
  \code{\link{WebData}}\cr
}
\author{
Joshua Ulrich

Maintainer: Joshua Ulrich
}
\references{
  The following sites were used to code/document this package:\cr
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
  data(ttrc)

  # Bollinger Bands
  bbands <- BBands( ttrc[,c("High","Low","Close")] )

  # Directional Movement Index
  adx <- ADX(ttrc[,c("High","Low","Close")])

  # Moving Averages
  ema <- EMA(ttrc[,"Close"], n=20)
  sma <- SMA(ttrc[,"Close"], n=20)

  # MACD
  macd <- MACD( ttrc[,"Close"] )

  # RSI
  rsi <- RSI(ttrc[,"Close"])

  # Stochastics
  stochOsc <- stoch(ttrc[,c("High","Low","Close")])

  ### Note: you must have a working internet connection
  ### for the examples below to work!

  # Fetch U.S. symbols from the internet
  nyseSymbols <- stockSymbols("NYSE")

  # Fetch Yahoo! Finance data from the internet
  ibm <- getYahooData("IBM", 19990404, 20050607)
}
