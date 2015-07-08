#'Technical Trading Rule Composite data
#'
#'Historical Open, High, Low, Close, and Volume data for the periods January 2,
#'1985 to December 31, 2006. Randomly generated.
#'
#'These data do not represent an actual security.  They are provided so
#'examples do not necessitate an internet connection.
#'
#'@name ttrc
#'@docType data
#'@format The format is: \tabular{lll}{ Date: \tab Class 'Date' \tab 5480 5481
#'5482 5485 5486 ...\cr Open: \tab num \tab 3.18 3.09 3.11 3.09 3.10 ...\cr
#'High: \tab num \tab 3.18 3.15 3.12 3.12 3.12 ...\cr Low: \tab num \tab 3.08
#'3.09 3.08 3.07 3.08 ...\cr Close: \tab num \tab 3.08 3.11 3.09 3.10 3.11
#'...\cr Volume: \tab num \tab 1870906 3099506 2274157 2086758 2166348 ...\cr }
#'@source Randomly generated.
#'@keywords datasets
#'@examples
#'
#' data(ttrc)
#' plot(tail(ttrc[,"Close"],100), type="l")
#'@rdname ttrc
NULL


#'Functions to create Technical Trading Rules (TTR)
#'
#'This package contains many of the most popular technical analysis functions,
#'as well as functions to retrieve U.S. stock symbols, and data from Yahoo
#'Finance.
#'
#'Users will probably be most interested in the following functions:\cr
#'\code{\link{ADX}}\cr \code{\link{BBands}}\cr \code{\link{changes}}\cr
#'\code{\link{MovingAverages}}\cr \code{\link{MACD}}\cr \code{\link{RSI}}\cr
#'\code{\link{runFun}}\cr \code{\link{stoch}}\cr \code{\link{VWAP}}\cr
#'\code{\link{WebData}}\cr
#'
#'@name TTR
#'@docType package
#'@useDynLib TTR
#'@author Joshua Ulrich
#'
#'Maintainer: Joshua Ulrich
#'@references The following sites were used to code/document this package:\cr
#'\url{http://www.fmlabs.com/reference/default.htm}\cr
#'\url{http://www.equis.com/Customer/Resources/TAAZ/?p=0}\cr
#'\url{http://www.linnsoft.com/tour/technicalindicators.htm}\cr
#'\url{http://stockcharts.com/education/IndicatorAnalysis/}\cr
#'@keywords package
#'@examples
#'
#' data(ttrc)
#'
#' # Bollinger Bands
#' bbands <- BBands( ttrc[,c("High","Low","Close")] )
#'
#' # Directional Movement Index
#' adx <- ADX(ttrc[,c("High","Low","Close")])
#'
#' # Moving Averages
#' ema <- EMA(ttrc[,"Close"], n=20)
#' sma <- SMA(ttrc[,"Close"], n=20)
#'
#' # MACD
#' macd <- MACD( ttrc[,"Close"] )
#'
#' # RSI
#' rsi <- RSI(ttrc[,"Close"])
#'
#' # Stochastics
#' stochOsc <- stoch(ttrc[,c("High","Low","Close")])
#'
#' ### Note: you must have a working internet connection
#' ### for the examples below to work!
#'
#' # Fetch U.S. symbols from the internet
#' nyseSymbols <- stockSymbols("NYSE")
#'
#' # Fetch Yahoo! Finance data from the internet
#' ibm <- getYahooData("IBM", 19990404, 20050607)
#' 
#'@rdname TTR
#'@import xts zoo
#'@importFrom stats approx embed na.omit sd
#'@importFrom utils flush.console read.csv read.table
NULL
