#' Signal to Noise Ratio
#' 
#' The n-day SNR for a given market is calculated by taking the absolute
#' price change over an n-day period and dividing it by the average
#' n-day volatility. 
#' 
#' \deqn{SNR(n) = \frac{ABS(P_t - P_{t-n})}{ATR_n}}{SNR(n) = ABS(P_t - P_(t-n))/ATR_n}
#' 
#' Using average true range as the volatility measure captures more of the 
#' intraday and overnight volatility in a way that a measurement of Close-to-
#' Close price change does not.
#' 
#' The interpretation is then relatively intuitive - an SNR value of five indicates 
#' that the market has moved five times the volatility (average true range) over 
#' the given look-back period
#'
#' @param HLC Object that is coercible to xts or matrix and contains High-Low-Close prices
#' @param n Number of periods for moving average
#' @param ... parameters passed into \code{\link{ATR}}
#'
#' @return xts time series of signal to noise ratio
#' 
#' @author Peter Carl
#' @references Skeggs, James and Hill, Alex (2015). Back in Black Part 2: The 
#' Opportunity Set for Trend Following.  
#' \url{http://208.75.238.16/content/dam/shared/alternativeedge-snapshots/AlternativeEdge-Snapshot-Back-Black-Trend-Following-Opportunities.pdf}
#' @export
#' 
SNR <- function(HLC, n, ...) {
  require(TTR)
  require(quantmod)
  snr = abs(Cl(HLC) - lag(Cl(HLC), n))/ATR(HLC, n)$atr
  return(snr)
}
