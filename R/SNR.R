#
#   TTR: Technical Trading Rules
#
#   Copyright (C) 2016  Peter Carl, Joshua M. Ulrich
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 2 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

#'Signal to Noise Ratio
#'
#'The n-day SNR for a given market is calculated by taking the absolute
#'price change over an n-day period and dividing it by the average
#'n-day volatility.
#'
#'\deqn{SNR_n = \frac{|C_t - C_{t-n}|}{ATR_n}
#'}{SNR = abs(Cl - lag(Cl,n)) / ATR(HLC, n)$atr}
#'
#'Using average true range as the volatility measure captures more of the
#'intraday and overnight volatility in a way that a measurement of
#'Close-to-Close price change does not.
#'
#'The interpretation is then relatively intuitive: an SNR value of five
#'indicates that the market has moved five times the volatility (average true
#'range) over the given look-back period.
#'
#'@param HLC Object that is coercible to xts or matrix and contains
#'High-Low-Close prices.
#'@param n Number of periods for moving average.
#'@param ... Other arguments to be passed to \code{\link{ATR}}.
#'@return A object of the same class as HLC or a matrix (if try.xts fails)
#'containing the signal to noise ratio.
#'@author Peter Carl
#'@references Skeggs, James and Hill, Alex (2015). Back in Black Part 2: The 
#'Opportunity Set for Trend Following.  
#' 
#'@export
SNR <- function(HLC, n, ...) {
  HLC <- try.xts(HLC, error=as.matrix)

  snr <- abs(HLC[,3] - lag.xts(HLC[,3], n)) / ATR(HLC, n, ...)$atr

  return(reclass(snr, HLC))
}
