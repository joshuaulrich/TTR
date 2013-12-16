#
#   TTR: Technical Trading Rules
#
#   Copyright (C) 2007-2013  Joshua M. Ulrich
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

#'The Ultimate Oscillator
#'
#'The Ultimate Oscillator is a momentum oscillator designed to capture momentum across three
#'different time frames.
#' 
#'Created by Larry Williams in 1976.
#' 
#'@param HLC Object that is coercible to xts or matrix and contains
#'High-Low-Close prices.
#'@param n A vector of the number of periods to use for each average calculation.
#'@param wts The weights applied to each average.
#'@author Ivan Popivanov
#'@references The following site(s) were used to code/document this
#'indicator:\cr
#'\url{http://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:ultimate_oscillator}\cr
#'@keywords ts
#'@examples
#'
#'data(ttrc)
#'ult.osc <- ultimateOscillator(ttrc[,c("High","Low","Close")])
#'
#'@export
ultimateOscillator <-
function(HLC, n=c(7,14,28), wts=c(4,2,1)) {

  # Ultimate Oscillator

  if(length(n) != 3 || length(wts) != 3)
    stop("length(n) and length(wts) must both be 3")

  HLC <- try.xts(HLC, error=as.matrix)

  # avoid reclassing in ATR and runSum
  HLC.RECLASS <- attr(HLC, ".RECLASS")
  attr(HLC, ".RECLASS") <- FALSE

  # only need 'tr' and 'trueLow'
  atr <- ATR(HLC, n=1)

  buyPressure <- HLC[,3] - atr[,'trueLow']

  avgs <- sapply(n, function(i) runSum(buyPressure, n=i)/runSum(atr[,'tr'], n=i))

  # restore HLC .RECLASS attribute
  attr(HLC, ".RECLASS") <- HLC.RECLASS

  osc <- 100.0*(wts[1]*avgs[,1] + wts[2]*avgs[,2] + wts[3]*avgs[,3]) / sum(wts)

  reclass(osc, HLC)
}
