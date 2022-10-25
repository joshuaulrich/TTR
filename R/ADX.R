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

#'Welles Wilder's Directional Movement Index
#'
#'Directional Movement Index; developed by J. Welles Wilder.
#'
#'The \code{DIp}/\code{DIn} (positive/negative) is the percentage of the true
#'range that is up/down.
#'
#'@aliases ADX DI DX
#'@param HLC Object that is coercible to xts or matrix and contains
#'High-Low-Close prices.
#'@param n Number of periods to use for DX calculation (not ADX calculation).
#'@param maType A function or a string naming the function to be called.
#'@param \dots Other arguments to be passed to the \code{maType} function.
#'@return A object of the same class as \code{HLC} or a matrix (if
#'\code{try.xts} fails) containing the columns:
#' \describe{
#'  \item{ DIp }{ The positive Direction Index. }
#'  \item{ DIn }{ The negative Direction Index. }
#'  \item{ DX }{ The Direction Index. }
#'  \item{ ADX }{ The Average Direction Index (trend strength). }
#' }
#'@note A buy/sell signal is generated when the +/-DI crosses up over the
#'-/+DI, when the DX/ADX signals a strong trend.  A high/low DX signals a
#'strong/weak trend.  DX is usually smoothed with a moving average (i.e. the
#'ADX).
#'@author Joshua Ulrich
#'@seealso See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average
#'options; and note Warning section.  The DX calculation uses
#'\code{\link{ATR}}.  See \code{\link{aroon}}, \code{\link{CCI}},
#'\code{\link{TDI}}, \code{\link{VHF}}, \code{\link{GMMA}} for other indicators
#'that measure trend direction/strength.
#'@references The following site(s) were used to code/document this
#'indicator:\cr \url{https://www.fmlabs.com/reference/DI.htm}\cr
#'\url{https://www.fmlabs.com/reference/DX.htm}\cr
#'\url{https://www.fmlabs.com/reference/ADX.htm}\cr
#'\url{https://www.fmlabs.com/reference/ADXR.htm}\cr
#'\url{https://www.metastock.com/Customer/Resources/TAAZ/?p=49}\cr
#'\url{https://www.linnsoft.com/techind/directional-indicator-diplus-diminus}\cr
#'\url{https://www.linnsoft.com/techind/adx-avg-directional-movement}\cr
#'\url{https://www.linnsoft.com/techind/adxr-avg-directional-movement-rating}\cr
#'\url{https://school.stockcharts.com/doku.php?id=technical_indicators:average_directional_index_adx}\cr
#'@keywords ts
#'@examples
#'
#' data(ttrc)
#' dmi.adx <- ADX(ttrc[,c("High","Low","Close")])
#'
"ADX" <-
function(HLC, n=14, maType, ...) {

  # Welles Wilder's Directional Movement Index

  HLC <- try.xts(HLC, error=as.matrix)
  dH  <- momentum(HLC[,1])
  dL  <- -momentum(HLC[,2])

  DMIp <- ifelse( dH==dL | (dH< 0 & dL< 0), 0, ifelse( dH >dL, dH, 0 ) )
  DMIn <- ifelse( dH==dL | (dH< 0 & dL< 0), 0, ifelse( dH <dL, dL, 0 ) )

  tr    <- TR(HLC)[,"tr"]
  TRsum <- wilderSum(tr, n=n)

  DIp <- 100 * wilderSum(DMIp, n=n) / TRsum
  DIn <- 100 * wilderSum(DMIn, n=n) / TRsum

  DX  <- 100 * ( abs(DIp - DIn) / (DIp + DIn) )

  maArgs <- list(n=n, ...)
  
  # Default Welles Wilder EMA
  if(missing(maType)) {
    maType <- 'EMA'
    if(is.null(maArgs$wilder)) {
      # do not overwrite user-provided value
      maArgs$wilder <- TRUE
    }
  }

  ADX <- do.call( maType, c( list(DX), maArgs ) )

  result <- cbind( DIp, DIn, DX, ADX )
  colnames(result) <- c( "DIp", "DIn", "DX", "ADX" )

  reclass(result, HLC)
}
