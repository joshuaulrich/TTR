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

#'Bollinger Bands
#'
#'Bollinger Bands are a way to compare a security's volatility and price levels
#'over a period of time.  Developed by John Bollinger.
#'
#'Bollinger Bands consist of three lines:
#'
#'The middle band is generally a 20-period SMA of the typical price ([high +
#'low + close]/3).  The upper and lower bands are \code{sd} standard deviations
#'(generally 2) above and below the MA.
#'
#'The middle band is usually calculated using the typical price, but if a
#'univariate series (e.g. Close, Weighted Close, Median Price, etc.) is
#'provided, it will be used instead.
#'
#'@aliases bollingerBands BBands
#'@param HLC Object that is coercible to xts or matrix and contains
#'High-Low-Close prices.  If only a univariate series is given, it will be
#'used.  See details.
#'@param n Number of periods for moving average.
#'@param maType A function or a string naming the function to be called.
#'@param sd The number of standard deviations to use.
#'@param \dots Other arguments to be passed to the \code{maType} function.
#'@return A object of the same class as \code{HLC} or a matrix (if
#'\code{try.xts} fails) containing the columns:
#' \describe{
#'  \item{ dn }{ The lower Bollinger Band. }
#'  \item{ mavg }{ The middle Moving Average (see notes). }
#'  \item{ up }{ The upper Bollinger Band. }
#'  \item{ pctB }{ The \%B calculation. }
#' }            
#'@note Using any moving average other than SMA will result in inconsistencies
#'between the moving average calculation and the standard deviation
#'calculation.  Since, by definition, a rolling standard deviation uses a
#'simple moving average.
#'@author Joshua Ulrich
#'@seealso See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average
#'options; and note Warning section.
#'@references The following site(s) were used to code/document this
#'indicator:\cr \url{http://www.fmlabs.com/reference/Bollinger.htm}\cr
#'\url{http://www.fmlabs.com/reference/BollingerWidth.htm}\cr
#'\url{http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=36}\cr
#'\url{http://www.linnsoft.com/tour/techind/bb.htm}\cr
#'\url{http://stockcharts.com/education/IndicatorAnalysis/indic_Bbands.html}\cr
#'\url{http://stockcharts.com/education/IndicatorAnalysis/indic_BBWidth.htm}\cr
#'@keywords ts
#'@examples
#'
#' ## The examples below show the differences between using a
#' ## High-Low-Close series, and just a close series when
#' ## calculating Bollinger Bands.
#' data(ttrc)
#' bbands.HLC <- BBands( ttrc[,c("High","Low","Close")] )
#' bbands.close <- BBands( ttrc[,"Close"] )
#'@rdname bollingerBands
#'@export
"BBands" <-
function(HLC, n=20, maType, sd=2, ...) {

  # Bollinger Bands

  HLC <- try.xts(HLC, error=as.matrix)

  if(NCOL(HLC)==3) {
    if(is.xts(HLC)) {
      xa <- xcoredata(HLC)
      HLC <- xts(apply(HLC, 1, mean),index(HLC))
      xcoredata(HLC) <- xa
    } else {
      HLC <- apply(HLC, 1, mean)
    }
  } else
  if(NCOL(HLC)!=1) {
    stop("Price series must be either High-Low-Close, or Close/univariate.")
  }

  maArgs <- list(n=n, ...)
  # Default MA
  if(missing(maType)) {
    maType <- 'SMA'
  }

  mavg  <- do.call( maType, c( list(HLC), maArgs ) )

  # Calculate standard deviation by hand to incorporate various MAs
  sdev   <- runSD(HLC, n, sample=FALSE)

  up     <- mavg + sd * sdev
  dn     <- mavg - sd * sdev
  pctB  <- (HLC - dn) / (up - dn)

  res <- cbind(dn, mavg, up, pctB)
  colnames(res) <- c("dn", "mavg", "up", "pctB")

  reclass(res, HLC)
}
