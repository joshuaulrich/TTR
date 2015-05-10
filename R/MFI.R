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

#'Money Flow Index
#'
#'The MFI is a ratio of positive and negative money flow over time.
#'
#'Money Flow (MF) is the product of price and volume.  Positive/negative MF
#'occur when today's price is higher/lower than yesterday's price.  The MFI is
#'calculated by dividing positive MF by negative MF for the past \code{n}
#'periods.  It is then scaled between 0 and 100.
#'
#'MFI is usually calculated using the typical price, but if a univariate series
#'(e.g. Close, Weighted Close, Median Price, etc.) is provided, it will be used
#'instead.
#'
#'@aliases MFI moneyFlow
#'@param HLC Object that is coercible to xts or matrix and contains
#'High-Low-Close prices.  If only a univariate series is given, it will be
#'used.  See details.
#'@param volume Vector or matrix of volume observations corresponding to
#'\code{HLC} object.
#'@param n Number of periods to use.
#'@return A object of the same class as \code{HLC} and \code{volume} or a
#'vector (if \code{try.xts} fails) containing the MFI values.
#'@note Divergence between MFI and price can be indicative of a reversal.  In
#'addition, values above/below 80/20 indicate market tops/bottoms.
#'@author Joshua Ulrich
#'@seealso See \code{\link{OBV}} and \code{\link{CMF}}.
#'@references The following site(s) were used to code/document this
#'indicator:\cr
#'\url{http://www.fmlabs.com/reference/default.htm?url=MoneyFlowIndex.htm}\cr
#'\url{http://www.linnsoft.com/tour/techind/mfi.htm}\cr
#'\url{http://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:money_flow_index_mfi}\cr
#'@keywords ts
#'@examples
#'
#' data(ttrc)
#' mfi <- MFI(ttrc[,c("High","Low","Close")], ttrc[,"Volume"])
#'
#'@export
"MFI" <-
function(HLC, volume, n=14) {

  # Money Flow Index

  HLC <- try.xts(HLC, error=as.matrix)
  volume <- try.xts(volume, error=as.matrix)

  if(!(is.xts(HLC) && is.xts(volume))) {
    HLC <- as.matrix(HLC)
    volume <- as.matrix(volume)
  }

  if(NCOL(HLC)==3) {
    if(is.xts(HLC)) {
      HLC <- xts(apply(HLC, 1, mean),index(HLC))
    } else {
      HLC <- apply(HLC, 1, mean)
    }
  } else
  if(NCOL(HLC)!=1) {
    stop("Price series must be either High-Low-Close, or Close/univariate.")
  }

  if(is.xts(HLC)) {
    priceLag <- lag.xts(HLC)
  } else {
    priceLag <- c( NA, HLC[-NROW(HLC)] )
  }

  # Calculate Money Flow
  mf <- HLC * volume
  # Calculate positive and negative Money Flow
  pmf <- ifelse( HLC > priceLag, mf, 0 )
  nmf <- ifelse( HLC < priceLag, mf, 0 )

  # Calculate Money Ratio and Money Flow Index
  mr <- runSum( pmf, n ) / runSum( nmf, n )
  mfi <- 100 - ( 100 / ( 1 + mr ) )
  if(is.xts(mfi)) colnames(mfi) <- 'mfi'

  reclass( mfi, HLC )
}
