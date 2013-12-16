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

#'DV Intermediate Oscillator
#'
#'The DV Intermediate oscillator (DVI) is a very smooth momentum oscillator
#'that can also be used as a trend indicator.  Created by David Varadi.
#'
#'The DVI combines smoothed returns over different time windows and the
#'relative number of up versus down days (stretch) over different time windows.
#'
#'@param price Price series that is coercible to xts or matrix.
#'@param n Number of periods for the percent rank.
#'@param wts The weight given to the smoothed returns (magnitude) component and
#'the up/down days (stretch) component, respectively.
#'@param smooth The number of periods to smooth price.
#'@param magnitude A set of 3 periods used to smooth magnitude.
#'@param stretch A set of 3 periods used to smooth stretch.
#'@param exact.multiplier The weight applied to identical values in the window.
#'See \code{runPercentRank}.
#'@return A object of the same class as \code{price} or a vector (if
#'\code{try.xts} fails) containing the DVI values.
#'@author Joshua Ulrich
#'@references The following site(s) were used to code/document this
#'indicator:\cr
#'\url{http://cssanalytics.wordpress.com/2009/12/13/what-is-the-dvi/}\cr
#'\url{http://marketsci.wordpress.com/2010/07/27/css-analytics\%E2\%80\%99-dvi-indicator-revealed/}\cr
#'@keywords ts
#'@examples
#'
#' data(ttrc)
#' dvi <- DVI(ttrc[,"Close"])
#'
#'@export
DVI <- function(price, n=252, wts=c(0.8,0.2), smooth=3,
  magnitude=c(5,100,5), stretch=c(10,100,2), exact.multiplier=1) {

  # David Varadi's DVI indicator

  # try to convert 'price' to xts
  price <- try.xts(price, error=as.matrix)

  # ensure magnitude + stretch = 1
  wts.sum <- sum(wts)
  wts[1] <- wts[1] / wts.sum
  wts[2] <- wts[2] / wts.sum

  # calculate magnitude, based on average price return
  r <- price/SMA(price,smooth)-1
  mag <- SMA( ( SMA(r,magnitude[1]) + SMA(r,magnitude[2])/10 )/2, magnitude[3] )

  # calculate stretch, based on whether return is +/-
  b <- ifelse( price > lag.xts(price), 1, -1 )
  str <- SMA( ( runSum(b,stretch[1]) + runSum(b,stretch[2])/10 )/2, stretch[3] )


  # calculate the DVI magnitude and stretch for each period
  dvi.mag <- runPercentRank(mag, n, FALSE, exact.multiplier)
  dvi.str <- runPercentRank(str, n, FALSE, exact.multiplier)

  # calculate final DVI value
  dvi <- wts[1] * dvi.mag + wts[2] * dvi.str

  # convert final DVI, magnitude, and stretch back to
  # original class of 'price'
  reclass(cbind(dvi.mag,dvi.str,dvi), price)
}

