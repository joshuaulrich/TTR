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

#'Zig Zag
#'
#'Zig Zag higlights trends by removing price changes smaller than \code{change}
#'and interpolating lines between the extreme points.
#'
#'The Zig Zag is non-predictive.  The purpose of the Zig Zag is filter noise
#'and make chart patterns clearer.  It's more a visual tool than an indicator.
#'
#'@aliases ZigZag zigzag
#'@param HL Object that is coercible to xts or matrix and contains either a
#'High-Low price series, or a Close price series.
#'@param change Minimum price movement, either in dollars or percent (see
#'\code{percent}).
#'@param percent Use percentage or dollar change?
#'@param retrace Is \code{change} a retracement of the previous move, or an
#'absolute change from peak to trough?
#'@param lastExtreme If the extreme price is the same over multiple periods,
#'should the extreme price be the first or last observation?
#'@return A object of the same class as \code{HL} or a vector (if
#'\code{try.xts} fails) containing the Zig Zag indicator.
#'@note If High-Low prices are given, the function calculates the max/min using
#'the high/low prices.  Otherwise the function calculates the max/min of the
#'single series.
#'@author Joshua Ulrich
#'@references The following site(s) were used to code/document this
#'indicator:\cr
#'\url{http://www.fmlabs.com/reference/default.htm?url=ZigZag.htm}\cr
#'\url{http://www.linnsoft.com/tour/techind/zigzag.htm}\cr
#'\url{http://www.linnsoft.com/tour/techind/zigosc.htm}\cr
#'\url{http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=127}\cr
#'\url{http://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:zigzag}\cr
#'@keywords ts
#'@examples
#'
#' ## Get Data and Indicator ##
#' data(ttrc)
#' zz <- ZigZag( ttrc[,c("High", "Low")], change=20 )
#'
#'@export
"ZigZag" <- 
function( HL, change=10, percent=TRUE, retrace=FALSE, lastExtreme=TRUE ) {

  # Zig Zag Indicator
  # Adapted from Alberto Santini's code

  HL <- try.xts(HL, error=as.matrix)
  HL.na <- naCheck(HL,0)
  
  # Calculation if HL series is given
  if(NCOL(HL)==2) {
    high  <- HL[HL.na$nonNA,1]
    low   <- HL[HL.na$nonNA,2]
  } else

  # Calculation if price vector is given
  if(NCOL(HL.na)==1) {
    high  <- HL[HL.na$nonNA]
    low   <- HL[HL.na$nonNA]
  } else

  stop("Price series must be either High-Low, or Univariate")
  
  # Initialize necessary parameters
  nn <- NROW(HL.na$nonNA)
  zz <- rep(0, nn)
  if(percent) {
    change <- change/100
  }

  # Call Fortran routine
  zz <- .Fortran("zigzag", iha = as.double( high ),
                           ila = as.double( low ),
                           la  = as.integer( nn ),
                           ch  = as.double( change ),
                           pct = as.integer( percent ),
                           rtr = as.integer( retrace ),
                           lex = as.integer( lastExtreme ),
                           zz  = as.double( zz ),
                           PACKAGE = "TTR",
                           DUP = TRUE )$zz
  
  # Interpolate results
  zz <- ifelse( zz==0, NA, zz )
  zz <- approx( zz, xout=1:nn )$y

  # Prepend NAs from original data
  zz <- c( rep( NA, HL.na$NAs ), zz ) 

  reclass( zz, HL )
}

