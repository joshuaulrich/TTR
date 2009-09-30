#
#   TTR: Technical Trading Rules
#
#   Copyright (C) 2007-2008  Joshua M. Ulrich
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
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

"ZigZag" <- 
function( HL, change=10, percent=TRUE, retrace=FALSE, lastExtreme=TRUE ) {

  # Zig Zag Indicator
  # Adapted from Alberto Santini's code

  # http://www.fmlabs.com/reference/default.htm?url=ZigZag.htm
  # http://www.linnsoft.com/tour/techind/zigzag.htm
  # http://www.linnsoft.com/tour/techind/zigosc.htm
  # http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=127
  # http://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:zigzag

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
                           DUP = FALSE )$zz
  
  # Interpolate results
  zz <- ifelse( zz==0, NA, zz )
  zz <- approx( zz, xout=1:nn )$y

  # Prepend NAs from original data
  zz <- c( rep( NA, HL.na$NAs ), zz ) 

  reclass( zz, HL )
}

