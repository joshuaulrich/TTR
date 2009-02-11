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

"SAR" <-
function(HL, accel=c(.02,.2)) {

  # Parabolic Stop-and-Reverse (SAR)
  # ----------------------------------------------
  #       HL = HL vector, matrix, or dataframe
  # accel[1] = acceleration factor
  # accel[2] = maximum acceleration factor

  # http://www.linnsoft.com/tour/techind/sar.htm
  # http://www.fmlabs.com/reference/SAR.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic_ParaSAR.htm
  # http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=87

  # WISHLIST:
  # Determine signal based on DM+/DM- for first bar
  # If sig[1]==1, then ep[1]==high; if sig[1]==-1, then ep[1]==low
  # The first SAR value should be the opposite (high/low) of ep
  # The first acceleration factor is based on the first signal

  # Since I've already lost one bar, do what TA-lib does and use that bar to
  # determine the inital signal value.  Also try to incorporate different
  # accel factors for long/short.
  # accel = c( long = c( 0.02, 0.2 ), short = long )

  HL <- try.xts(HL, error=as.matrix)
  HL.na <- naCheck(HL, 0)

  # Initialize necessary vector
  sar <- rep(0,NROW(HL.na$nonNA))

  sar <- .Fortran("psar", iha = as.double( HL[HL.na$nonNA,1] ),
                          ila = as.double( HL[HL.na$nonNA,2] ),
                          la  = as.integer( NROW( HL.na$nonNA ) ),
                          af  = as.double( accel[1] ),
                          maf = as.double( accel[2] ),
                          sar = as.double( sar[] ),
                          PACKAGE = "TTR",
                          DUP = FALSE )$sar
  
  # Prepend NAs from original data
  sar <- c( rep( NA, HL.na$NAs ), sar ) 

  reclass( sar, HL )
}
