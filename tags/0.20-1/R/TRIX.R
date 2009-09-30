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

"TRIX" <-
function(price, n=20, nSig=9, maType, percent=TRUE, ...) {

  # Triple Smoothed Exponential Oscillator

  # http://www.fmlabs.com/reference/default.htm?url=TRIX.htm
  # http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=114
  # http://www.linnsoft.com/tour/techind/trix.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic_trix.htm

  # Default MA
  if(missing(maType)) {
    maType <- 'EMA'
  }

  # Case of different 'maType's for all MAs.
  if( is.list(maType) ) {

    # Make sure maType is a list of lists
    maTypeInfo <- sapply(maType,is.list)
    if( !(all(maTypeInfo) && length(maTypeInfo) == 4) ) {
      stop("If \'maType\' is a list, you must specify\n ",
      "*four* MAs (see Examples section of ?TRIX)")
    }

    # If MA function has 'n' arg, see if it's populated in maType;
    # if it isn't, populate it with function's formal 'n'
    if( !is.null( formals(maType[[1]][[1]])$n ) && is.null( maType[[1]]$n ) ) {
      maType[[1]]$n <- n
    }
    if( !is.null( formals(maType[[2]][[1]])$n ) && is.null( maType[[2]]$n ) ) {
      maType[[2]]$n <- n
    }
    if( !is.null( formals(maType[[3]][[1]])$n ) && is.null( maType[[3]]$n ) ) {
      maType[[3]]$n <- n
    }
    if( !is.null( formals(maType[[4]][[1]])$n ) && is.null( maType[[4]]$n ) ) {
      maType[[4]]$n <- nSig
    }
    
    mavg1 <- do.call( maType[[1]][[1]], c( list(price), maType[[1]][-1] ) )
    mavg2 <- do.call( maType[[2]][[1]], c( list(mavg1), maType[[2]][-1] ) )
    mavg3 <- do.call( maType[[3]][[1]], c( list(mavg2), maType[[3]][-1] ) )

  }
  
  # Case of one 'maType' for all MAs.
  else {
  
    mavg1 <- do.call( maType, c( list(price), list(n=n, ...) ) )
    mavg2 <- do.call( maType, c( list(mavg1), list(n=n, ...) ) )
    mavg3 <- do.call( maType, c( list(mavg2), list(n=n, ...) ) )

  }

  if(percent) {
    TRIX <- 100 * ROC(mavg3, n=1, na.pad=TRUE, type="discrete")
  } else {
    TRIX <- momentum( mavg3, n=1, na.pad=TRUE )
  }
  
  if( is.list(maType) ) {
    signal <- do.call( maType[[4]][[1]], c( list(TRIX), maType[[4]][-1] ) )
  } else {
    signal <- do.call( maType, c( list(TRIX), list(n=n, ...) ) )
  }

  result <- cbind( TRIX, signal )
  colnames(result) <- c( "TRIX", "signal" )

  return( result )
}
