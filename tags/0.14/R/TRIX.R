#-------------------------------------------------------------------------#
# TTR, copyright (C) Joshua M. Ulrich, 2007                               #
# Distributed under GNU GPL version 3                                     #
#-------------------------------------------------------------------------#

"TRIX" <-
function(price, n=20, nSig=9, maType="EMA", percent=TRUE, ...) {

  # Triple Smoothed Exponential Oscillator

  # http://www.fmlabs.com/reference/default.htm?url=TRIX.htm
  # http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=114
  # http://www.linnsoft.com/tour/techind/trix.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic_trix.htm

  price  <- as.matrix(price)

  # Case of two different 'maType's for both MAs.
  if( is.list(maType) ) {

    # If MA function has 'n' arg, see if it's populated in maType;
    # if it isn't, populate it with function's formal 'n'
    if( !is.null( formals(maType[[1]])$n ) && is.null( maType[[1]]$n ) ) {
      maType[[1]]$n <- n
    }
    if( !is.null( formals(maType[[2]])$n ) && is.null( maType[[2]]$n ) ) {
      maType[[2]]$n <- n
    }
    if( !is.null( formals(maType[[3]])$n ) && is.null( maType[[3]]$n ) ) {
      maType[[3]]$n <- n
    }
    if( !is.null( formals(maType[[4]])$n ) && is.null( maType[[4]]$n ) ) {
      maType[[4]]$n <- nSig
    }
    
    mavg1 <- do.call( maType[[1]][[1]], c( list(price), maType[[1]][-1] ) )
    mavg2 <- do.call( maType[[2]][[1]], c( list(mavg1), maType[[2]][-1] ) )
    mavg3 <- do.call( maType[[3]][[1]], c( list(mavg2), maType[[3]][-1] ) )

  }
  
  # Case of one 'maType' for both MAs.
  else {
  
    mavg1 <- do.call( maType, c( list(price), list(n=n, ...) ) )
    mavg2 <- do.call( maType, c( list(mavg1), list(n=n, ...) ) )
    mavg3 <- do.call( maType, c( list(mavg2), list(n=n, ...) ) )

  }

  if(percent) {
    TRIX <- 100 * ROC(mavg3, n=1, na=NA, type="discrete")
  } else {
    TRIX <- momentum( mavg3, n=1, na=NA )
  }
  
  if( is.list(maType) ) {
    signal <- do.call( maType[[4]][[1]], c( list(TRIX), maType[[4]][-1] ) )
  } else {
    signal <- do.call( maType, c( list(TRIX), list(n=n, ...) ) )
  }

  return( cbind( TRIX, signal ) )
}
