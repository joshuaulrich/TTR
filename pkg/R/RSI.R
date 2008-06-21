#-------------------------------------------------------------------------#
# TTR, copyright (C) Joshua M. Ulrich, 2007                               #
# Distributed under GNU GPL version 3                                     #
#-------------------------------------------------------------------------#

"RSI" <- 
function(price, n=14, maType, ...) {

  # Relative Strength Index

  # http://www.fmlabs.com/reference/RSI.htm
  # http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=100
  # http://linnsoft.com/tour/techind/rsi.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic_RSI.html

  # Stochastic RSI

  # http://www.fmlabs.com/reference/StochRSI.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic_stochRSI.html

  up <- momentum(price, n=1, na=NA)
  dn <- ifelse(up<0, abs(up), 0)
  up <- ifelse(up>0,     up , 0)

  maArgs <- list(n=n, ...)
  # Default Welles Wilder EMA
  if(missing(maType)) {
    maType <- 'EMA'
    maArgs$wilder <- TRUE
  }

  # Case of two different 'maType's for both MAs.
  # e.g. RSI(price, n=14, maType=list(maUp=list(EMA,ratio=1/5), maDown=list(WMA,wts=1:10)) )
  if( is.list(maType) ) {

    # If MA function has 'n' arg, see if it's populated in maType;
    # if it isn't, populate it with RSI's formal 'n'
    for(i in 1:length(maType)) {
      if( !is.null( formals(maType[[i]])$n ) && is.null( maType[[i]]$n ) ) {
        maType[[i]]$n <- n
      }
      mavgUp <- do.call( maType[[1]][[1]], c( list(up), maType[[1]][-1] ) )
      mavgDn <- do.call( maType[[2]][[1]], c( list(dn), maType[[2]][-1] ) )
    }
  }
  
  # Case of one 'maType' for both MAs.
  # e.g. RSI(price, n=14, maType="WMA", wts=volume )
  else {
  
    mavgUp <- do.call( maType, c( list(up), maArgs ) )
    mavgDn <- do.call( maType, c( list(dn), maArgs ) )
  }

  rsi <- 100 * mavgUp / ( mavgUp + mavgDn )

  return( rsi )
}
