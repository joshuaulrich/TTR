#-------------------------------------------------------------------------#
# TTR, copyright (C) Joshua M. Ulrich, 2007                               #
# Distributed under GNU GPL version 3                                     #
#-------------------------------------------------------------------------#

"ROC" <-
function(x, n=1, type=c("continuous","discrete"), na=NA) {

  # Rate of Change

  # http://www.fmlabs.com/reference/RateOfChange.htm
  # http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=95
  # http://linnsoft.com/tour/techind/roc.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic_ROC.htm

  x    <- as.vector(x)
  roc  <- vector("numeric", NROW(x))
  #type <- match.arg(type) match.arg only works with 1 arg cleanly -jar

  # Discrete changes
  if(type[1]=="discrete") {
    roc <- c( rep(na,n),      x[(1+n):NROW(x)] / x[1:(NROW(x)-n)] -1 )
  }

  # Continuous changes
  if(type[1]=="continuous") {
    roc <- c( rep(na,n), log( x[(1+n):NROW(x)] / x[1:(NROW(x)-n)] )  )
  }

  return( roc )
}

#-------------------------------------------------------------------------#

"momentum" <-
function(x, n=1, na=NA) {

  # Momentum

  # http://www.fmlabs.com/reference/Momentum.htm
  # http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=95
  # http://linnsoft.com/tour/techind/momentum.htm

  mom <- c( rep(na, n), diff(x, n) )

  return( mom )
}
