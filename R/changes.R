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

  x <- try.xts(x, error=FALSE)
  type <- match.arg(type)

  if(is.xts(x)) {
    if(type=="discrete") {
      roc <- c( rep(na,n), x/lag(x,-n)-1 )
    }
    # Continuous changes
    if(type=="continuous") {
      roc <- c( rep(na,n), log( x/lag(x,-n) ) )
    }
    # Convert back to original class
    reclass(roc, x)
  } else {
    # Discrete changes
    if(type=="discrete") {
      roc <- c( rep(na,n), x[(1+n):NROW(x)] / x[1:(NROW(x)-n)] -1 )
    }
    # Continuous changes
    if(type=="continuous") {
      roc <- c( rep(na,n), log( x[(1+n):NROW(x)] / x[1:(NROW(x)-n)] )  )
    }
    return(roc)
  }
}

#-------------------------------------------------------------------------#

"momentum" <-
function(x, n=1, na.pad=TRUE) {

  # Momentum

  # http://www.fmlabs.com/reference/Momentum.htm
  # http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=95
  # http://linnsoft.com/tour/techind/momentum.htm
  
  x <- try.xts(x, error=FALSE)
  if(is.xts(x)) {
    mom <- diff(x,n,na.pad=na.pad)
  } else {
    NAs <- NULL
    if(na.pad) {
      NAs <- rep(NA,n)
    }
    mom <- c( NAs, diff(x, n) )
  }
  reclass(mom,x)
}
