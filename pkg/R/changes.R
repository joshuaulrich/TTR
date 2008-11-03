#-------------------------------------------------------------------------#
# TTR, copyright (C) Joshua M. Ulrich, 2007                               #
# Distributed under GNU GPL version 3                                     #
#-------------------------------------------------------------------------#

"ROC" <-
function(x, n=1, type=c("continuous","discrete"), na.pad=TRUE) {

  # Rate of Change

  # http://www.fmlabs.com/reference/RateOfChange.htm
  # http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=95
  # http://linnsoft.com/tour/techind/roc.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic_ROC.htm

  x <- try.xts(x, error=FALSE)
  type <- match.arg(type)

  if(is.xts(x)) {
    if(type=="discrete") {
      roc <- x / lag(x,n,na.pad=na.pad) - 1
    }
    # Continuous change
    if(type=="continuous") {
      roc <- diff(log(x),n,na.pad=na.pad)
    }
    # Convert back to original class
    reclass(roc, x)
  } else {
    NAs <- NULL
    if(na.pad) {
      NAs <- rep(NA,n)
    }
    # Discrete changes
    if(type=="discrete") {
      roc <- c( NAs, x[-1] / x[-NROW(x)] - 1 )
    }
    # Continuous changes
    if(type=="continuous") {
      roc <- c( NAs, diff(log(x),n) )
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
