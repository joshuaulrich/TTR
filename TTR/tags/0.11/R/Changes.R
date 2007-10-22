"ROC" <-
function(x, n=1, type=c("discrete","continuous"), na=NA) {

  # Rate of Change

  # http://www.fmlabs.com/reference/RateOfChange.htm
  # http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=95
  # http://linnsoft.com/tour/techind/roc.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic_ROC.htm

  x    <- as.vector(x)
  roc  <- vector("numeric", NROW(x))

  if(missing(type))
    type <- substr(type[1],1,1)  else
    type <- substr(type,1,1)

  # Discrete changes
  if(type=="d") {
    roc <- c( rep(na,n),      x[(1+n):NROW(x)] / x[1:(NROW(x)-n)] -1 )
  } else

  # Continuous changes
  if(type=="c") {
    roc <- c( rep(na,n), log( x[(1+n):NROW(x)] / x[1:(NROW(x)-n)] )  )
  } else

  stop("Invalid compounding type \n",
       "Please choose: type = \"continuous\", or type = \"discrete\"")

  return( roc )
}

# -----------------------------------------------------------

"momentum" <-
function(x, n=1, na=NA) {

  # Momentum

  # http://www.fmlabs.com/reference/Momentum.htm
  # http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=95
  # http://linnsoft.com/tour/techind/momentum.htm

  mom <- c( rep(na, n), diff(x, n) )

  return( mom )
}