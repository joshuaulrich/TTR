#-------------------------------------------------------------------------#
# TTR, copyright (C) Joshua M. Ulrich, 2007                               #
# Distributed under GNU GPL version 3                                     #
#-------------------------------------------------------------------------#

"VHF" <-
function(price, n=28) {

  # Vertical Horizontal Filter

  # http://www.fmlabs.com/reference/VHF.htm
  # http://www.equis.com/Customer/Resources/TAAZ?c=3&p=119

  #price <- as.matrix(price)

  # Calculation if price series is given
  if(NCOL(price)==1) {
    high  <- price
    low   <- price
    close <- price
  } else

  # Calculation if HLC series is given
  if(NCOL(price)==3) {
    high  <- price[,1]
    low   <- price[,2]
    close <- price[,3]
  } else

  stop("Price series must be either Close, or High-Low-Close")

  # Find highest max, and lowest min of price series
  hmax  <- runMax( high, n)
  lmin  <- runMin(  low, n)
  denom <- momentum(close, n=1, na.pad=TRUE)

  VHF <- ( hmax - lmin ) / runSum(denom, n)

  return( VHF )
}
