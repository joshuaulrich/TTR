"VHF" <-
function(price, n=28) {

  # Vertical Horizontal Filter

  # http://www.fmlabs.com/reference/VHF.htm
  # http://www.equis.com/Customer/Resources/TAAZ?c=3&p=119

  price <- as.matrix(price)

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
  hmax  <- rollFUN( high, n, FUN="max")
  lmin  <- rollFUN(  low, n, FUN="min")
  denom <- momentum(close, n=1, na=0)

  VHF <- ( hmax - lmin ) / rollFUN(denom, n, FUN="sum")

  return( VHF )

}
