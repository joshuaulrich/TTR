"TDI" <-
function(price, n=20) {

  # Trend Detection Index

  # http://www.linnsoft.com/tour/techind/tdi.htm

  mom <- momentum(price, n=n, na=0)

  av.1n <- abs( roll.fn(mom, n,   FUN="sum") )
  am.2n <- roll.fn(abs(mom), n*2, FUN="sum")
  am.1n <- roll.fn(abs(mom), n  , FUN="sum")

  tdi <- av.1n - (am.2n - am.1n)
  di  <- roll.fn(mom, n, FUN="sum")

  return( cbind( tdi,di ) )
}

