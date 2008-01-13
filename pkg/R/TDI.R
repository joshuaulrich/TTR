#-------------------------------------------------------------------------#
# TTR, copyright (C) Joshua M. Ulrich, 2007                               #
# Distributed under GNU GPL version 3                                     #
#-------------------------------------------------------------------------#

"TDI" <-
function(price, n=20) {

  # Trend Detection Index

  # http://www.linnsoft.com/tour/techind/tdi.htm

  mom <- momentum(price, n=n, na=0)

  av.1n <- abs( runSum(mom, n  ) )
  am.2n <- runSum(abs(mom), n*2)
  am.1n <- runSum(abs(mom), n  )

  tdi <- av.1n - (am.2n - am.1n)
  di  <- runSum(mom, n)

  return( cbind( tdi,di ) )
}
