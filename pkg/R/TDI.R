#-------------------------------------------------------------------------#
# TTR, copyright (C) Joshua M. Ulrich, 2007                               #
# Distributed under GNU GPL version 3                                     #
#-------------------------------------------------------------------------#

"TDI" <-
function(price, n=20, multiple=2) {

  # Trend Detection Index

  # http://www.linnsoft.com/tour/techind/tdi.htm

  mom <- momentum(price, n, na.pad=TRUE)
  mom[is.na(mom)] <- 0

  di  <- runSum(mom, n)
  abs.di <- abs(di)

  abs.mom.2n <- runSum(abs(mom), n*multiple)
  abs.mom.1n <- runSum(abs(mom), n  )

  tdi <- abs.di - (abs.mom.2n - abs.mom.1n)

  return( cbind( tdi,di ) )
}
