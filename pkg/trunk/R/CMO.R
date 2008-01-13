#-------------------------------------------------------------------------#
# TTR, copyright (C) Joshua M. Ulrich, 2007                               #
# Distributed under GNU GPL version 3                                     #
#-------------------------------------------------------------------------#

"CMO" <-
function(x, n=14) {

  # Chande Momentum Oscillator

  # http://www.fmlabs.com/reference/CMO.htm

  up <- momentum(x, n=1, na=100)
  dn <- ifelse(up<0, abs(up), 0)
  up <- ifelse(up>0,     up , 0)

  up <- runSum(up, n)
  dn <- runSum(dn, n)

  cmo <- 100 * (up-dn)/(up+dn)
  return( cmo )
}
