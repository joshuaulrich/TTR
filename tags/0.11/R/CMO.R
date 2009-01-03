"CMO" <-
function(x, n=14) {

  # Chande Momentum Oscillator

  # http://www.fmlabs.com/reference/CMO.htm

  up <- momentum(x, n=1, na=100)
  dn <- ifelse(up<0, abs(up), 0)
  up <- ifelse(up>0,     up , 0)

  up <- roll.fn(up, n, FUN="sum")
  dn <- roll.fn(dn, n, FUN="sum")

  cmo <- 100 * (up-dn)/(up+dn)
  return( cmo )
}
