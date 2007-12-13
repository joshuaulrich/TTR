"aroon" <-
function(HL, n=20) {

  # Aroon up, down, and oscillator.

  # http://www.fmlabs.com/reference/Aroon.htm
  # http://www.fmlabs.com/reference/AroonOscillator.htm
  # http://www.linnsoft.com/tour/techind/aroon.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic-Aroon.htm

  HL <- as.matrix(HL)
  aroon.up <- aroon.dn <- vector("numeric",NROW(HL))

  # Calculation if price vector is given
  if(NCOL(HL)==1) {
    high <- HL
    low  <- HL
  } else

  # Calculation if HL series is given
  if(NCOL(HL)==2) {
    high <- HL[,1]
    low  <- HL[,2]
  } else

  stop("Price series must be either High-Low, or Close")

  # Find max and min of price series
  hmax <- rollFUN(high, n+1, FUN="max")
  lmin <- rollFUN( low, n+1, FUN="min")

  # Calculate Aroon UP and DOWN
  for(i in n:NROW(HL)) {
    aroon.up[i] <- 100 * ( ( n - (match(hmax[i], high[i:(i-n)])-1) ) / n )
    aroon.dn[i] <- 100 * ( ( n - (match(lmin[i],  low[i:(i-n)])-1) ) / n )
  }

  oscillator <- aroon.up - aroon.dn

  return( cbind( aroon.up, aroon.dn, oscillator ) )
}
