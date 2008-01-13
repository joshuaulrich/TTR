#-------------------------------------------------------------------------#
# TTR, copyright (C) Joshua M. Ulrich, 2007                               #
# Distributed under GNU GPL version 3                                     #
#-------------------------------------------------------------------------#

"aroon" <-
function(HL, n=20) {

  # Aroon up, down, and oscillator.

  # http://www.fmlabs.com/reference/Aroon.htm
  # http://www.fmlabs.com/reference/AroonOscillator.htm
  # http://www.linnsoft.com/tour/techind/aroon.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic-Aroon.htm

  HL <- as.matrix(HL)
  aroonUp <- aroonDn <- vector("numeric",NROW(HL))

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
  hmax <- runMax(high, n+1)
  lmin <- runMin( low, n+1)

  # Calculate Aroon UP and DOWN
  for(i in n:NROW(HL)) {
    aroonUp[i] <- 100 * ( ( n - (match(hmax[i], high[i:(i-n)])-1) ) / n )
    aroonDn[i] <- 100 * ( ( n - (match(lmin[i],  low[i:(i-n)])-1) ) / n )
  }

  oscillator <- aroonUp - aroonDn

  return( cbind( aroonUp, aroonDn, oscillator ) )
}
