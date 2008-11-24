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

  HL <- try.xts(HL, error=FALSE)

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

  # Find max and min of price series over past (n+1) days
  # It must be (n+1) to cover today, and the past n days
  hmax <- runMax(high, n+1)
  lmin <- runMin( low, n+1)

  # Calculate Aroon UP and DOWN
  for(i in (n+1):NROW(HL)) {
    aroonUp[i] <- 100 * ( max((0:n)[high[(i-n):i] %in% hmax[i]]) / n )
    aroonDn[i] <- 100 * ( max((0:n)[ low[(i-n):i] %in% lmin[i]]) / n )
  }

  oscillator <- aroonUp - aroonDn

  # Convert back to original class
  reclass( cbind( aroonUp, aroonDn, oscillator ), HL )
}
