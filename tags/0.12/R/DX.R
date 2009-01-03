"DX" <-
function(HLC, n=14, ma.adx=list("EMA", n=n, wilder=TRUE)) {

  # Welles Wilder's Directional Movement Index

  # http://www.fmlabs.com/reference/DI.htm
  # http://www.fmlabs.com/reference/DX.htm
  # http://www.fmlabs.com/reference/ADX.htm
  # http://www.fmlabs.com/reference/ADXR.htm
  # http://www.equis.com/Customer/Resources/TAAZ/Default.aspx?c=3&p=49
  # http://linnsoft.com/tour/techind/dirInd.htm
  # http://linnsoft.com/tour/techind/adx.htm
  # http://linnsoft.com/tour/techind/adxr.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic_ADX.html

  HLC <- as.matrix(HLC)
  dH  <- HLC[,1] - c(0, HLC[-NROW(HLC),1])
  dL  <- c(0, HLC[-NROW(HLC),2]) - HLC[,2]

  DMIp <- ifelse( dH==dL | (dH< 0 & dL< 0), 0, ifelse( dH >dL, dH, 0 ) )
  DMIn <- ifelse( dH==dL | (dH< 0 & dL< 0), 0, ifelse( dH <dL, dL, 0 ) )

  TR    <- ATR(HLC)[,"tr"]
  TRsum <- wilder.sum(TR, n=n)

  DIp <- 100 * wilder.sum(DMIp, n=n) / TRsum
  DIn <- 100 * wilder.sum(DMIn, n=n) / TRsum

  DX  <- 100 * ( abs(DIp - DIn) / (DIp + DIn) )

  ADX <- do.call( ma.adx[[1]], c( list(DX), ma.adx[-1] ) )

  return( cbind( DIp, DIn, DX, ADX ) )
}
