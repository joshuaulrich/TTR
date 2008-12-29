#-------------------------------------------------------------------------#
# TTR, copyright (C) Joshua M. Ulrich, 2007                               #
# Distributed under GNU GPL version 3                                     #
#-------------------------------------------------------------------------#

"ADX" <-
function(HLC, n=14, maType, ...) {

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

  HLC <- try.xts(HLC, error=FALSE)
  dH  <- momentum(HLC[,1])
  dL  <- -momentum(HLC[,2])

  DMIp <- ifelse( dH==dL | (dH< 0 & dL< 0), 0, ifelse( dH >dL, dH, 0 ) )
  DMIn <- ifelse( dH==dL | (dH< 0 & dL< 0), 0, ifelse( dH <dL, dL, 0 ) )

  TR    <- ATR(HLC)[,"tr"]
  TRsum <- wilderSum(TR, n=n)

  DIp <- 100 * wilderSum(DMIp, n=n) / TRsum
  DIn <- 100 * wilderSum(DMIn, n=n) / TRsum

  DX  <- 100 * ( abs(DIp - DIn) / (DIp + DIn) )

  maArgs <- list(n=n, ...)
  
  # Default Welles Wilder EMA
  if(missing(maType)) {
    maType <- 'EMA'
    maArgs$wilder <- TRUE
  }

  ADX <- do.call( maType, c( list(DX), maArgs ) )

  result <- cbind( DIp, DIn, DX, ADX )
  colnames(result) <- c( "DIp", "DIn", "DX", "ADX" )

  reclass(result, HLC)
}
