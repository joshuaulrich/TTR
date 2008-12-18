#-------------------------------------------------------------------------#
# TTR, copyright (C) Joshua M. Ulrich, 2007                               #
# Distributed under GNU GPL version 3                                     #
#-------------------------------------------------------------------------#

"MFI" <-
function(HLC, volume, n=14) {

  # Money Flow Index

  # http://www.fmlabs.com/reference/default.htm?url=MoneyFlowIndex.htm
  # http://www.linnsoft.com/tour/techind/mfi.htm
  # http://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:money_flow_index_mfi

  HLC <- try.xts(HLC, error=FALSE)
  volume <- try.xts(volume, error=FALSE)

  if(!(is.xts(HLC) && is.xts(volume))) {
    HLC <- as.matrix(HLC)
    volume <- as.matrix(volume)
  }

  if(NCOL(HLC)==3) {
    if(is.xts(HLC)) {
      HLC <- xts(rowMeans(HLC),index(HLC))
    } else {
      HLC <- rowMeans(HLC)
    }
  } else
  if(NCOL(HLC)!=1) {
    stop("Price series must be either High-Low-Close, or Close/univariate.")
  }

  if(is.xts(HLC)) {
    priceLag <- lag(HLC)
  } else {
    priceLag <- c( NA, HLC[-NROW(HLC)] )
  }

  # Calculate Money Flow
  mf <- HLC * volume
  # Calculate positive and negative Money Flow
  pmf <- ifelse( HLC > priceLag, mf, 0 )
  nmf <- ifelse( HLC < priceLag, mf, 0 )

  # Calculate Money Ratio and Money Flow Index
  mr <- runSum( pmf, n ) / runSum( nmf, n )
  mfi <- 100 - ( 100 / ( 1 + mr ) )
  if(is.xts(mfi)) colnames(mfi) <- 'mfi'

  reclass( mfi, HLC )
}
