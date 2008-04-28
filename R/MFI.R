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

  if(NCOL(HLC)==1) {
    HLC <- as.vector(HLC)
  } else

  if(NCOL(HLC)==3) {
    # Typical Price
    HLC <- rowMeans(HLC)
  } else

  stop("Price series must be either High-Low-Close, or Close/univariate.")
  
  # Calculate Money Flow
  mf <- HLC * as.vector(volume)
  # Calculate positive and negative Money Flow
  pmf <- ifelse( HLC > c(HLC[1],HLC[-NROW(HLC)]), mf, 0 )
  nmf <- ifelse( HLC < c(HLC[1],HLC[-NROW(HLC)]), mf, 0 )

  # Calculate Money Ratio and Money Flow Index
  mr <- runSum( pmf, n ) / runSum( nmf, n )
  mfi <- 100 - ( 100 / ( 1 + mr ) )

  return( mfi )
}
