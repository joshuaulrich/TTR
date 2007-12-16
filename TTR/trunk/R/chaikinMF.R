#-------------------------------------------------------------------------#
# TTR, copyright (C) Joshua M. Ulrich, 2007                               #
# Distributed under GNU GPL version 3                                     #
#-------------------------------------------------------------------------#

"chaikinMF" <-
function(HLC, volume, n=20) {

  # Chaikin Money Flow

  # http://www.fmlabs.com/reference/ChaikinMoneyFlow.htm
  # http://www.linnsoft.com/tour/techind/cmf.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic_ChaikinMoneyFlow1.html

  HLC <- as.matrix(HLC)

  clv    <- as.double( CLV(HLC) )
  volume <- as.double( volume )

  MF <- runSum(clv*volume, n) / runSum(volume, n)

  return( MF )
}
