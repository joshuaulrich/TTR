#-------------------------------------------------------------------------#
# TTR, copyright (C) Joshua M. Ulrich, 2007                               #
# Distributed under GNU GPL version 3                                     #
#-------------------------------------------------------------------------#

"CMF" <-
function(HLC, volume, n=20) {

  # Chaikin Money Flow

  # http://www.fmlabs.com/reference/ChaikinMoneyFlow.htm
  # http://www.linnsoft.com/tour/techind/cmf.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic_ChaikinMoneyFlow1.html

  HLC <- try.xts(HLC, error=FALSE)
  volume <- try.xts(volume, error=FALSE)

  if(!(is.xts(HLC) && is.xts(volume))) {
    clv <- CLV(as.matrix(HLC))
    volume <- as.matrix(volume)
  }

  cmf <- runSum(clv*volume, n) / runSum(volume, n)

  reclass(cmf, HLC)
}
