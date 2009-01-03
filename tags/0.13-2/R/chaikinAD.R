"chaikinAD" <-
function(HLC, volume) {

  # Chaikin Accumulation / Distribution

  # http://www.fmlabs.com/reference/AccumDist.htm
  # http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=27
  # http://www.linnsoft.com/tour/techind/acc_dis.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic_AccumDistLine.html

  HLC <- as.matrix(HLC)
  ad  <- cumsum( CLV(HLC) * volume )

  return( ad )
}

