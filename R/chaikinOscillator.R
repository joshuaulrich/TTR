"chaikinOscillator" <-
function(HLC, volume, ma.slow = list("EMA", n=10), ma.fast = list("EMA", n=3), percent = FALSE) {

  # Chaikin Oscillator

  # http://www.fmlabs.com/reference/ChaikinOscillator.htm
  # http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=41
  # http://stockcharts.com/education/IndicatorAnalysis/indic_ChaikinOscillator.html

  HLC <- as.matrix(HLC)
  AD  <- chaikinAD(HLC, volume)

  oscillator <- oscillator( AD, ma.slow, ma.fast, ma.sig = NULL, percent )

  return( oscillator )
}
