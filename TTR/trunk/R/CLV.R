"CLV" <-
function(HLC) {

  # Close Location Value

  # http://stockcharts.com/education/IndicatorAnalysis/indic_AccumDistLine.html

  HLC <- as.matrix(HLC)
  clv <- ((HLC[,3]-HLC[,2]) - (HLC[,1]-HLC[,3])) / (HLC[,1]-HLC[,2])

  # Account for H=L=C

  clv <- replace(clv, match(NaN,clv), 0)

  return( clv )
}

