#-------------------------------------------------------------------------#
# TTR, copyright (C) Joshua M. Ulrich, 2007                               #
# Distributed under GNU GPL version 3                                     #
#-------------------------------------------------------------------------#

"CLV" <-
function(HLC) {

  # Close Location Value

  # http://stockcharts.com/education/IndicatorAnalysis/indic_AccumDistLine.html

  HLC <- as.matrix(HLC)
  clv <- ((HLC[,3]-HLC[,2]) - (HLC[,1]-HLC[,3])) / (HLC[,1]-HLC[,2])

  # Account for H=L=C
  clv[is.nan(clv)] <- 0

  return( clv )
}
