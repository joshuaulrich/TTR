#-------------------------------------------------------------------------#
# TTR, copyright (C) Joshua M. Ulrich, 2007                               #
# Distributed under GNU GPL version 3                                     #
#-------------------------------------------------------------------------#

"chaikinAD" <-
function(HLC, volume) {

  # Chaikin Accumulation / Distribution

  # http://www.fmlabs.com/reference/AccumDist.htm
  # http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=27
  # http://www.linnsoft.com/tour/techind/acc_dis.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic_AccumDistLine.html

  HLC <- try.xts(HLC, error=FALSE)
  volume <- try.xts(volume, error=FALSE)

  if(!(is.xts(HLC) && is.xts(volume))) {
    HLC <- as.matrix(HLC)
    volume <- as.matrix(volume)
  }

  ad  <- CLV(HLC) * volume

  ad.na <- naCheck(ad)
  ad <- cumsum( ad[ad.na$nonNA] )
  ad <- c( rep( NA, ad.na$NAs ), ad )

  reclass(ad, HLC)
}
