#
#   TTR: Technical Trading Rules
#
#   Copyright (C) 2007-2010  Joshua M. Ulrich
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

"BBands" <-
function(HLC, n=20, maType, sd=2, ...) {

  # Bollinger Bands

  # http://www.fmlabs.com/reference/Bollinger.htm
  # http://www.fmlabs.com/reference/BollingerWidth.htm
  # http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=36
  # http://www.linnsoft.com/tour/techind/bb.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic_Bbands.html
  # http://stockcharts.com/education/IndicatorAnalysis/indic_BBWidth.htm

  HLC <- try.xts(HLC, error=as.matrix)

  if(NCOL(HLC)==3) {
    if(is.xts(HLC)) {
      xa <- xcoredata(HLC)
      HLC <- xts(rowMeans(HLC),index(HLC))
      xcoredata(HLC) <- xa
    } else {
      HLC <- rowMeans(HLC)
    }
  } else
  if(NCOL(HLC)!=1) {
    stop("Price series must be either High-Low-Close, or Close/univariate.")
  }

  maArgs <- list(n=n, ...)
  # Default MA
  if(missing(maType)) {
    maType <- 'SMA'
  }

  mavg  <- do.call( maType, c( list(HLC), maArgs ) )

  # Calculate standard deviation by hand to incorporate various MAs
  sdev   <- runSD(HLC, n)

  up     <- mavg + sd * sdev
  dn     <- mavg - sd * sdev
  pctB  <- (HLC - dn) / (up - dn)

  res <- cbind(dn, mavg, up, pctB)
  colnames(res) <- c("dn", "mavg", "up", "pctB")

  reclass(res, HLC)
}
