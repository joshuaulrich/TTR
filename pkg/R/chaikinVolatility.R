#-------------------------------------------------------------------------#
# TTR, copyright (C) Joshua M. Ulrich, 2007                               #
# Distributed under GNU GPL version 3                                     #
#-------------------------------------------------------------------------#

"chaikinVolatility" <-
function(HL, n=10, maType="EMA", ...) {

  # Chaikin Volatility

  # http://www.fmlabs.com/reference/ChaikinVolatility.htm
  # http://www.equis.com/Customer/Resources/TAAZ/Default.aspx?c=3&p=120

  HL   <- as.matrix(HL)

  maArgs <- list(n=n, ...)
  mavg <- do.call( maType, c( list(HL[,1]-HL[,2]), maArgs ) )

  volatility <- ROC( mavg, n, type="discrete" )

  return( volatility )
}
