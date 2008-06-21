#-------------------------------------------------------------------------#
# TTR, copyright (C) Joshua M. Ulrich, 2007                               #
# Distributed under GNU GPL version 3                                     #
#-------------------------------------------------------------------------#

"chaikinVolatility" <-
function(HL, n=10, maType, ...) {

  # Chaikin Volatility

  # http://www.fmlabs.com/reference/ChaikinVolatility.htm
  # http://www.equis.com/Customer/Resources/TAAZ/Default.aspx?c=3&p=120

  HL   <- as.matrix(HL)

  maArgs <- list(n=n, ...)
  # Default MA
  if(missing(maType)) {
    maType <- 'EMA'
  }

  mavg <- do.call( maType, c( list(HL[,1]-HL[,2]), maArgs ) )

  volatility <- ROC( mavg, n, type="discrete" )

  return( volatility )
}
