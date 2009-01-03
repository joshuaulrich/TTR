"chaikinVolatility" <-
function(HL, ma=list("EMA", n=10)) {

  # Chaikin Volatility

  # http://www.fmlabs.com/reference/ChaikinVolatility.htm
  # http://www.equis.com/Customer/Resources/TAAZ/Default.aspx?c=3&p=120

  HL   <- as.matrix(HL)
  mavg <- do.call( ma[[1]], c( list(HL[,1]-HL[,2]), ma[-1] ) )

  volatility <- ROC( mavg, ma$n )

  return( volatility )
}

