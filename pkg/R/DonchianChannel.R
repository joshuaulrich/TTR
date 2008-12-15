#-------------------------------------------------------------------------#
# TTR, copyright (C) Joshua M. Ulrich, 2007                               #
# Distributed under GNU GPL version 3                                     #
#-------------------------------------------------------------------------#

'DonchianChannel' <-
function(HL, n=10) {

  # Donchian Channel

  # http://www.linnsoft.com/tour/techind/donch.htm

  high <- runMax(HL[,1],n)
  low <- runMin(HL[,2],n)
  mid <- (high+low)/2

  result <- cbind(high,mid,low)
  
  return(result)
}
