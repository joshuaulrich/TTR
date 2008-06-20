#-------------------------------------------------------------------------#
# TTR, copyright (C) Joshua M. Ulrich, 2007                               #
# Distributed under GNU GPL version 3                                     #
#-------------------------------------------------------------------------#

'DonchianChannel' <-
function(HL, n=10) {

  # Donchian Channel

  # Thanks to Jeff Ryan for the code
  # (so blame him if it doesn't work)
  high <- runMax(HL[,1],n)
  low <- runMin(HL[,2],n)
  
  return(cbind(high,low))
}
