"SAR" <-
function(HL, accel=c(.02,.2)) {

  # Parabolic Stop-and-Reverse (SAR)
  # ----------------------------------------------
  #       HL = HL vector, matrix, or dataframe
  # accel[1] = acceleration factor
  # accel[2] = maximum acceleration factor

  # http://www.linnsoft.com/tour/techind/sar.htm
  # http://www.fmlabs.com/reference/SAR.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic_ParaSAR.htm
  # http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=87

  # WISHLIST:
  # Determine signal based on DM+/DM- for first bar
  # If sig[1]==1, then ep[1]==high; if sig[1]==-1, then ep[1]==low
  # The first SAR value should be the opposite (high/low) of ep
  # The first acceleration factor is based on the first signal

  # Since I've already lost one bar, do what TA-lib does and use that bar to
  # determine the inital signal value.  Also try to incorporate different
  # accel factors for long/short.
  # accel = c( long = c( 0.02, 0.2 ), short = long )

  HL <- as.matrix(HL)

  # Initialize all the necessary vectors
  sar <- ep <- af <- sig <- rep(NA,NROW(HL))
  sig[1] <- 1              # Signal (long/short)
  ep[1]  <- HL[1,1]      # Extreme Price
  sar[1] <- HL[1,2]-.01  # Stop and Reverse Value
  af[1]  <- accel[1]       # Acceleration Factor

  for (i in 2:NROW(sar)) {

    # Create sig (Signal) Vector
    if (sig[i-1] == 1) {
      sig[i] <- ifelse(HL[i,2]>sar[i-1], 1,-1)
    } else
    if (sig[i-1] ==-1) {
      sig[i] <- ifelse(HL[i,1]<sar[i-1],-1, 1)
    }

    # Create EP (Extreme Price) vector
    if (sig[i-1] == 1) {
      ep[i] <- ifelse(HL[i,1]>ep[i-1], HL[i,1], ep[i-1])
    } else
    if (sig[i-1] ==-1) {
      ep[i] <- ifelse(HL[i,2]<ep[i-1], HL[i,2], ep[i-1])
    }

    # Create AF (Acceleration Factor) vector
    if (sig[i] == sig[i-1]) {
      if(sig[i] == 1) {
        af[i] <- ifelse(ep[i]>ep[i-1], ifelse(af[i-1]==accel[2], af[i-1], accel[1]+af[i-1]), af[i-1])
      } else
      if(sig[i] ==-1) {
        af[i] <- ifelse(ep[i]<ep[i-1], ifelse(af[i-1]==accel[2], af[i-1], accel[1]+af[i-1]), af[i-1])
      }
    } else
      af[i] <- accel[1]

    # Create SAR (Stop-and-Reverse) vector
    if (sig[i] == sig[i-1]) {
      sar[i] <- sar[i-1] + (ep[i-1] - sar[i-1]) * af[i-1]
      if(sig[i] == 1) {
        sar[i] <- ifelse(sar[i] < min(HL[(i-1):i,2]), sar[i], min(HL[(i-1):i,2]))
      } else
      if(sig[i] ==-1) {
        sar[i] <- ifelse(sar[i] > max(HL[(i-1):i,1]), sar[i], max(HL[(i-1):i,1]))
      }
    } else
      sar[i] <- ep[i-1]
  }

  return( sar )
}
