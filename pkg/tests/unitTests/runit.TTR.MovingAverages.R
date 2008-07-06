#
# RUnit tests TTR moving averages
#

# test reclass works and throws error
# test xtsAttributes, both CLASS and USER
# test all.equal(CLASS) and !all.equal(CLASS) cases

# Create datums :)
data(ttrc)

# create raw 'xts' object
xtsX <- xts( sample_matrix, order.by=date.index )
# create 'matrix' object
xtsM <- as.xts( sample_matrix )
# create 'data.frame' object
xtsDF <- as.xts( data.frame(sample_matrix) )
# create 'zoo' object
xtsZ <- as.xts( zoo(sample_matrix, date.index) )
# create 'its' object
xtsI <- as.xts( its(sample_matrix) )
# create 'irts' object
xtsIR <- as.xts( irts(as.POSIXct(date.index), sample_matrix) )
# create 'ts' object
xts.ts <- as.xts( ts(sample_matrix, start=as.numeric(date.index)[1]) )
# create 'timeSeries' object
xts.TS <- as.xts( timeSeries(sample_matrix, charvec=date.index) )

#################################################
# everything :)
test.SMA <- function() {
  # Separate 'Date' and 'POSIXt' index
  ccD <- cbind( xtsZ[,1:2],
                xts.ts[,3:4] )
  ccP1 <- cbind( xtsM[,1],
                 xtsDF[,2],
                 xtsI[,3],
                 xtsIR[,4] )
  ccP2 <- cbind( xtsM[,1],
                 xtsDF[,2],
                 xtsIR[,3],
                 xts.TS[,4] )
  checkIdentical(ccP1, ccP2)
  checkIdentical(ccD, xtsX)
}

test.rbind_xts <- function() {
  # Separate 'Date' and 'POSIXt' index
  rrD <- rbind( xtsZ[c(s1,s2),],
                xts.ts[c(s3,s4),] )
  rrP1 <- rbind( xtsM[s1,],
                 xtsDF[s2,],
                 xtsI[s3,],
                 xtsIR[s4,] )
  rrP2 <- rbind( xtsM[s1,],
                 xtsDF[s2,],
                 xtsIR[s3,],
                 xts.TS[s4,] )
  checkIdentical(rrP1, rrP2)
  checkIdentical(rrD, xtsX)
}

