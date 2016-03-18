#
# RUnit tests TTR moving averages
#

# test reclass works and throws error
# test xtsAttributes, both CLASS and USER
# test all.equal(CLASS) and !all.equal(CLASS) cases

# Create input data
data(ttrc)
rownames(ttrc) <- ttrc$Date
ttrc$Date <- NULL

#input <- list( all=ttrc[1:250,], top=ttrc[1:250,], mid=ttrc[1:250,] )
#input$top[1:10,] <- NA
#input$mid[9:20,] <- NA

#iAll <- as.matrix(ttrc[1:250,])
iAll <- ttrc[1:250,]
iTop <- iAll; iTop[1:10,] <- NA
iMid <- iAll; iMid[9:20,] <- NA

hl  <- c('High','Low')
hlc <- c('High','Low','Close')
cl  <- 'Close'

# Load output data
load(system.file("unitTests/output.volume.rda", package="TTR"))

#################################################

# On Balance Volume
test.OBV <- function() {
  checkEqualsNumeric( OBV(iAll$Close, iAll$Volume), output$allOBV )
  #checkEqualsNumeric( OBV(iTop[,cl], iTop[,'Volume']), output$topOBV )
  #checkException( OBV(iMid[,cl], iMid[,'Volume']) )
  #checkException( OBV(iAll[,cl], iMid[,'Volume']) )
  #checkException( OBV(iMid[,cl], iAll[,'Volume']) )
}

# Chaikin Accumulation / Distribution
test.chaikinAD <- function() {
  checkEqualsNumeric( chaikinAD(iAll[,hlc], iAll[,'Volume']), output$allChaikinAD )
  #checkEqualsNumeric( chaikinAD(iTop[,hlc], iTop[,'Volume']), output$topChaikinAD )
  #checkException( chaikinAD(iMid[,hlc], iMid[,'Volume']) )
  #checkException( chaikinAD(iAll[,hlc], iMid[,'Volume']) )
  #checkException( chaikinAD(iMid[,hlc], iAll[,'Volume']) )
}

# Chaikin Money Flow
test.CMF <- function() {
  ia <- iAll[,hlc];  rownames(ia) <- NULL
  it <- iTop[,hlc];  rownames(it) <- NULL
  checkEqualsNumeric( CMF(ia, iAll[,'Volume']), output$allCMF )
  checkEqualsNumeric( CMF(it, iTop[,'Volume']), output$topCMF )
  checkException( CMF(iMid[,hlc], iMid[,'Volume']) )
  checkException( CMF(iAll[,hlc], iMid[,'Volume']) )
  checkException( CMF(iMid[,hlc], iAll[,'Volume']) )
}

# Money Flow Index
test.MFI <- function() {
  ia <- iAll[,hlc];  rownames(ia) <- NULL
  it <- iTop[,hlc];  rownames(it) <- NULL
  checkEqualsNumeric( MFI(ia, iAll[,'Volume']), output$allMFI )
  checkEqualsNumeric( MFI(it, iTop[,'Volume']), output$topMFI )
  checkException( MFI(iMid[,hlc], iMid[,'Volume']) )
  checkException( MFI(iAll[,hlc], iMid[,'Volume']) )
  checkException( MFI(iMid[,hlc], iAll[,'Volume']) )
}

# Williams' Accumulation / Distribution
test.williamsAD <- function() {
  # non-xts
  ia <- iAll[,hlc]
  it <- iTop[,hlc]
  im <- iMid[,hlc]
  rownames(ia) <- rownames(it) <- rownames(im) <- NULL
  checkEqualsNumeric( williamsAD(ia), output$allWilliamsAD )
  #checkEqualsNumeric( williamsAD(iTop[,hlc]), output$topWilliamsAD )
  #checkException( williamsAD(iMid[,hlc]) )
}
