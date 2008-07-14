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

input <- list( all=ttrc[1:250,], top=ttrc[1:250,], mid=ttrc[1:250,] )
input$top[1:10,] <- NA
input$mid[9:20,] <- NA

# Load output data
load('unitTests/output.volume.rda')

#################################################

# On Balance Volume
test.OBV <- function() {
  checkEqualsNumeric( OBV(input$all$Close, input$all$Volume), output$allOBV )
  #checkEqualsNumeric( OBV(input$top$Close, input$top$Volume), output$topOBV )
  #checkException( OBV(input$mid$Close, input$mid$Volume) )
  #checkException( OBV(input$all$Close, input$mid$Volume) )
  #checkException( OBV(input$mid$Close, input$all$Volume) )
}

# Chaikin Accumulation / Distribution
test.chaikinAD <- function() {
  checkEqualsNumeric( chaikinAD(input$all[,c('High','Low','Close')], input$all$Volume), output$allChaikinAD )
  #checkEqualsNumeric( chaikinAD(input$top[,c('High','Low','Close')], input$top$Volume), output$topChaikinAD )
  #checkException( chaikinAD(input$mid[,c('High','Low','Close')], input$mid$Volume) )
  #checkException( chaikinAD(input$all[,c('High','Low','Close')], input$mid$Volume) )
  #checkException( chaikinAD(input$mid[,c('High','Low','Close')], input$all$Volume) )
}

# Chaikin Money Flow
test.CMF <- function() {
  checkEqualsNumeric( CMF(input$all[,c('High','Low','Close')], input$all$Volume), output$allCMF )
  checkEqualsNumeric( CMF(input$top[,c('High','Low','Close')], input$top$Volume), output$topCMF )
  checkException( CMF(input$mid[,c('High','Low','Close')], input$mid$Volume) )
  checkException( CMF(input$all[,c('High','Low','Close')], input$mid$Volume) )
  checkException( CMF(input$mid[,c('High','Low','Close')], input$all$Volume) )
}

# Money Flow Index
test.MFI <- function() {
  checkEqualsNumeric( MFI(input$all[,c('High','Low','Close')], input$all$Volume), output$allMFI )
  checkEqualsNumeric( MFI(input$top[,c('High','Low','Close')], input$top$Volume), output$topMFI )
  checkException( MFI(input$mid[,c('High','Low','Close')], input$mid$Volume) )
  checkException( MFI(input$all[,c('High','Low','Close')], input$mid$Volume) )
  checkException( MFI(input$mid[,c('High','Low','Close')], input$all$Volume) )
}

# Williams' Accumulation / Distribution
test.williamsAD <- function() {
  checkEqualsNumeric( williamsAD(input$all[,c('High','Low','Close')]), output$allWilliamsAD )
  #checkEqualsNumeric( williamsAD(input$top[,c('High','Low','Close')]), output$topWilliamsAD )
  #checkException( williamsAD(input$mid[,c('High','Low','Close')]) )
}
