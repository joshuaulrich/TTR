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
load('unitTests/output.misc.rda')

#################################################

# Rate-of-Change
test.ROC <- function() {
  checkEqualsNumeric( ROC(input$all$Close, type='continuous'), output$allROCc )
  #checkEqualsNumeric( ROC(input$top$Close, type='continuous'), output$topROCc )
  checkEqualsNumeric( ROC(input$all$Close, type='discrete'), output$allROCd )
  #checkEqualsNumeric( ROC(input$top$Close, type='discrete'), output$topROCd )
  #checkException( ROC(input$mid$Close) )
}

# Momentum
test.momentum <- function() {
  checkEqualsNumeric( momentum(input$all$Close), output$allMom )
  #checkEqualsNumeric( momentum(input$top$Close), output$topMom )
  #checkException( momentum(input$mid$Close) )
}

# Close Location Value
test.CLV <- function() {
  checkEqualsNumeric( CLV(input$all[,c('High','Low','Close')]), output$allCLV )
  checkEqualsNumeric( CLV(input$top[,c('High','Low','Close')]), output$topCLV )
}

# Arms' Ease of Movement
test.EMV <- function() {
  checkEqualsNumeric( EMV(input$all[,c('High','Low')], input$all$Volume), output$allEMV )
  #checkEqualsNumeric( EMV(input$top[,c('High','Low')], input$top$Volume), output$topEMV )
  #checkException( EMV(input$mid[,c('High','Low')], input$mid$Volume) )
  #checkException( EMV(input$all[,c('High','Low')], input$mid$Volume) )
  #checkException( EMV(input$mid[,c('High','Low')], input$all$Volume) )
}

# Know Sure Thing
test.KST <- function() {
  checkEqualsNumeric( KST(input$all$Close), output$allKST )
  #checkEqualsNumeric( KST(input$top$Close), output$topKST )
  #checkException( KST(input$mid$Close) )
}
