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
load('unitTests/output.trend.rda')

#################################################

# ADX
test.ADX <- function() {
  checkEqualsNumeric( ADX(input$all[,c('High','Low','Close')]), output$allADX )
  checkEqualsNumeric( ADX(input$top[,c('High','Low','Close')]), output$topADX )
  #checkException( ADX(input$mid[,c('High','Low','Close')]) )
}

# Aroon
test.aroon <- function() {
  checkEqualsNumeric( aroon(input$all[,c('High','Low')]), output$allAroon )
  checkEqualsNumeric( aroon(input$top[,c('High','Low')]), output$topAroon )
  #checkException( aroon(input$mid[,c('High','Low')]) )
}

# Average True Range
test.ATR <- function() {
  checkEqualsNumeric( ATR(input$all[,c('High','Low','Close')]), output$allATR )
  checkEqualsNumeric( ATR(input$top[,c('High','Low','Close')]), output$topATR )
  #checkException( ATR(input$mid[,c('High','Low','Close')]) )
}

# Commodity Channel Index
test.CCI <- function() {
  checkEqualsNumeric( CCI(input$all[,c('High','Low','Close')]), output$allCCI )
  checkEqualsNumeric( CCI(input$top[,c('High','Low','Close')]), output$topCCI )
  #checkException( CCI(input$mid[,c('High','Low','Close')]) )
}

# Trend Detection Index
test.TDI <- function() {
  checkEqualsNumeric( TDI(input$all$Close), output$allTDI )
  #checkEqualsNumeric( TDI(input$top$Close), output$topTDI )
  #checkException( TDI(input$mid$Close) )
}

# Vertical Horizontal Filter
test.VHF <- function() {
  checkEqualsNumeric( VHF(input$all$Close), output$allVHF )
  #checkEqualsNumeric( VHF(input$top$Close), output$topVHF )
  #checkException( VHF(input$mid$Close) )
}
