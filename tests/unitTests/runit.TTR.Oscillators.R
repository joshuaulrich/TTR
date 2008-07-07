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
load('output.Oscillators.rda')

#################################################

# MACD
test.MACD <- function() {
  checkEqualsNumeric( MACD(input$all$Close), output$allMACD )
  checkEqualsNumeric( MACD(input$top$Close), output$topMACD )
  checkException( MACD(input$mid$Close) )
}

# Stochastics
test.stoch <- function() {
  checkEqualsNumeric( stoch(input$all[,c('High','Low','Close')]), output$allStoch )
  checkEqualsNumeric( stoch(input$top[,c('High','Low','Close')]), output$topStoch )
  checkException( stoch(input$mid[,c('High','Low','Close')]) )
}

# Stochastic Momentum Index
test.SMI <- function() {
  checkEqualsNumeric( SMI(input$all[,c('High','Low','Close')]), output$allSMI )
  checkEqualsNumeric( SMI(input$top[,c('High','Low','Close')]), output$topSMI )
  checkException( SMI(input$mid[,c('High','Low','Close')]) )
}

# Relative Strength Index
test.RSI <- function() {
  checkEqualsNumeric( RSI(input$all$Close), output$allRSI )
  checkEqualsNumeric( RSI(input$top$Close), output$topRSI )
  checkException( RSI(input$mid$Close) )
}

# Chande Momentum Oscillator
test.CMO <- function() {
  checkEqualsNumeric( CMO(input$all$Close), output$allCMO )
  checkEqualsNumeric( CMO(input$top$Close), output$topCMO )
  checkException( CMO(input$mid$Close) )
}

# De-trended Price Oscillator
test.DPO <- function() {
  checkEqualsNumeric( DPO(input$all$Close), output$allDPO )
  checkEqualsNumeric( DPO(input$top$Close), output$topDPO )
  checkException( DPO(input$mid$Close) )
}

# TRIX
test.TRIX <- function() {
  checkEqualsNumeric( TRIX(input$all$Close), output$allTRIX )
  checkEqualsNumeric( TRIX(input$top$Close), output$topTRIX )
  checkException( TRIX(input$mid$Close) )
}

# Willams' Percent R
test.WPR <- function() {
  checkEqualsNumeric( WPR(input$all[,c('High','Low','Close')]), output$allWPR )
  checkEqualsNumeric( WPR(input$top[,c('High','Low','Close')]), output$topWPR )
  checkException( WPR(input$mid$Close) )
}
