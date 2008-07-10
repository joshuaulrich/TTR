#
# RUnit tests TTR moving averages
#

# Create input data
data(ttrc)
rownames(ttrc) <- ttrc$Date
ttrc$Date <- NULL

input <- list( all=ttrc[1:250,], top=ttrc[1:250,], mid=ttrc[1:250,] )
input$top[1:10,] <- NA
input$mid[9:20,] <- NA

# Load output data
load('unitTests/output.MA.rda')

#################################################

# Simple Moving Average
test.SMA <- function() {
  checkEqualsNumeric( SMA(input$all$Close), output$allSMA )
  checkEqualsNumeric( SMA(input$top$Close), output$topSMA )
  checkException( SMA(input$mid$Close) )
}

# Exponential Moving Average
test.EMA <- function() {
  checkEqualsNumeric( EMA(input$all$Close), output$allEMA )
  checkEqualsNumeric( EMA(input$top$Close), output$topEMA )
  checkException( EMA(input$mid$Close) )
}

# Exponential Moving Average, Wilder ratio
test.EMA.wilder <- function() {
  checkEqualsNumeric( EMA(input$all$Close, wilder=TRUE), output$allEMAwilder )
  checkEqualsNumeric( EMA(input$top$Close, wilder=TRUE), output$topEMAwilder )
  checkException( EMA(input$mid$Close, wilder=TRUE) )
}

# Double-Exponential Moving Average
test.DEMA <- function() {
  checkEqualsNumeric( DEMA(input$all$Close), output$allDEMA )
  checkEqualsNumeric( DEMA(input$top$Close), output$topDEMA )
  checkException( DEMA(input$mid$Close) )
}

# Weighted Moving Average, 1:n
test.WMA <- function() {
  checkEqualsNumeric( WMA(input$all$Close), output$allWMA )
  checkEqualsNumeric( WMA(input$top$Close), output$topWMA )
  checkException( WMA(input$mid$Close) )
  checkException( WMA(input$all$Close, wts=1) )
}

# Weighted Moving Average, Volume
test.WMAvol <- function() {
  checkEqualsNumeric( WMA(input$all$Close, wts=input$all$Volume), output$allWMAvol )
  checkEqualsNumeric( WMA(input$top$Close, wts=input$top$Volume), output$topWMAvol )
  checkException( WMA(input$all$Close, wts=input$mid$Volume) )
}

# Exponential, Volume-Weighted Moving Average
test.EVWMA <- function() {
  checkEqualsNumeric( EVWMA(input$all$Close, input$all$Volume), output$allEVWMA )
  checkEqualsNumeric( EVWMA(input$top$Close, input$top$Volume), output$topEVWMA )
  checkException( EVWMA(input$mid$Close, input$mid$Volume) )
  checkException( EVWMA(input$all$Close) )
}

# Zero-Lag Exponential Moving Average
test.ZLEMA <- function() {
  checkEqualsNumeric( ZLEMA(input$all$Close), output$allZLEMA )
  checkEqualsNumeric( ZLEMA(input$top$Close), output$topZLEMA )
  checkException( ZLEMA(input$mid$Close) )
}
