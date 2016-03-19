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
load(system.file("unitTests/output.MA.rda", package="TTR"))

#################################################

# Simple Moving Average
test.SMA <- function() {
  checkEqualsNumeric( SMA(input$all$Close), output$allSMA )
  checkEquals( attributes(SMA(input$all$Close)), attributes(output$allSMA) )
  checkEqualsNumeric( SMA(input$top$Close), output$topSMA )
  checkEquals( attributes(SMA(input$top$Close)), attributes(output$topSMA) )
  checkException( SMA(input$mid$Close) )
  checkException( SMA(input$all[,1:2]) )
}

# Exponential Moving Average
test.EMA <- function() {
  checkEqualsNumeric( EMA(input$all$Close), output$allEMA )
  checkEquals( attributes(EMA(input$all$Close)), attributes(output$allEMA) )
  checkEqualsNumeric( EMA(input$top$Close), output$topEMA )
  checkEquals( attributes(EMA(input$top$Close)), attributes(output$topEMA) )
  checkException( EMA(input$mid$Close) )
  checkException( EMA(input$all[,1:2]) )
}

# Exponential Moving Average, Wilder ratio
test.EMA.wilder <- function() {
  checkEqualsNumeric( EMA(input$all$Close, wilder=TRUE), output$allEMAwilder )
  checkEquals( attributes(EMA(input$all$Close, wilder=TRUE)), attributes(output$allEMAwilder) )
  checkEqualsNumeric( EMA(input$top$Close, wilder=TRUE), output$topEMAwilder )
  checkEquals( attributes(EMA(input$top$Close, wilder=TRUE)), attributes(output$topEMAwilder) )
  checkException( EMA(input$mid$Close, wilder=TRUE) )
}

# Double-Exponential Moving Average
test.DEMA <- function() {
  checkEqualsNumeric( DEMA(input$all$Close), output$allDEMA )
  checkEquals( attributes(DEMA(input$all$Close)), attributes(output$allDEMA) )
  checkEqualsNumeric( DEMA(input$top$Close), output$topDEMA )
  checkEquals( attributes(DEMA(input$top$Close)), attributes(output$topDEMA) )
  checkException( DEMA(input$mid$Close) )
  checkException( DEMA(input$all[,1:2]) )
}

# Weighted Moving Average, 1:n
test.WMA <- function() {
  checkEqualsNumeric( WMA(input$all$Close), output$allWMA )
  checkEquals( attributes(WMA(input$all$Close)), attributes(output$allWMA) )
  checkEqualsNumeric( WMA(input$top$Close), output$topWMA )
  checkEquals( attributes(WMA(input$top$Close)), attributes(output$topWMA) )
  checkException( WMA(input$mid$Close) )
  checkException( WMA(input$all$Close, wts=1) )
  checkException( WMA(input$all[,1:2]) )
}

# Weighted Moving Average, Volume
test.WMAvol <- function() {
  checkEqualsNumeric( WMA(input$all$Close, wts=input$all$Volume), output$allWMAvol )
  checkEquals( attributes(WMA(input$all$Close, wts=input$all$Volume)), attributes(output$allWMAvol) )
  checkEqualsNumeric( WMA(input$top$Close, wts=input$top$Volume), output$topWMAvol )
  checkEquals( attributes(WMA(input$top$Close, wts=input$top$Volume)), attributes(output$topWMAvol) )
  checkException( WMA(input$all$Close, wts=input$mid$Volume) )
  checkException( WMA(input$all[,1:2], wts=input$all$Volume) )
  checkException( WMA(input$all$Close, wts=input$all[,1:2]) )
}

# Exponential, Volume-Weighted Moving Average
test.EVWMA <- function() {
  checkEqualsNumeric( EVWMA(input$all$Close, input$all$Volume), output$allEVWMA )
  checkEquals( attributes(EVWMA(input$all$Close, input$all$Volume)), attributes(output$allEVWMA) )
  checkEqualsNumeric( EVWMA(input$top$Close, input$top$Volume), output$topEVWMA )
  checkEquals( attributes(EVWMA(input$top$Close, input$top$Volume)), attributes(output$topEVWMA) )
  checkException( EVWMA(input$mid$Close, input$mid$Volume) )
  checkException( EVWMA(input$all$Close) )
  checkException( EVWMA(input$all[,1:2], input$all$Volume) )
  checkException( EVWMA(input$all$Close, input$all[,1:2]) )
}

# Zero-Lag Exponential Moving Average
test.ZLEMA <- function() {
  checkEqualsNumeric( ZLEMA(input$all$Close), output$allZLEMA )
  checkEquals( attributes(ZLEMA(input$all$Close)), attributes(output$allZLEMA) )
  checkEqualsNumeric( ZLEMA(input$top$Close), output$topZLEMA )
  checkEquals( attributes(ZLEMA(input$top$Close)), attributes(output$topZLEMA) )
  checkException( ZLEMA(input$mid$Close) )
  checkException( ZLEMA(input$all[,1:2]) )
}
