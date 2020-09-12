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

# ALMA
test.ALMA.output.length.eq.input.length <- function() {
  v <- 1:10
  x <- xts::.xts(v, seq_along(v))
  av <- ALMA(v)
  ax <- ALMA(x)
  checkEquals(NROW(av), NROW(ax))
}

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
  checkException( EMA(input$all$Close, n = -1) )
  checkException( EMA(input$all$Close, n = NROW(input$all) + 1) )
}

test.EMA.n.ratio <- function() {
  out <- 0:9 * 1.0
  is.na(out) <- 1:2
  checkEqualsNumeric(EMA(1:10, ratio = 0.5), out)
  checkEqualsNumeric(EMA(1:10, n = 3), out)
  checkEqualsNumeric(EMA(1:10, n = 3, ratio = 0.5), out)
}

test.EMA.ratio.eq.0 <- function() {
  checkException(EMA(1:10, ratio = 0.0))
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

# Hull Moving Average
test.HMA <- function() {
  hma <- HMA(1:10, 2)
  checkEqualsNumeric(hma, c(NA, 2:10 + 1/3))
}
test.HMA.odd.n <- function() {
  hma <- HMA(1:10, 3)
  checkEqualsNumeric(hma, c(rep(NA, 2), 3:10 + 2/3))
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
  checkException( WMA(input$all$Close, n = -1) )
  checkException( WMA(input$all$Close, n = NROW(input$all) + 1) )
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

test.WMA_returns_xts <- function() {
  x <- xts::.xts(x = c(NA, 1:3), 1:4)
  wma <- WMA(x, 2)
  checkTrue(inherits(wma, "xts"))
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
  checkException( EVWMA(input$all$Close, n = -1) )
  checkException( EVWMA(input$all$Close, n = NROW(input$all) + 1) )
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

test.ZLEMA.n.ratio <- function() {
  out <- c(rep(NA, 6), 4.0, 6.0, 7.75, 9.3125)
  checkEqualsNumeric(ZLEMA(1:10, ratio = 0.25), out)
  checkEqualsNumeric(ZLEMA(1:10, n = 7), out)
  checkEqualsNumeric(ZLEMA(1:10, n = 7, ratio = 0.25), out)
}

test.ZLEMA.ratio.eq.0 <- function() {
  checkException(ZLEMA(1:10, ratio = 0.0))
}

test.EMA.non.na.eq.n.does.not.error <- function() {
  x <- c(NA, rnorm(10))
  e <- EMA(x, 10)
  z <- ZLEMA(x, 10)
  # error will prevent reaching here, failing test
  return(TRUE)
}

