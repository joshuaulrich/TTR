# Create input data
data(ttrc)
rownames(ttrc) <- ttrc$Date
ttrc$Date <- NULL

input <- list( all=ttrc[1:250,], top=ttrc[1:250,], mid=ttrc[1:250,] )
input$top[1:10,] <- NA
input$mid[9:20,] <- NA

# Load output data
load(system.file("tinytest/output/moving-averages.rda", package="TTR"))

#################################################

# ALMA
v <- 1:10
x <- xts::.xts(v, seq_along(v))
av <- ALMA(v)
ax <- ALMA(x)
expect_equal(NROW(av), NROW(ax))

# Simple Moving Average
expect_equal( SMA(input$all$Close), output$allSMA )
expect_equal( attributes(SMA(input$all$Close)), attributes(output$allSMA) )
expect_equal( SMA(input$top$Close), output$topSMA )
expect_equal( attributes(SMA(input$top$Close)), attributes(output$topSMA) )
expect_error( SMA(input$mid$Close) )
expect_error( SMA(input$all[,1:2]) )

# Exponential Moving Average
expect_equal( EMA(input$all$Close), output$allEMA )
expect_equal( attributes(EMA(input$all$Close)), attributes(output$allEMA) )
expect_equal( EMA(input$top$Close), output$topEMA )
expect_equal( attributes(EMA(input$top$Close)), attributes(output$topEMA) )
expect_error( EMA(input$mid$Close) )
expect_error( EMA(input$all[,1:2]) )
expect_error( EMA(input$all$Close, n = -1) )
expect_error( EMA(input$all$Close, n = NROW(input$all) + 1) )

out <- 0:9 * 1.0
is.na(out) <- 1:2
expect_equal(EMA(1:10, ratio = 0.5), out)
expect_equal(EMA(1:10, n = 3), out)
expect_warning(ema_n_ratio <- EMA(1:10, n = 3, ratio = 0.5), info = "'n' and 'ratio' specified")
expect_equal(ema_n_ratio, out)
expect_error(EMA(1:10, ratio = 0.0), info = "ratio = 0")

# Exponential Moving Average, Wilder ratio
expect_equal( EMA(input$all$Close, wilder=TRUE), output$allEMAwilder )
expect_equal( attributes(EMA(input$all$Close, wilder=TRUE)), attributes(output$allEMAwilder) )
expect_equal( EMA(input$top$Close, wilder=TRUE), output$topEMAwilder )
expect_equal( attributes(EMA(input$top$Close, wilder=TRUE)), attributes(output$topEMAwilder) )
expect_error( EMA(input$mid$Close, wilder=TRUE) )

# Double-Exponential Moving Average
expect_equal( DEMA(input$all$Close), output$allDEMA )
expect_equal( attributes(DEMA(input$all$Close)), attributes(output$allDEMA) )
expect_equal( DEMA(input$top$Close), output$topDEMA )
expect_equal( attributes(DEMA(input$top$Close)), attributes(output$topDEMA) )
expect_error( DEMA(input$mid$Close) )
expect_error( DEMA(input$all[,1:2]) )

# Hull Moving Average
hma <- HMA(1:10, 2)
expect_equal(hma, c(NA, 2:10 + 1/3))

hma <- HMA(1:10, 3)
expect_equal(hma, c(rep(NA, 2), 3:10 + 2/3))

# Weighted Moving Average, 1:n
expect_equal( WMA(input$all$Close), output$allWMA )
expect_equal( attributes(WMA(input$all$Close)), attributes(output$allWMA) )
expect_equal( WMA(input$top$Close), output$topWMA )
expect_equal( attributes(WMA(input$top$Close)), attributes(output$topWMA) )
expect_error( WMA(input$mid$Close) )
expect_error( WMA(input$all$Close, wts=1) )
expect_error( WMA(input$all[,1:2]) )
expect_error( WMA(input$all$Close, n = -1) )
expect_error( WMA(input$all$Close, n = NROW(input$all) + 1) )

# Weighted Moving Average, Volume
expect_equal( WMA(input$all$Close, wts=input$all$Volume), output$allWMAvol )
expect_equal( attributes(WMA(input$all$Close, wts=input$all$Volume)), attributes(output$allWMAvol) )
expect_equal( WMA(input$top$Close, wts=input$top$Volume), output$topWMAvol )
expect_equal( attributes(WMA(input$top$Close, wts=input$top$Volume)), attributes(output$topWMAvol) )
expect_error( WMA(input$all$Close, wts=input$mid$Volume) )
expect_error( WMA(input$all[,1:2], wts=input$all$Volume) )
expect_error( WMA(input$all$Close, wts=input$all[,1:2]) )

x <- xts::.xts(x = c(NA, 1:3), 1:4)
wma <- WMA(x, 2)
expect_true(inherits(wma, "xts"))

# Exponential, Volume-Weighted Moving Average
expect_equal( EVWMA(input$all$Close, input$all$Volume), output$allEVWMA )
expect_equal( attributes(EVWMA(input$all$Close, input$all$Volume)), attributes(output$allEVWMA) )
expect_equal( EVWMA(input$top$Close, input$top$Volume), output$topEVWMA )
expect_equal( attributes(EVWMA(input$top$Close, input$top$Volume)), attributes(output$topEVWMA) )
expect_error( EVWMA(input$mid$Close, input$mid$Volume) )
expect_error( EVWMA(input$all$Close) )
expect_error( EVWMA(input$all[,1:2], input$all$Volume) )
expect_error( EVWMA(input$all$Close, input$all[,1:2]) )
expect_error( EVWMA(input$all$Close, n = -1) )
expect_error( EVWMA(input$all$Close, n = NROW(input$all) + 1) )

# Zero-Lag Exponential Moving Average
expect_equal( ZLEMA(input$all$Close), output$allZLEMA )
expect_equal( attributes(ZLEMA(input$all$Close)), attributes(output$allZLEMA) )
expect_equal( ZLEMA(input$top$Close), output$topZLEMA )
expect_equal( attributes(ZLEMA(input$top$Close)), attributes(output$topZLEMA) )
expect_error( ZLEMA(input$mid$Close) )
expect_error( ZLEMA(input$all[,1:2]) )

out <- c(rep(NA, 6), 4.0, 6.0, 7.75, 9.3125)
expect_equal(ZLEMA(1:10, ratio = 0.25), out)
expect_equal(ZLEMA(1:10, n = 7), out)
expect_warning(zlema_n_ratio <- ZLEMA(1:10, n = 7, ratio = 0.25), info = "'n' and 'ratio' specified")
expect_equal(zlema_n_ratio, out)
expect_error(ZLEMA(1:10, ratio = 0.0), info = "ratio = 0")

x <- c(NA, rnorm(10))
expect_silent(EMA(x, 10), info = "non-na equals 'n' does not error")
expect_silent(ZLEMA(x, 10), info = "non-na equals 'n' does not error")

# column names
x <- xts::as.xts(ttrc)
p <- x[, "Close"]
v <- x[, "Volume"]
expect_equal("SMA",   colnames(SMA(p)))
expect_equal("EMA",   colnames(EMA(p)))
expect_equal("DEMA",  colnames(DEMA(p)))
expect_equal("WMA",   colnames(WMA(p)))
expect_equal("EVWMA", colnames(EVWMA(p, v)))
expect_equal("ZLEMA", colnames(ZLEMA(p)))
expect_equal("VWAP",  colnames(VWAP(p, v)))
expect_equal("HMA",   colnames(HMA(p)))
expect_equal("ALMA",  colnames(ALMA(p)))
