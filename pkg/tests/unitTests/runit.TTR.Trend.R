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

iAll <- as.matrix(ttrc[1:250,])
iTop <- iAll; iTop[1:10,] <- NA
iMid <- iAll; iMid[9:20,] <- NA

hl  <- c('High','Low')
hlc <- c('High','Low','Close')
cl  <- 'Close'

# Load output data
load('unitTests/output.trend.rda')

#################################################

# ADX
test.ADX <- function() {
  checkEqualsNumeric( ADX(iAll[,hlc]), output$allADX )
  checkEqualsNumeric( ADX(iTop[,hlc]), output$topADX )
  #checkException( ADX(iMid[,hlc]) )
}

# Aroon
test.aroon.orig <- function() {
  # non-xts
  ia <- iAll[,hl]
  it <- iTop[,hl]
  im <- iMid[,hl]
  rownames(ia) <- rownames(it) <- rownames(im) <- NULL
  checkEqualsNumeric( aroon(ia), output$allAroon )
  checkEqualsNumeric( aroon(it), output$topAroon )
  #checkException( aroon(im) )
}

test.aroon.xts <- function() {
  # xts
  checkEqualsNumeric( aroon(iAll[,hl]), output$allAroon )
  checkEqualsNumeric( aroon(iTop[,hl]), output$topAroon )
  #checkException( aroon(iMid[,hl]) )
}

# Average True Range
test.ATR.orig <- function() {
  # non-xts
  ia <- iAll[,hlc]
  it <- iTop[,hlc]
  im <- iMid[,hlc]
  rownames(ia) <- rownames(it) <- rownames(im) <- NULL
  aATR <- ATR(ia);  rownames(aATR) <- rownames(iAll)
  tATR <- ATR(it);  rownames(tATR) <- rownames(iTop)
  checkEqualsNumeric( aATR, output$allATR )
  checkEqualsNumeric( tATR, output$topATR )
  #checkException( ATR(im) )
}

test.ATR.xts <- function() {
  # xts
  checkEqualsNumeric( ATR(iAll[,hlc]), output$allATR )
  checkEqualsNumeric( ATR(iTop[,hlc]), output$topATR )
  #checkException( ATR(iMid[,hlc]) )
}

# Commodity Channel Index
test.CCI <- function() {
  checkEqualsNumeric( CCI(iAll[,c('High','Low','Close')]), output$allCCI )
  checkEqualsNumeric( CCI(iTop[,c('High','Low','Close')]), output$topCCI )
  #checkException( CCI(input$mid[,c('High','Low','Close')]) )
}

# Trend Detection Index
test.TDI <- function() {
  checkEqualsNumeric( TDI(iAll[,cl]), output$allTDI )
  #checkEqualsNumeric( TDI(iTop[,cl]), output$topTDI )
  #checkException( TDI(iMid[,cl]) )
}

# Vertical Horizontal Filter
test.VHF <- function() {
  checkEqualsNumeric( VHF(iAll[,cl]), output$allVHF )
  #checkEqualsNumeric( VHF(iTop[,cl]), output$topVHF )
  #checkException( VHF(iMid[,cl] )
}
