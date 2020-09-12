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
load(system.file("unitTests/output.trend.rda", package="TTR"))

#################################################

# ADX
test.ADX <- function() {
  checkEqualsNumeric( ADX(iAll[,hlc]), output$allADX )
  checkEquals( attributes(ADX(iAll[,hlc])), attributes(output$allADX) )
  checkEqualsNumeric( ADX(iTop[,hlc]), output$topADX )
  checkEquals( attributes(ADX(iTop[,hlc])), attributes(output$topADX) )
  #checkException( ADX(iMid[,hlc]) )
}

test.ADX.does.not.overwrite.maArgs <- function() {
  wilder.and.matype <- ADX(iAll[,hlc], maType = "EMA", wilder = FALSE)
  wilder.only <- ADX(iAll[,hlc], wilder = FALSE)
  checkEqualsNumeric( wilder.and.matype, wilder.only )
}

# Aroon
test.aroon.orig <- function() {
  # non-xts
  ia <- iAll[,hl]
  it <- iTop[,hl]
  im <- iMid[,hl]
  rownames(ia) <- rownames(it) <- rownames(im) <- NULL
  oa <- aroon(ia);  rownames(oa) <- rownames(iAll)
  ot <- aroon(it);  rownames(ot) <- rownames(iTop)
  checkEqualsNumeric( oa, output$allAroon )
  checkEquals( attributes(oa), attributes(output$allAroon) )
  checkEqualsNumeric( ot, output$topAroon )
  checkEquals( attributes(ot), attributes(output$topAroon) )
  #checkException( aroon(im) )
}

test.aroon.xts <- function() {
  # xts
  checkEqualsNumeric( aroon(iAll[,hl]), output$allAroon )
  checkEquals( attributes(aroon(iAll[,hl])), attributes(output$allAroon) )
  checkEqualsNumeric( aroon(iTop[,hl]), output$topAroon )
  checkEquals( attributes(aroon(iTop[,hl])), attributes(output$topAroon) )
  #checkException( aroon(iMid[,hl]) )
}

test.aroon.non.na.eq.n.does.not.error <- function() {
  # xts
  x <- c(NA, rnorm(10))
  a <- aroon(x, 10)
  # error will prevent reaching here, failing test
  return(TRUE)
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
  checkEquals( attributes(aATR), attributes(output$allATR) )
  checkEqualsNumeric( tATR, output$topATR )
  checkEquals( attributes(tATR), attributes(output$topATR) )
  #checkException( ATR(im) )
}

test.ATR.xts <- function() {
  # xts
  checkEqualsNumeric( ATR(iAll[,hlc]), output$allATR )
  checkEquals( attributes(ATR(iAll[,hlc])), attributes(output$allATR) )
  checkEqualsNumeric( ATR(iTop[,hlc]), output$topATR )
  checkEquals( attributes(ATR(iTop[,hlc])), attributes(output$topATR) )
  #checkException( ATR(iMid[,hlc]) )
}

test.ATR.does.not.overwrite.maArgs <- function() {
  wilder.and.matype <- ATR(iAll[,hlc], maType = "EMA", wilder = FALSE)
  wilder.only <- ATR(iAll[,hlc], wilder = FALSE)
  checkEqualsNumeric( wilder.and.matype, wilder.only )
}

# Commodity Channel Index
test.CCI <- function() {
  checkEqualsNumeric( CCI(iAll[,hlc]), output$allCCI )
  checkEquals( attributes(CCI(iAll[,hlc])), attributes(output$allCCI) )
  checkEqualsNumeric( CCI(iTop[,hlc]), output$topCCI )
  checkEquals( attributes(CCI(iTop[,hlc])), attributes(output$topCCI) )
  #checkException( CCI(input$mid[,c('High','Low','Close')]) )
}

# Trend Detection Index
test.TDI <- function() {
  ia <- iAll[,cl]
  it <- iTop[,cl]
  names(ia) <- names(it) <- NULL
  checkEqualsNumeric( TDI(ia), output$allTDI )
  checkEquals( attributes(TDI(ia)), attributes(output$allTDI) )
  #checkEqualsNumeric( TDI(iTop[,cl]), output$topTDI )
  #checkException( TDI(iMid[,cl]) )
}

# Vertical Horizontal Filter
test.VHF <- function() {
  ia <- iAll[,cl]
  it <- iTop[,cl]
  names(ia) <- names(it) <- NULL
  checkEqualsNumeric( VHF(ia), output$allVHF )
  checkEquals( attributes(VHF(ia)), attributes(output$allVHF) )
  #checkEqualsNumeric( VHF(iTop[,cl]), output$topVHF )
  #checkException( VHF(iMid[,cl] )
}
