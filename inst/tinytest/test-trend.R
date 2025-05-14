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
load(system.file("tinytest/output/trend.rda", package="TTR"))

#################################################

# ADX
expect_equal( ADX(iAll[,hlc]), output$allADX )
expect_equal( attributes(ADX(iAll[,hlc])), attributes(output$allADX) )
expect_equal( ADX(iTop[,hlc]), output$topADX )
expect_equal( attributes(ADX(iTop[,hlc])), attributes(output$topADX) )
#expect_error( ADX(iMid[,hlc]) )

wilder.and.matype <- ADX(iAll[,hlc], maType = "EMA", wilder = FALSE)
wilder.only <- ADX(iAll[,hlc], wilder = FALSE)
expect_equal( wilder.and.matype, wilder.only, info = "ADX does not overwrite maArgs" )

# Aroon
# non-xts
ia <- iAll[,hl]
it <- iTop[,hl]
im <- iMid[,hl]
rownames(ia) <- rownames(it) <- rownames(im) <- NULL
oa <- aroon(ia);  rownames(oa) <- rownames(iAll)
ot <- aroon(it);  rownames(ot) <- rownames(iTop)
expect_equal( oa, output$allAroon )
expect_equal( attributes(oa), attributes(output$allAroon) )
expect_equal( ot, output$topAroon )
expect_equal( attributes(ot), attributes(output$topAroon) )
#expect_error( aroon(im) )

# xts
expect_equal( aroon(iAll[,hl]), output$allAroon )
expect_equal( attributes(aroon(iAll[,hl])), attributes(output$allAroon) )
expect_equal( aroon(iTop[,hl]), output$topAroon )
expect_equal( attributes(aroon(iTop[,hl])), attributes(output$topAroon) )
#expect_error( aroon(iMid[,hl]) )

x <- c(NA, rnorm(10))
expect_silent(aroon(x, 10), info = "non-na equals 'n' does not error")

# Average True Range
# non-xts
ia <- iAll[,hlc]
it <- iTop[,hlc]
im <- iMid[,hlc]
rownames(ia) <- rownames(it) <- rownames(im) <- NULL
aATR <- ATR(ia);  rownames(aATR) <- rownames(iAll)
tATR <- ATR(it);  rownames(tATR) <- rownames(iTop)
expect_equal( aATR, output$allATR )
expect_equal( attributes(aATR), attributes(output$allATR) )
expect_equal( tATR, output$topATR )
expect_equal( attributes(tATR), attributes(output$topATR) )
#expect_error( ATR(im) )

# xts
expect_equal( ATR(iAll[,hlc]), output$allATR )
expect_equal( attributes(ATR(iAll[,hlc])), attributes(output$allATR) )
expect_equal( ATR(iTop[,hlc]), output$topATR )
expect_equal( attributes(ATR(iTop[,hlc])), attributes(output$topATR) )
#expect_error( ATR(iMid[,hlc]) )

wilder.and.matype <- ATR(iAll[,hlc], maType = "EMA", wilder = FALSE)
wilder.only <- ATR(iAll[,hlc], wilder = FALSE)
expect_equal( wilder.and.matype, wilder.only, info = "ATR does not overwrite maArgs" )

# Commodity Channel Index
expect_equal( CCI(iAll[,hlc]), output$allCCI )
expect_equal( attributes(CCI(iAll[,hlc])), attributes(output$allCCI) )
expect_equal( CCI(iTop[,hlc]), output$topCCI )
expect_equal( attributes(CCI(iTop[,hlc])), attributes(output$topCCI) )
#expect_error( CCI(input$mid[,c('High','Low','Close')]) )

# Trend Detection Index
ia <- iAll[,cl]
it <- iTop[,cl]
names(ia) <- names(it) <- NULL
expect_equal( TDI(ia), output$allTDI )
expect_equal( attributes(TDI(ia)), attributes(output$allTDI) )
#expect_equal( TDI(iTop[,cl]), output$topTDI )
#expect_error( TDI(iMid[,cl]) )

# Vertical Horizontal Filter
ia <- iAll[,cl]
it <- iTop[,cl]
names(ia) <- names(it) <- NULL
expect_equal( VHF(ia), output$allVHF )
expect_equal( attributes(VHF(ia)), attributes(output$allVHF) )
#expect_equal( VHF(iTop[,cl]), output$topVHF )
#expect_error( VHF(iMid[,cl] )
