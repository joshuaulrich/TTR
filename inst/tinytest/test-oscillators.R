# Create input data
data(ttrc)
rownames(ttrc) <- ttrc$Date
ttrc$Date <- NULL

input <- list( all=ttrc[1:250,], top=ttrc[1:250,], mid=ttrc[1:250,] )
input$top[1:10,] <- NA
input$mid[9:20,] <- NA

# Load output data
load(system.file("tinytest/output/oscillators.rda", package="TTR"))

#################################################

# MACD
expect_equal( MACD(input$all$Close), output$allMACD )
expect_equal( attributes(MACD(input$all$Close)), attributes(output$allMACD) )
expect_equal( MACD(input$top$Close), output$topMACD )
expect_equal( attributes(MACD(input$top$Close)), attributes(output$topMACD) )
expect_error( MACD(input$mid$Close) )

# Stochastics
# This mess is because data.frames' attributes don't come through reclass() well
ia <- as.matrix(input$all[,c('High','Low','Close')])
it <- as.matrix(input$top[,c('High','Low','Close')])
#rn <- rownames(ia)
#rownames(ia) <- rownames(it) <- NULL
oa <- stoch(ia); #rownames(oa) <- rn
ot <- stoch(it); #rownames(ot) <- rn
# End: mess
expect_equal( oa, output$allStoch )
expect_equal( attributes(oa), attributes(output$allStoch) )
expect_equal( ot, output$topStoch )
expect_equal( attributes(ot), attributes(output$topStoch) )
expect_error( stoch(input$mid[,c('High','Low','Close')]) )

a <- c(53.99, 54.69, rep(55.55, 3), rep(52.5, 13), rep(51.77, 2))
idx <- structure(1446422400 + cumsum(c(0, rep(86400, 4), 259200,
   rep(86400, 4), 259200, rep(86400,4), 259200, rep(86400, 2), 172800,
   259200)), tzone = "UTC", tclass = "Date")
X <- structure(c(a, a, a+0.1), .Dim = c(20L, 3L), class = c("xts", "zoo"),
      index = idx, .Dimnames = list(NULL, c("High", "Low", "Close")))

o <- structure(c(rep(NA, 9), rep(0.0327868852459021, 5), rep(0.5, 4),
    rep(0.136986301369856, 2), rep(NA, 11), rep(0.0327868852459021, 3),
    0.188524590163935, 0.344262295081967, 0.5, 0.5, 0.378995433789952,
    0.257990867579904, rep(NA, 13), 0.0327868852459021, 0.084699453551913,
    0.188524590163935, 0.344262295081967, 0.448087431693989,
    0.459665144596651, 0.378995433789952), .Dim = c(20L, 3L),
    .Dimnames = list(NULL, c("fastK", "fastD", "slowD")), index = idx,
    class = c("xts", "zoo"))

s <- TTR::stoch(X, 10, 3)
expect_equal(s, o, info = "handle Inf fastK")
# TODO: delete line above after xts is released w/fix for #322
#expect_equal(s, o


# Stochastic Momentum Index
expect_equal( SMI(input$all[,c('High','Low','Close')]), output$allSMI )
expect_equal( attributes(SMI(input$all[,c('High','Low','Close')])), attributes(output$allSMI) )
expect_equal( SMI(input$top[,c('High','Low','Close')]), output$topSMI )
expect_equal( attributes(SMI(input$top[,c('High','Low','Close')])), attributes(output$topSMI) )
expect_error( SMI(input$mid[,c('High','Low','Close')]) )

# Relative Strength Index
expect_equal( RSI(input$all$Close), output$allRSI )
expect_equal( attributes(RSI(input$all$Close)), attributes(output$allRSI) )
expect_equal( RSI(input$top$Close), output$topRSI )
expect_equal( attributes(RSI(input$top$Close)), attributes(output$topRSI) )
expect_error( RSI(input$mid$Close) )

wilder.and.matype <- RSI(input$all$Close, maType = "EMA", wilder = FALSE)
wilder.only <- RSI(input$all$Close, wilder = FALSE)
expect_equal( wilder.and.matype, wilder.only, info = "RSI does not overwrite maArgs")

# Chande Momentum Oscillator
expect_equal( CMO(input$all$Close), output$allCMO )
expect_equal( attributes(CMO(input$all$Close)), attributes(output$allCMO) )
expect_equal( CMO(input$top$Close), output$topCMO )
expect_equal( attributes(CMO(input$top$Close)), attributes(output$topCMO) )
expect_error( CMO(input$mid$Close) )

# De-trended Price Oscillator
expect_equal( DPO(input$all$Close), output$allDPO )
expect_equal( attributes(DPO(input$all$Close)), attributes(output$allDPO) )
expect_equal( DPO(input$top$Close), output$topDPO )
expect_equal( attributes(DPO(input$top$Close)), attributes(output$topDPO) )
expect_error( DPO(input$mid$Close) )

# TRIX
expect_equal( TRIX(input$all$Close), output$allTRIX )
expect_equal( attributes(TRIX(input$all$Close)), attributes(output$allTRIX) )
expect_equal( TRIX(input$top$Close), output$topTRIX )
expect_equal( attributes(TRIX(input$top$Close)), attributes(output$topTRIX) )
expect_error( TRIX(input$mid$Close) )

# Willams' Percent R
# This mess is because data.frames' attributes don't come through reclass() well
ia <- input$all[,c('High','Low','Close')]
it <- input$top[,c('High','Low','Close')]
rn <- rownames(ia)
rownames(ia) <- rownames(it) <- NULL
oa <- WPR(ia); names(oa) <- rn
ot <- WPR(it); names(ot) <- rn
# End: mess
expect_equal( oa, output$allWPR )
expect_equal( attributes(oa), attributes(output$allWPR) )
expect_equal( ot, output$topWPR )
expect_equal( attributes(ot), attributes(output$topWPR) )
expect_error( WPR(input$mid$Close) )

# Ultimate Oscillator
# This mess is because data.frames' attributes don't come through reclass() well
ia <- input$all[,c('High','Low','Close')]
it <- input$top[,c('High','Low','Close')]
rn <- rownames(ia)
rownames(ia) <- rownames(it) <- NULL
oa <- ultimateOscillator(ia); names(oa) <- rn
ot <- ultimateOscillator(it); names(ot) <- rn
# End: mess
expect_equal( oa, output$allUltOsc )
expect_equal( attributes(oa), attributes(output$allUltOsc) )
expect_equal( ot, output$topUltOsc )
expect_equal( attributes(ot), attributes(output$topUltOsc) )
expect_error( ultimateOscillator(input$mid$Close) )

# Ultimate Oscillator on non-xts monthly data
iam <- xts::to.monthly(input$all, name=NULL)[,c('High','Low','Close')]
rn <- rownames(iam)
rownames(iam) <- NULL
oam <- ultimateOscillator(iam, c(2,5,8))
# Ultimate Oscillator on xts monthly data
xia <- xts::as.xts(input$all)
xiam <- xts::to.monthly(xia, name=NULL)[,c('High','Low','Close')]
xoam <- ultimateOscillator(xiam, c(2,5,8))
expect_equal( oam, as.numeric(xoam) )
