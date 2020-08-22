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
load(system.file("unitTests/output.Oscillators.rda", package="TTR"))

#################################################

# MACD
test.MACD <- function() {
  checkEqualsNumeric( MACD(input$all$Close), output$allMACD )
  checkEquals( attributes(MACD(input$all$Close)), attributes(output$allMACD) )
  checkEqualsNumeric( MACD(input$top$Close), output$topMACD )
  checkEquals( attributes(MACD(input$top$Close)), attributes(output$topMACD) )
  checkException( MACD(input$mid$Close) )
}

# Stochastics
test.stoch <- function() {
  # This mess is because data.frames' attributes don't come through reclass() well
  ia <- as.matrix(input$all[,c('High','Low','Close')])
  it <- as.matrix(input$top[,c('High','Low','Close')])
  #rn <- rownames(ia)
  #rownames(ia) <- rownames(it) <- NULL
  oa <- stoch(ia); #rownames(oa) <- rn
  ot <- stoch(it); #rownames(ot) <- rn
  # End: mess
  checkEqualsNumeric( oa, output$allStoch )
  checkEquals( attributes(oa), attributes(output$allStoch) )
  checkEqualsNumeric( ot, output$topStoch )
  checkEquals( attributes(ot), attributes(output$topStoch) )
  checkException( stoch(input$mid[,c('High','Low','Close')]) )
}

test.stoch.for.Inf.fastK <- function() {
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
  checkEqualsNumeric(s, o)
  # TODO: delete line above after xts is released w/fix for #322
  #checkEquals(s, o
}

# Stochastic Momentum Index
test.SMI <- function() {
  checkEqualsNumeric( SMI(input$all[,c('High','Low','Close')]), output$allSMI )
  checkEquals( attributes(SMI(input$all[,c('High','Low','Close')])), attributes(output$allSMI) )
  checkEqualsNumeric( SMI(input$top[,c('High','Low','Close')]), output$topSMI )
  checkEquals( attributes(SMI(input$top[,c('High','Low','Close')])), attributes(output$topSMI) )
  checkException( SMI(input$mid[,c('High','Low','Close')]) )
}

# Relative Strength Index
test.RSI <- function() {
  checkEqualsNumeric( RSI(input$all$Close), output$allRSI )
  checkEquals( attributes(RSI(input$all$Close)), attributes(output$allRSI) )
  checkEqualsNumeric( RSI(input$top$Close), output$topRSI )
  checkEquals( attributes(RSI(input$top$Close)), attributes(output$topRSI) )
  checkException( RSI(input$mid$Close) )
}

test.RSI.does.not.overwrite.maArgs <- function() {
  wilder.and.matype <- RSI(input$all$Close, maType = "EMA", wilder = FALSE)
  wilder.only <- RSI(input$all$Close, wilder = FALSE)
  checkEqualsNumeric( wilder.and.matype, wilder.only )
}

# Chande Momentum Oscillator
test.CMO <- function() {
  checkEqualsNumeric( CMO(input$all$Close), output$allCMO )
  checkEquals( attributes(CMO(input$all$Close)), attributes(output$allCMO) )
  checkEqualsNumeric( CMO(input$top$Close), output$topCMO )
  checkEquals( attributes(CMO(input$top$Close)), attributes(output$topCMO) )
  checkException( CMO(input$mid$Close) )
}

# De-trended Price Oscillator
test.DPO <- function() {
  checkEqualsNumeric( DPO(input$all$Close), output$allDPO )
  checkEquals( attributes(DPO(input$all$Close)), attributes(output$allDPO) )
  checkEqualsNumeric( DPO(input$top$Close), output$topDPO )
  checkEquals( attributes(DPO(input$top$Close)), attributes(output$topDPO) )
  checkException( DPO(input$mid$Close) )
}

# TRIX
test.TRIX <- function() {
  checkEqualsNumeric( TRIX(input$all$Close), output$allTRIX )
  checkEquals( attributes(TRIX(input$all$Close)), attributes(output$allTRIX) )
  checkEqualsNumeric( TRIX(input$top$Close), output$topTRIX )
  checkEquals( attributes(TRIX(input$top$Close)), attributes(output$topTRIX) )
  checkException( TRIX(input$mid$Close) )
}

# Willams' Percent R
test.WPR <- function() {
  # This mess is because data.frames' attributes don't come through reclass() well
  ia <- input$all[,c('High','Low','Close')]
  it <- input$top[,c('High','Low','Close')]
  rn <- rownames(ia)
  rownames(ia) <- rownames(it) <- NULL
  oa <- WPR(ia); names(oa) <- rn
  ot <- WPR(it); names(ot) <- rn
  # End: mess
  checkEqualsNumeric( oa, output$allWPR )
  checkEquals( attributes(oa), attributes(output$allWPR) )
  checkEqualsNumeric( ot, output$topWPR )
  checkEquals( attributes(ot), attributes(output$topWPR) )
  checkException( WPR(input$mid$Close) )
}

# Ultimate Oscillator
test.ultimateOscillator <- function() {
  # This mess is because data.frames' attributes don't come through reclass() well
  ia <- input$all[,c('High','Low','Close')]
  it <- input$top[,c('High','Low','Close')]
  rn <- rownames(ia)
  rownames(ia) <- rownames(it) <- NULL
  oa <- ultimateOscillator(ia); names(oa) <- rn
  ot <- ultimateOscillator(it); names(ot) <- rn
  # End: mess
  checkEqualsNumeric( oa, output$allUltOsc )
  checkEquals( attributes(oa), attributes(output$allUltOsc) )
  checkEqualsNumeric( ot, output$topUltOsc )
  checkEquals( attributes(ot), attributes(output$topUltOsc) )
  checkException( ultimateOscillator(input$mid$Close) )
}

test.ultimateOscillator.monthly.xts <- function() {
  stopifnot(requireNamespace("xts"))
  # Ultimate Oscillator on non-xts monthly data
  iam <- xts::to.monthly(input$all, name=NULL)[,c('High','Low','Close')]
  rn <- rownames(iam)
  rownames(iam) <- NULL
  oam <- ultimateOscillator(iam, c(2,5,8))
  # Ultimate Oscillator on xts monthly data
  xia <- xts::as.xts(input$all)
  xiam <- xts::to.monthly(xia, name=NULL)[,c('High','Low','Close')]
  xoam <- ultimateOscillator(xiam, c(2,5,8))
  checkEqualsNumeric( oam, xoam )
}
