#
# RUnit tests TTR running/rolling functions
#

# Create input data
data(ttrc)
rownames(ttrc) <- ttrc$Date
ttrc$Date <- NULL

input <- list( all=ttrc[1:250,], top=ttrc[1:250,], mid=ttrc[1:250,] )
input$top[1:10,] <- NA
input$mid[9:20,] <- NA

# Load output data
load(system.file("unitTests/output.runFun.rda", package="TTR"))

#################################################

# Sum
test.runSum <- function() {
  checkEqualsNumeric( runSum(input$all$Close), output$allSum )
  checkEquals( attributes(runSum(input$all$Close)), attributes(output$allSum) )
  checkEqualsNumeric( runSum(input$top$Close), output$topSum )
  checkEquals( attributes(runSum(input$top$Close)), attributes(output$topSum) )
  checkException( runSum(input$mid$Close) )
  checkException( runSum(input$all[,1:2]) )
  checkEqualsNumeric( tail(runSum(input$all$Close,250),1), sum(input$all$Close) )
  checkException( runSum(input$all$Close, n = -1) )
  checkException( runSum(input$all$Close, n = NROW(input$all) + 1) )
}

# Wilder Sum
test.wilderSum <- function() {
  checkEqualsNumeric( wilderSum(input$all$Close), output$allwSum )
  checkEquals( attributes(wilderSum(input$all$Close)), attributes(output$allwSum) )
  checkEqualsNumeric( wilderSum(input$top$Close), output$topwSum )
  checkEquals( attributes(wilderSum(input$top$Close)), attributes(output$topwSum) )
  checkException( wilderSum(input$mid$Close) )
  checkException( wilderSum(input$all[,1:2]) )
  checkException( wilderSum(input$all$Close, n = -1) )
  checkException( wilderSum(input$all$Close, n = NROW(input$all) + 1) )
}

# Min
test.runMin <- function() {
  checkEqualsNumeric( runMin(input$all$Close), output$allMin )
  checkEquals( attributes(runMin(input$all$Close)), attributes(output$allMin) )
  checkEqualsNumeric( runMin(input$top$Close), output$topMin )
  checkEquals( attributes(runMin(input$top$Close)), attributes(output$topMin) )
  checkException( runMin(input$mid$Close) )
  checkException( runMin(input$all[,1:2]) )
  checkException( runMin(input$all$Close, n = -1) )
  checkException( runMin(input$all$Close, n = NROW(input$all) + 1) )
  checkEqualsNumeric( tail(runMin(input$all$Close,250),1), min(input$all$Close) )
}
test.runMin.cumulative <- function() {
  ttr <- runMin(input$all$Close, 1, TRUE)
  base <- cummin(input$all$Close)
  checkEqualsNumeric(base, ttr)
}

# Max
test.runMax <- function() {
  checkEqualsNumeric( runMax(input$all$Close), output$allMax )
  checkEquals( attributes(runMax(input$all$Close)), attributes(output$allMax) )
  checkEqualsNumeric( runMax(input$top$Close), output$topMax )
  checkEquals( attributes(runMax(input$top$Close)), attributes(output$topMax) )
  checkException( runMax(input$mid$Close) )
  checkException( runMax(input$all[,1:2]) )
  checkException( runMax(input$all$Close, n = -1) )
  checkException( runMax(input$all$Close, n = NROW(input$all) + 1) )
  checkEqualsNumeric( tail(runMax(input$all$Close,250),1), max(input$all$Close) )
}
test.runMax.cumulative <- function() {
  ttr <- runMax(input$all$Close, 1, TRUE)
  base <- cummax(input$all$Close)
  checkEqualsNumeric(base, ttr)
}

# Mean
test.runMean <- function() {
  checkEqualsNumeric( runMean(input$all$Close), output$allMean )
  checkEquals( attributes(runMean(input$all$Close)), attributes(output$allMean) )
  checkEqualsNumeric( runMean(input$top$Close), output$topMean )
  checkEquals( attributes(runMean(input$top$Close)), attributes(output$topMean) )
  checkException( runMean(input$mid$Close) )
  checkException( runMean(input$all[,1:2]) )
  checkException( runMean(input$all$Close, n = -1) )
  checkException( runMean(input$all$Close, n = NROW(input$all) + 1) )
  checkEqualsNumeric( tail(runMean(input$all$Close,250),1), mean(input$all$Close) )
}
test.runMean.cumulative <- function() {
  ttr <- runMean(input$all$Close, 5, TRUE)
  base <- cumsum(input$all$Close) / seq_along(input$all$Close)
  is.na(base) <- 1:4
  checkEqualsNumeric(base, ttr)
}
test.runMean.cumulative.n.equals.1 <- function() {
  n.1.cum <- runMean(1, n = 1, cumulative = TRUE)
  n.1.noncum <- runMean(1, n = 1, cumulative = FALSE)
  checkEqualsNumeric(n.1.cum, n.1.noncum)
}


# Median
test.runMedian <- function() {
  checkEqualsNumeric( runMedian(input$all$Close), output$allMedian )
  checkEquals( attributes(runMedian(input$all$Close)), attributes(output$allMedian) )
  checkEqualsNumeric( runMedian(input$top$Close), output$topMedian )
  checkEquals( attributes(runMedian(input$top$Close)), attributes(output$topMedian) )
  checkException( runMedian(input$mid$Close) )
  checkException( runMedian(input$all[,1:2]) )
  checkException( runMedian(input$all$Close, n = -1) )
  checkException( runMedian(input$all$Close, n = NROW(input$all) + 1) )
  checkEqualsNumeric( tail(runMedian(input$all$Close,250),1), median(input$all$Close) )
}
test.runMedian.cumulative <- function() {
  cummedian <- compiler::cmpfun(
    function(x) {
      med <- x * NA_real_
      for (i in seq_along(x)) {
        med[i] <- median(x[1:i])
      }
      med
    }
  )
  base <- cummedian(input$all$Close)
  is.na(base) <- 1:4
  ttr <- runMedian(input$all$Close, 5, "mean", TRUE)
  checkEqualsNumeric(base, ttr)

  is.na(base) <- 1:5
  ttr <- runMedian(input$all$Close, 6, "mean", TRUE)
  checkEqualsNumeric(base, ttr)
}

test.runMedian.cumulative.leading.NA <- function() {
  na <- rep(NA, 10)
  x <- input$all$Close
  xmed <- runMedian(x, 1, "mean", TRUE)
  y <- c(na, input$all$Close)
  ymed <- runMedian(y, 1, "mean", TRUE)
  checkEqualsNumeric(ymed, c(na, xmed))
}
test.runMedian.cumulative.n.equals.1 <- function() {
  n.1.cum <- runMedian(1, n = 1, cumulative = TRUE)
  n.1.noncum <- runMedian(1, n = 1, cumulative = FALSE)
  checkEqualsNumeric(n.1.cum, n.1.noncum)
}

# Covariance
test.runCov <- function() {
  checkEqualsNumeric( runCov(input$all$High, input$all$Low), output$allCov )
  checkEquals( attributes(runCov(input$all$High, input$all$Low)), attributes(output$allCov) )
  checkEqualsNumeric( runCov(input$top$High, input$top$Low), output$topCov )
  checkEquals( attributes(runCov(input$top$High, input$top$Low)), attributes(output$topCov) )
  checkException( runCov(input$mid$High, input$mid$Low) )
  checkException( runCov(input$all$High) )
  checkException( runCov(input$all[,1:2], input$all$Low) )
  checkException( runCov(input$all$Close, n = -1) )
  checkException( runCov(input$all$Close, n = NROW(input$all) + 1) )
  checkEqualsNumeric( tail(runCov(input$all$High, input$all$Low, 250),1), cov(input$all$High, input$all$Low) )
  # x argument as xts object
  checkEqualsNumeric( runCov(xts::as.xts(input$all)$High, input$all$Low), output$allCov )
  # x and y arguments as xts objects
  checkEqualsNumeric( runCov(xts::as.xts(input$all)$High, xts::as.xts(input$all)$Low), output$allCov )
}

test.runCov.xts.nonleading.na <- function() {
  top <- input$top$Close
  mid <- input$mid$Close
  checkException(runCov(top, mid))
}

test.runCov.cumulative <- function() {
  cumcov <- compiler::cmpfun(
    function(x) {
      cov <- x * NA_real_
      for (i in seq_along(x)) {
        y <- x[1:i]
        cov[i] <- cov(y, y)
      }
      cov
    }
  )
  x <- input$all$Close
  base <- cumcov(x)
  is.na(base) <- 1:4
  ttr <- runCov(x, x, 5, "all.obs", TRUE, TRUE)
  checkEqualsNumeric(base, ttr)

  is.na(base) <- 1:5
  ttr <- runCov(x, x, 6, "all.obs", TRUE, TRUE)
  checkEqualsNumeric(base, ttr)
}


# Correlation
test.runCor <- function() {
  checkEqualsNumeric( runCor(input$all$High, input$all$Low), output$allCor )
  checkEquals( attributes(runCor(input$all$High, input$all$Low)), attributes(output$allCor) )
  checkEqualsNumeric( runCor(input$top$High, input$top$Low), output$topCor )
  checkEquals( attributes(runCor(input$top$High, input$top$Low)), attributes(output$topCor) )
  checkException( runCor(input$mid$High, input$mid$Low) )
  checkException( runCor(input$all$High) )
  checkException( runCor(input$all[,1:2], input$all$Low) )
  checkException( runCor(input$all$Close, n = -1) )
  checkException( runCor(input$all$Close, n = NROW(input$all) + 1) )
  checkEqualsNumeric( tail(runCor(input$all$High, input$all$Low, 250),1), cor(input$all$High, input$all$Low) )
}

# Variance
test.runVar <- function() {
  checkEqualsNumeric( runVar(input$all$Close), output$allVar )
  checkEquals( attributes(runVar(input$all$Close)), attributes(output$allVar) )
  checkEqualsNumeric( runVar(input$top$Close), output$topVar )
  checkEquals( attributes(runVar(input$top$Close)), attributes(output$topVar) )
  checkException( runVar(input$mid$Close) )
  checkException( runVar(input$all[,1:2], input$all$Low) )
  checkException( runVar(input$all$Close, n = -1) )
  checkException( runVar(input$all$Close, n = NROW(input$all) + 1) )
  checkEqualsNumeric( tail(runVar(input$all$Close,n=250),1), var(input$all$Close) )
}

# Standard Deviation
test.runSD <- function() {
  checkEqualsNumeric( runSD(input$all$Close), output$allSD )
  checkEquals( attributes(runSD(input$all$Close)), attributes(output$allSD) )
  checkEqualsNumeric( runSD(input$top$Close), output$topSD )
  checkEquals( attributes(runSD(input$top$Close)), attributes(output$topSD) )
  checkException( runSD(input$mid$Close) )
  checkException( runSD(input$all[,1:2]) )
  checkException( runSD(input$all$Close, n = -1) )
  checkException( runSD(input$all$Close, n = NROW(input$all) + 1) )
  checkEqualsNumeric( tail(runSD(input$all$Close,250),1), sd(input$all$Close) )
}

# Absolute deviation
test.runMAD <- function() {
  checkEqualsNumeric( runMAD(input$all$Close), output$allMAD )
  checkEquals( attributes(runMAD(input$all$Close)), attributes(output$allMAD) )
  checkEqualsNumeric( runMAD(input$top$Close), output$topMAD )
  checkEquals( attributes(runMAD(input$top$Close)), attributes(output$topMAD) )
  checkException( runMAD(input$mid$Close) )
  checkException( runMAD(input$all[,1:2]) )
  checkException( runMAD(input$all$Close, n = -1) )
  checkException( runMAD(input$all$Close, n = NROW(input$all) + 1) )
  checkEqualsNumeric( tail(runMAD(input$all$Close,250),1), mad(input$all$Close) )
}

test.runMAD.cumulative <- function() {
  cummad <- compiler::cmpfun(
    function(x) {
      mad <- x * NA_real_
      for (i in seq_along(x)) {
        y <- x[1:i]
        mad[i] <- mad(y)
      }
      mad
    }
  )
  x <- input$all$Close
  base <- cummad(x)
  is.na(base) <- 1:4
  ttr <- runMAD(x, 5, cumulative = TRUE)
  checkEqualsNumeric(base, ttr)

  is.na(base) <- 1:5
  ttr <- runMAD(x, 6, cumulative = TRUE)
  checkEqualsNumeric(base, ttr)
}

test.runMAD.cumulative.leading.NA <- function() {
  na <- rep(NA, 10)
  x <- input$all$Close
  xmed <- runMAD(x, 1, cumulative = TRUE)
  y <- c(na, input$all$Close)
  ymed <- runMAD(y, 1, cumulative = TRUE)
  checkEqualsNumeric(ymed, c(na, xmed))
}


# Percent Rank
test.runPercentRank_exact.multiplier_bounds <- function() {
  x <- input$all$Close
  checkException( runPercentRank(x, 10, exact.multiplier = -0.1) )
  checkException( runPercentRank(x, 10, exact.multiplier = 1.1) )
}

xdata <- c(7.9, 5.2, 17.5, -12.7, 22, 4.3, -15.7, -9.3, 0.6, 0,
           -22.8, 7.6, -5.5, 1.7, 5.6, 15.1, 6.6, 11.2, -7.8, -4.3)
xrank10_1 <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.4, 0.1, 0.8,
               0.5, 0.7, 0.9, 1, 0.8, 0.9, 0.2, 0.4)

xrank10_0 <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.3, 0, 0.7,
               0.4, 0.6, 0.8, 0.9, 0.7, 0.8, 0.1, 0.3)


test.runPercentRank_exact.multiplier_eq0 <- function() {
  xrank <- round(xrank10_0, 2)
  checkIdentical(xrank, runPercentRank(xdata, 10, FALSE, 0))
}

test.runPercentRank_exact.multiplier_eq0.5 <- function() {
  xrank <- round(xrank10_0 + 0.05, 2)
  checkIdentical(xrank, runPercentRank(xdata, 10, FALSE, 0.5))
}

test.runPercentRank_exact.multiplier_eq1 <- function() {
  xrank <- round(xrank10_0 + 0.1, 2)
  checkIdentical(xrank, runPercentRank(xdata, 10, FALSE, 1))
}

test.runPercentRank_cumulTRUE_exact.multiplier_eq0 <- function() {
  xrank <- c(0, 0, 2, 0, 4, 1, 0, 2, 3, 3, 0, 8,
             4, 7, 10, 13, 11, 14, 4, 6) / 1:20
  xrank[1:9] <- NA
  xrank[10] <- 0
  checkIdentical(xrank, runPercentRank(xdata, 10, TRUE, 0))
}

test.runPercentRank_cumulTRUE_exact.multiplier_eq0.5 <- function() {
  xrank <- (c(0, 0, 2, 0, 4, 1, 0, 2, 3, 3, 0, 8,
             4, 7, 10, 13, 11, 14, 4, 6) + 0.5) / 1:20
  xrank[1:9] <- NA
  xrank[10] <- 0.5
  checkIdentical(xrank, runPercentRank(xdata, 10, TRUE, 0.5))
}

test.runPercentRank_cumulTRUE_exact.multiplier_eq1 <- function() {
  xrank <- (c(0, 0, 2, 0, 4, 1, 0, 2, 3, 3, 0, 8,
             4, 7, 10, 13, 11, 14, 4, 6) + 1) / 1:20
  xrank[1:9] <- NA
  xrank[10] <- 1
  checkIdentical(xrank, runPercentRank(xdata, 10, TRUE, 1))
}
