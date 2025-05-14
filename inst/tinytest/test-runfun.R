# Create input data
data(ttrc)
rownames(ttrc) <- ttrc$Date
ttrc$Date <- NULL

input <- list( all=ttrc[1:250,], top=ttrc[1:250,], mid=ttrc[1:250,] )
input$top[1:10,] <- NA
input$mid[9:20,] <- NA

# Load output data
load(system.file("tinytest/output/running-functions.rda", package="TTR"))

#################################################

# Sum
expect_equal( runSum(input$all$Close), output$allSum )
expect_equal( attributes(runSum(input$all$Close)), attributes(output$allSum) )
expect_equal( runSum(input$top$Close), output$topSum )
expect_equal( attributes(runSum(input$top$Close)), attributes(output$topSum) )
expect_error( runSum(input$mid$Close) )
expect_error( runSum(input$all[,1:2]) )
expect_equal( tail(runSum(input$all$Close,250),1), sum(input$all$Close) )
expect_error( runSum(input$all$Close, n = -1) )
expect_error( runSum(input$all$Close, n = NROW(input$all) + 1) )

# Wilder Sum
expect_equal( wilderSum(input$all$Close), output$allwSum )
expect_equal( attributes(wilderSum(input$all$Close)), attributes(output$allwSum) )
expect_equal( wilderSum(input$top$Close), output$topwSum )
expect_equal( attributes(wilderSum(input$top$Close)), attributes(output$topwSum) )
expect_error( wilderSum(input$mid$Close) )
expect_error( wilderSum(input$all[,1:2]) )
expect_error( wilderSum(input$all$Close, n = -1) )
expect_error( wilderSum(input$all$Close, n = NROW(input$all) + 1) )

# Min
expect_equal( runMin(input$all$Close), output$allMin )
expect_equal( attributes(runMin(input$all$Close)), attributes(output$allMin) )
expect_equal( runMin(input$top$Close), output$topMin )
expect_equal( attributes(runMin(input$top$Close)), attributes(output$topMin) )
expect_error( runMin(input$mid$Close) )
expect_error( runMin(input$all[,1:2]) )
expect_error( runMin(input$all$Close, n = -1) )
expect_error( runMin(input$all$Close, n = NROW(input$all) + 1) )
expect_equal( tail(runMin(input$all$Close,250),1), min(input$all$Close) )

ttr <- runMin(input$all$Close, 1, TRUE)
base <- cummin(input$all$Close)
expect_equal(base, ttr)

# Max
expect_equal( runMax(input$all$Close), output$allMax )
expect_equal( attributes(runMax(input$all$Close)), attributes(output$allMax) )
expect_equal( runMax(input$top$Close), output$topMax )
expect_equal( attributes(runMax(input$top$Close)), attributes(output$topMax) )
expect_error( runMax(input$mid$Close) )
expect_error( runMax(input$all[,1:2]) )
expect_error( runMax(input$all$Close, n = -1) )
expect_error( runMax(input$all$Close, n = NROW(input$all) + 1) )
expect_equal( tail(runMax(input$all$Close,250),1), max(input$all$Close) )

ttr <- runMax(input$all$Close, 1, TRUE)
base <- cummax(input$all$Close)
expect_equal(base, ttr)

# Mean
expect_equal( runMean(input$all$Close), output$allMean )
expect_equal( attributes(runMean(input$all$Close)), attributes(output$allMean) )
expect_equal( runMean(input$top$Close), output$topMean )
expect_equal( attributes(runMean(input$top$Close)), attributes(output$topMean) )
expect_error( runMean(input$mid$Close) )
expect_error( runMean(input$all[,1:2]) )
expect_error( runMean(input$all$Close, n = -1) )
expect_error( runMean(input$all$Close, n = NROW(input$all) + 1) )
expect_equal( tail(runMean(input$all$Close,250),1), mean(input$all$Close) )

ttr <- runMean(input$all$Close, 5, TRUE)
base <- cumsum(input$all$Close) / seq_along(input$all$Close)
is.na(base) <- 1:4
expect_equal(base, ttr)

n.1.cumul <- runMean(1, n = 1, cumulative = TRUE)
n.1.noncumul <- runMean(1, n = 1, cumulative = FALSE)
expect_equal(n.1.cumul, n.1.noncumul, info = "cumulative n = 1")

x <- c(rep(NA_real_, 5), 1:5)
target <- c(rep(NA_real_, 5), cumsum(1:5) / 1:5)
result <- runMean(x, n = 1, cumulative = TRUE)
expect_equal(target, result, info = "cumulative accounts for leading NA")

# Median
expect_equal( runMedian(input$all$Close), output$allMedian )
expect_equal( attributes(runMedian(input$all$Close)), attributes(output$allMedian) )
expect_equal( runMedian(input$top$Close), output$topMedian )
expect_equal( attributes(runMedian(input$top$Close)), attributes(output$topMedian) )
expect_error( runMedian(input$mid$Close) )
expect_error( runMedian(input$all[,1:2]) )
expect_error( runMedian(input$all$Close, n = -1) )
expect_error( runMedian(input$all$Close, n = NROW(input$all) + 1) )
expect_equal( tail(runMedian(input$all$Close,250),1), median(input$all$Close) )

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
expect_equal(base, ttr)

is.na(base) <- 1:5
ttr <- runMedian(input$all$Close, 6, "mean", TRUE)
expect_equal(base, ttr)


na <- rep(NA, 10)
x <- input$all$Close
xmed <- runMedian(x, 1, "mean", TRUE)
y <- c(na, input$all$Close)
ymed <- runMedian(y, 1, "mean", TRUE)
expect_equal(ymed, c(na, xmed), info = "cumulative n = 1")

n.1.cumul <- runMedian(1, n = 1, cumulative = TRUE)
n.1.noncumul <- runMedian(1, n = 1, cumulative = FALSE)
expect_equal(n.1.cumul, n.1.noncumul, info = "cumulative accounts for leading NA")

# Covariance
expect_equal( runCov(input$all$High, input$all$Low), output$allCov )
expect_equal( attributes(runCov(input$all$High, input$all$Low)), attributes(output$allCov) )
expect_equal( runCov(input$top$High, input$top$Low), output$topCov )
expect_equal( attributes(runCov(input$top$High, input$top$Low)), attributes(output$topCov) )
expect_error( runCov(input$mid$High, input$mid$Low) )
expect_error( runCov(input$all$High) )
expect_error( runCov(input$all[,1:2], input$all$Low) )
expect_error( runCov(input$all$Close, n = -1) )
expect_error( runCov(input$all$Close, n = NROW(input$all) + 1) )
expect_equal( tail(runCov(input$all$High, input$all$Low, 250),1), cov(input$all$High, input$all$Low) )
# x argument as xts object
xhi <- xts::as.xts(input$all)$High
xlo <- xts::as.xts(input$all)$Low
expect_equal( as.numeric(runCov(xhi, input$all$Low)), output$allCov )
# x and y arguments as xts objects
expect_equal( as.numeric(runCov(xhi, xlo)), output$allCov )

top <- input$top$Close
mid <- input$mid$Close
expect_error(runCov(top, mid))

expect_warning(runCov(1:10, 1:10, n = 1, cumulative = FALSE),
    info = "n = 1 and cumulative = FALSE throws warning")

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

is.na(base) <- 1
ttr <- runCov(x, x, 1, "all.obs", TRUE, TRUE)
expect_equal(base, ttr)

is.na(base) <- 1:4
ttr <- runCov(x, x, 5, "all.obs", TRUE, TRUE)
expect_equal(base, ttr)

is.na(base) <- 1:5
ttr <- runCov(x, x, 6, "all.obs", TRUE, TRUE)
expect_equal(base, ttr)

# Correlation
expect_equal( runCor(input$all$High, input$all$Low), output$allCor )
expect_equal( attributes(runCor(input$all$High, input$all$Low)), attributes(output$allCor) )
expect_equal( runCor(input$top$High, input$top$Low), output$topCor )
expect_equal( attributes(runCor(input$top$High, input$top$Low)), attributes(output$topCor) )
expect_error( runCor(input$mid$High, input$mid$Low) )
expect_error( runCor(input$all$High) )
expect_error( runCor(input$all[,1:2], input$all$Low) )
expect_error( runCor(input$all$Close, n = -1) )
expect_error( runCor(input$all$Close, n = NROW(input$all) + 1) )
expect_equal( tail(runCor(input$all$High, input$all$Low, 250),1), cor(input$all$High, input$all$Low) )

# Variance
expect_equal( runVar(input$all$Close), output$allVar )
expect_equal( attributes(runVar(input$all$Close)), attributes(output$allVar) )
expect_equal( runVar(input$top$Close), output$topVar )
expect_equal( attributes(runVar(input$top$Close)), attributes(output$topVar) )
expect_error( runVar(input$mid$Close) )
expect_error( runVar(input$all[,1:2], input$all$Low) )
expect_error( runVar(input$all$Close, n = -1) )
expect_error( runVar(input$all$Close, n = NROW(input$all) + 1) )
expect_equal( tail(runVar(input$all$Close,n=250),1), var(input$all$Close) )

# Standard Deviation
expect_equal( runSD(input$all$Close), output$allSD )
expect_equal( attributes(runSD(input$all$Close)), attributes(output$allSD) )
expect_equal( runSD(input$top$Close), output$topSD )
expect_equal( attributes(runSD(input$top$Close)), attributes(output$topSD) )
expect_error( runSD(input$mid$Close) )
expect_error( runSD(input$all[,1:2]) )
expect_error( runSD(input$all$Close, n = -1) )
expect_error( runSD(input$all$Close, n = NROW(input$all) + 1) )
expect_equal( tail(runSD(input$all$Close,250),1), sd(input$all$Close) )

x <- c(rep(NA_real_, 5), 1:5)
target <- sapply(1:5, function(i) sd(seq_len(i)))
target <- c(rep(NA, 5), target)
result <- runSD(x, n = 1, cumulative = TRUE)
expect_equal(target, result)

# Absolute deviation
expect_equal( runMAD(input$all$Close), output$allMAD )
expect_equal( attributes(runMAD(input$all$Close)), attributes(output$allMAD) )
expect_equal( runMAD(input$top$Close), output$topMAD )
expect_equal( attributes(runMAD(input$top$Close)), attributes(output$topMAD) )
expect_error( runMAD(input$mid$Close) )
expect_error( runMAD(input$all[,1:2]) )
expect_error( runMAD(input$all$Close, n = -1) )
expect_error( runMAD(input$all$Close, n = NROW(input$all) + 1) )
expect_equal( tail(runMAD(input$all$Close,250),1), mad(input$all$Close) )

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
expect_equal(base, ttr)

is.na(base) <- 1:5
ttr <- runMAD(x, 6, cumulative = TRUE)
expect_equal(base, ttr)

na <- rep(NA, 10)
x <- input$all$Close
xmed <- runMAD(x, 1, cumulative = TRUE)
y <- c(na, input$all$Close)
ymed <- runMAD(y, 1, cumulative = TRUE)
expect_equal(ymed, c(na, xmed))

# Percent Rank
x <- input$all$Close
expect_error( runPercentRank(x, 10, exact.multiplier = -0.1), info = "exact.multiplier bounds" )
expect_error( runPercentRank(x, 10, exact.multiplier = 1.1), info = "exact.multiplier bounds" )

xdata <- c(7.9, 5.2, 17.5, -12.7, 22, 4.3, -15.7, -9.3, 0.6, 0,
           -22.8, 7.6, -5.5, 1.7, 5.6, 15.1, 6.6, 11.2, -7.8, -4.3)
xrank10_1 <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.4, 0.1, 0.8,
               0.5, 0.7, 0.9, 1, 0.8, 0.9, 0.2, 0.4)
xrank10_0 <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.3, 0, 0.7,
               0.4, 0.6, 0.8, 0.9, 0.7, 0.8, 0.1, 0.3)

xrank <- round(xrank10_0, 2)
expect_identical(xrank, runPercentRank(xdata, 10, FALSE, 0), info = "exact.multiplier = 0")

xrank <- round(xrank10_0 + 0.05, 2)
expect_identical(xrank, runPercentRank(xdata, 10, FALSE, 0.5), info = "exact.multiplier = 0.5")

xrank <- round(xrank10_0 + 0.1, 2)
expect_identical(xrank, runPercentRank(xdata, 10, FALSE, 1), info = "exact.multiplier = 1")

xrank <- c(0, 0, 2, 0, 4, 1, 0, 2, 3, 3, 0, 8,
           4, 7, 10, 13, 11, 14, 4, 6) / 1:20
xrank[1:9] <- NA
xrank[10] <- 0
expect_identical(xrank, runPercentRank(xdata, 10, TRUE, 0),
                 info = "exact.multiplier = 0, cumulative = TRUE")

xrank <- (c(0, 0, 2, 0, 4, 1, 0, 2, 3, 3, 0, 8,
           4, 7, 10, 13, 11, 14, 4, 6) + 0.5) / 1:20
xrank[1:9] <- NA
xrank[10] <- 0.5
expect_identical(xrank, runPercentRank(xdata, 10, TRUE, 0.5),
                 info = "exact.multiplier = 0.5, cumulative = TRUE")

xrank <- (c(0, 0, 2, 0, 4, 1, 0, 2, 3, 3, 0, 8,
           4, 7, 10, 13, 11, 14, 4, 6) + 1) / 1:20
xrank[1:9] <- NA
xrank[10] <- 1
expect_identical(xrank, runPercentRank(xdata, 10, TRUE, 1),
                 info = "exact.multiplier = 1, cumulative = TRUE")
