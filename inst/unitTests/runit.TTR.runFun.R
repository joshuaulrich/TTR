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
}

# Wilder Sum
test.wilderSum <- function() {
  checkEqualsNumeric( wilderSum(input$all$Close), output$allwSum )
  checkEquals( attributes(wilderSum(input$all$Close)), attributes(output$allwSum) )
  checkEqualsNumeric( wilderSum(input$top$Close), output$topwSum )
  checkEquals( attributes(wilderSum(input$top$Close)), attributes(output$topwSum) )
  checkException( wilderSum(input$mid$Close) )
  checkException( wilderSum(input$all[,1:2]) )
}

# Min
test.runMin <- function() {
  checkEqualsNumeric( runMin(input$all$Close), output$allMin )
  checkEquals( attributes(runMin(input$all$Close)), attributes(output$allMin) )
  checkEqualsNumeric( runMin(input$top$Close), output$topMin )
  checkEquals( attributes(runMin(input$top$Close)), attributes(output$topMin) )
  checkException( runMin(input$mid$Close) )
  checkException( runMin(input$all[,1:2]) )
  checkEqualsNumeric( tail(runMin(input$all$Close,250),1), min(input$all$Close) )
}

# Max
test.runMax <- function() {
  checkEqualsNumeric( runMax(input$all$Close), output$allMax )
  checkEquals( attributes(runMax(input$all$Close)), attributes(output$allMax) )
  checkEqualsNumeric( runMax(input$top$Close), output$topMax )
  checkEquals( attributes(runMax(input$top$Close)), attributes(output$topMax) )
  checkException( runMax(input$mid$Close) )
  checkException( runMax(input$all[,1:2]) )
  checkEqualsNumeric( tail(runMax(input$all$Close,250),1), max(input$all$Close) )
}

# Mean
test.runMean <- function() {
  checkEqualsNumeric( runMean(input$all$Close), output$allMean )
  checkEquals( attributes(runMean(input$all$Close)), attributes(output$allMean) )
  checkEqualsNumeric( runMean(input$top$Close), output$topMean )
  checkEquals( attributes(runMean(input$top$Close)), attributes(output$topMean) )
  checkException( runMean(input$mid$Close) )
  checkException( runMean(input$all[,1:2]) )
  checkEqualsNumeric( tail(runMean(input$all$Close,250),1), mean(input$all$Close) )
}

# Median
test.runMedian <- function() {
  checkEqualsNumeric( runMedian(input$all$Close), output$allMedian )
  checkEquals( attributes(runMedian(input$all$Close)), attributes(output$allMedian) )
  checkEqualsNumeric( runMedian(input$top$Close), output$topMedian )
  checkEquals( attributes(runMedian(input$top$Close)), attributes(output$topMedian) )
  checkException( runMedian(input$mid$Close) )
  checkException( runMedian(input$all[,1:2]) )
  checkEqualsNumeric( tail(runMedian(input$all$Close,250),1), median(input$all$Close) )
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
  checkEqualsNumeric( tail(runCov(input$all$High, input$all$Low, 250),1), cov(input$all$High, input$all$Low) )
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
  checkEqualsNumeric( tail(runMAD(input$all$Close,250),1), mad(input$all$Close) )
}
