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
load('unitTests/output.overlays.rda')

#################################################

# Bollinger Bands
test.BBands <- function() {
  checkEqualsNumeric( BBands(input$all[,c('High','Low','Close')]), output$allBBands )
  checkEqualsNumeric( BBands(input$top[,c('High','Low','Close')]), output$topBBands )
  checkException( BBands(input$mid[,c('High','Low','Close')]) )
}

# SAR
test.SAR <- function() {
  checkEqualsNumeric( SAR(input$all[,c('High','Low')]), output$allSAR )
  checkEqualsNumeric( SAR(input$top[,c('High','Low')]), output$topSAR )
  checkException( SAR(input$mid[,c('High','Low')]) )
}

# Zig Zag
test.ZigZag <- function() {
  checkEqualsNumeric( ZigZag(input$all[,c('High','Low')]), output$allZZ )
  checkEqualsNumeric( ZigZag(input$top[,c('High','Low')]), output$topZZ )
  checkException( ZigZag(input$mid[,c('High','Low')]) )
}

