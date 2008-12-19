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
  ia <- input$all[,c('High','Low')]
  it <- input$top[,c('High','Low')]
  im <- input$mid[,c('High','Low')]
  rownames(ia) <- rownames(it) <- rownames(im) <- NULL
  checkEqualsNumeric( SAR(ia), output$allSAR )
  checkEqualsNumeric( SAR(it), output$topSAR )
  checkException( SAR(im) )
}

# Zig Zag
test.ZigZag <- function() {
  ia <- input$all[,c('High','Low')]
  it <- input$top[,c('High','Low')]
  im <- input$mid[,c('High','Low')]
  rownames(ia) <- rownames(it) <- rownames(im) <- NULL
  checkEqualsNumeric( ZigZag(ia), output$allZZ )
  checkEqualsNumeric( ZigZag(it), output$topZZ )
  checkException( ZigZag(im) )
}

