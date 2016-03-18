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
load(system.file("unitTests/output.overlays.rda", package="TTR"))

#################################################

# Bollinger Bands
test.BBands <- function() {
  ia <- input$all[,c('High','Low','Close')]
  it <- input$top[,c('High','Low','Close')]
  im <- input$mid[,c('High','Low')]
  rownames(ia) <- rownames(it) <- NULL
  oa <- BBands(ia)
  ot <- BBands(it)
  rownames(oa) <- rownames(ot) <- rownames(input$all)
  checkEqualsNumeric( oa, output$allBBands )
  checkEquals( attributes(oa), attributes(output$allBBands) )
  checkEqualsNumeric( ot, output$topBBands )
  checkEquals( attributes(ot), attributes(output$topBBands) )
  checkException( BBands(im) )
}

# SAR
test.SAR <- function() {
  ia <- input$all[,c('High','Low')]
  it <- input$top[,c('High','Low')]
  im <- input$mid[,c('High','Low')]
  rownames(ia) <- rownames(it) <- rownames(im) <- NULL
  checkEqualsNumeric( SAR(ia), output$allSAR )
  checkEquals( attributes(SAR(ia)), attributes(output$allSAR) )
  checkEqualsNumeric( SAR(it), output$topSAR )
  checkEquals( attributes(SAR(it)), attributes(output$topSAR) )
  checkException( SAR(im) )
}

# Zig Zag
test.ZigZag <- function() {
  ia <- input$all[,c('High','Low')]
  it <- input$top[,c('High','Low')]
  im <- input$mid[,c('High','Low')]
  rownames(ia) <- rownames(it) <- rownames(im) <- NULL
  checkEqualsNumeric( ZigZag(ia), output$allZZ )
  checkEquals( attributes(ZigZag(ia)), attributes(output$allZZ) )
  checkEqualsNumeric( ZigZag(it), output$topZZ )
  checkEquals( attributes(ZigZag(it)), attributes(output$topZZ) )
  checkException( ZigZag(im) )
}

