# Create input data
data(ttrc)
rownames(ttrc) <- ttrc$Date
ttrc$Date <- NULL

input <- list( all=ttrc[1:250,], top=ttrc[1:250,], mid=ttrc[1:250,] )
input$top[1:10,] <- NA
input$mid[9:20,] <- NA

# Load output data
load(system.file("tinytest/output/overlays.rda", package="TTR"))

#################################################

# Bollinger Bands
ia <- input$all[,c('High','Low','Close')]
it <- input$top[,c('High','Low','Close')]
im <- input$mid[,c('High','Low')]
rownames(ia) <- rownames(it) <- NULL
oa <- BBands(ia)
ot <- BBands(it)
rownames(oa) <- rownames(ot) <- rownames(input$all)
expect_equal( oa, output$allBBands )
expect_equal( attributes(oa), attributes(output$allBBands) )
expect_equal( ot, output$topBBands )
expect_equal( attributes(ot), attributes(output$topBBands) )
expect_error( BBands(im) )

# SAR
ia <- input$all[,c('High','Low')]
it <- input$top[,c('High','Low')]
im <- input$mid[,c('High','Low')]
rownames(ia) <- rownames(it) <- rownames(im) <- NULL
expect_equal( SAR(ia), output$allSAR )
expect_equal( attributes(SAR(ia)), attributes(output$allSAR) )
expect_equal( SAR(it), output$topSAR )
expect_equal( attributes(SAR(it)), attributes(output$topSAR) )
expect_error( SAR(im) )

# Zig Zag
ia <- input$all[,c('High','Low')]
it <- input$top[,c('High','Low')]
im <- input$mid[,c('High','Low')]
rownames(ia) <- rownames(it) <- rownames(im) <- NULL
expect_equal( ZigZag(ia), output$allZZ )
expect_equal( attributes(ZigZag(ia)), attributes(output$allZZ) )
expect_equal( ZigZag(it), output$topZZ )
expect_equal( attributes(ZigZag(it)), attributes(output$topZZ) )
expect_error( ZigZag(im) )
