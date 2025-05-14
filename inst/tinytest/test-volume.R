# Create input data
data(ttrc)
rownames(ttrc) <- ttrc$Date
ttrc$Date <- NULL

#input <- list( all=ttrc[1:250,], top=ttrc[1:250,], mid=ttrc[1:250,] )
#input$top[1:10,] <- NA
#input$mid[9:20,] <- NA

#iAll <- as.matrix(ttrc[1:250,])
iAll <- ttrc[1:250,]
iTop <- iAll; iTop[1:10,] <- NA
iMid <- iAll; iMid[9:20,] <- NA

hl  <- c('High','Low')
hlc <- c('High','Low','Close')
cl  <- 'Close'

# Load output data
load(system.file("tinytest/output/volume.rda", package="TTR"))

#################################################

# On Balance Volume
expect_equal( OBV(iAll$Close, iAll$Volume), output$allOBV )
#expect_equal( OBV(iTop[,cl], iTop[,'Volume']), output$topOBV )
#expect_error( OBV(iMid[,cl], iMid[,'Volume']) )
#expect_error( OBV(iAll[,cl], iMid[,'Volume']) )
#expect_error( OBV(iMid[,cl], iAll[,'Volume']) )

# Chaikin Accumulation / Distribution
chAD <- chaikinAD(iAll[,hlc], iAll[,'Volume'])
expect_equivalent( chaikinAD(iAll[,hlc], iAll[,'Volume']), output$allChaikinAD )
#expect_equal( chaikinAD(iTop[,hlc], iTop[,'Volume']), output$topChaikinAD )
#expect_error( chaikinAD(iMid[,hlc], iMid[,'Volume']) )
#expect_error( chaikinAD(iAll[,hlc], iMid[,'Volume']) )
#expect_error( chaikinAD(iMid[,hlc], iAll[,'Volume']) )

# Chaikin Money Flow
ia <- iAll[,hlc];  rownames(ia) <- NULL
it <- iTop[,hlc];  rownames(it) <- NULL
expect_equal( CMF(ia, iAll[,'Volume']), output$allCMF )
expect_equal( CMF(it, iTop[,'Volume']), output$topCMF )
expect_error( CMF(iMid[,hlc], iMid[,'Volume']) )
expect_error( CMF(iAll[,hlc], iMid[,'Volume']) )
expect_error( CMF(iMid[,hlc], iAll[,'Volume']) )

# Money Flow Index
ia <- iAll[,hlc];  rownames(ia) <- NULL
it <- iTop[,hlc];  rownames(it) <- NULL
expect_equal( MFI(ia, iAll[,'Volume']), output$allMFI )
expect_equal( MFI(it, iTop[,'Volume']), output$topMFI )
expect_error( MFI(iMid[,hlc], iMid[,'Volume']) )
expect_error( MFI(iAll[,hlc], iMid[,'Volume']) )
expect_error( MFI(iMid[,hlc], iAll[,'Volume']) )

x <- structure(c(6284.19, 6284.19, 6284.19, 6284.19, 6284.19, 6285.22,
6285.96, 6287.54, 6287.84, 6287.89, 6288.95, 6284.19, 6284.19, 6284.19,
6284.19, 6284.19, 6283.98, 6284.20, 6285.54, 6286.71, 6286.58, 6286.75,
6284.19, 6284.19, 6284.19, 6284.19, 6284.19, 6284.46, 6285.54, 6287.47,
6286.92, 6286.82, 6288.95, 9171293400, 9171293400, 9171293400, 9171293400,
9171293400, 1650189487, 1796244384, 1864666606, 1845475611, 1831082797,
1918533018), .Dim = c(11L, 4L))
o <- c(NA, NA, NA, 50, 50, 100, 100, 100, 100, 66.95494, 67.27551)
m <- MFI(x[,-4], x[,4], n = 3)
expect_equal(m, o, info = "MFI when volume is unchanged")

# Williams' Accumulation / Distribution
# non-xts
ia <- iAll[,hlc]
it <- iTop[,hlc]
im <- iMid[,hlc]
rownames(ia) <- rownames(it) <- rownames(im) <- NULL
expect_equal( williamsAD(ia), output$allWilliamsAD )
#expect_equal( williamsAD(iTop[,hlc]), output$topWilliamsAD )
#expect_error( williamsAD(iMid[,hlc]) )
