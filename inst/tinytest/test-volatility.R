# Create input data
data(ttrc)
rownames(ttrc) <- ttrc$Date
ttrc$Date <- NULL

input <- list( all=ttrc[1:250,], top=ttrc[1:250,], mid=ttrc[1:250,] )
input$top[1:10,] <- NA
input$mid[9:20,] <- NA

# Load output data
load(system.file("tinytest/output/volatility.rda", package="TTR"))

#################################################

# Close
# Why does the last two checks fail if they're first???
#expect_equal( volatility(input$all[,c('Open','High','Low','Close')],calc='close'), output$allClose )
#expect_equal( volatility(input$top[,c('Open','High','Low','Close')],calc='close'), output$topClose )
expect_error( volatility(input$mid[,c('Open','High','Low','Close')],calc='close') )


# Garman Klass
ohlc <- c('Open','High','Low','Close')
expect_equal( volatility(input$all[,ohlc],calc='garman.klass')[["x"]], output$allGK[["x"]] )
expect_equal( volatility(input$top[,ohlc],calc='garman.klass')[["x"]], output$topGK[["x"]] )
expect_error( volatility(input$mid[,ohlc],calc='garman.klass') )


# Parkinson
ohlc <- c('Open','High','Low','Close')
expect_equal( volatility(input$all[,ohlc],calc='parkinson')[["x"]], output$allParkinson[["x"]] )
expect_equal( volatility(input$top[,ohlc],calc='parkinson')[["x"]], output$topParkinson[["x"]] )
expect_error( volatility(input$mid[,ohlc],calc='parkinson') )


# Rogers Satchell
ohlc <- c('Open','High','Low','Close')
expect_equal( volatility(input$all[,ohlc],calc='rogers.satchell')[["x"]], output$allRS[["x"]] )
expect_equal( volatility(input$top[,ohlc],calc='rogers.satchell')[["x"]], output$topRS[["x"]] )
expect_error( volatility(input$mid[,ohlc],calc='rogers.satchell') )


# Chaikin Volatility
ia <- as.matrix(input$all)
rownames(ia) <- NULL
expect_equal( chaikinVolatility(ia[,c('High','Low')]), output$allChaikin )
#expect_equal( chaikinVolatility(input$top[,c('Open','High','Low','Close')],calc='rogers.satchell'), output$topRS )
#expect_error( volatility(input$mid[,c('Open','High','Low','Close')],calc='rogers.satchell') )
