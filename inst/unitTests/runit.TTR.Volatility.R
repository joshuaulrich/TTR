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
load(system.file("unitTests/output.volatility.rda", package="TTR"))

#################################################

# Close
test.Close <- function() {
  # Why does the last two checks fail if they're first???
  #checkEqualsNumeric( volatility(input$all[,c('Open','High','Low','Close')],calc='close'), output$allClose )
  #checkEqualsNumeric( volatility(input$top[,c('Open','High','Low','Close')],calc='close'), output$topClose )
  checkException( volatility(input$mid[,c('Open','High','Low','Close')],calc='close') )
}

# Garman Klass 
test.garman.klass <- function() {
  checkEqualsNumeric( volatility(input$all[,c('Open','High','Low','Close')],calc='garman.klass'), output$allGK )
  checkEqualsNumeric( volatility(input$top[,c('Open','High','Low','Close')],calc='garman.klass'), output$topGK )
  checkException( volatility(input$mid[,c('Open','High','Low','Close')],calc='garman.klass') )
}

# Parkinson
test.parkinson <- function() {
  checkEqualsNumeric( volatility(input$all[,c('Open','High','Low','Close')],calc='parkinson'), output$allParkinson )
  checkEqualsNumeric( volatility(input$top[,c('Open','High','Low','Close')],calc='parkinson'), output$topParkinson )
  checkException( volatility(input$mid[,c('Open','High','Low','Close')],calc='parkinson') )
}

# Rogers Satchell
test.rogers.satchell <- function() {
  checkEqualsNumeric( volatility(input$all[,c('Open','High','Low','Close')],calc='rogers.satchell'), output$allRS )
  checkEqualsNumeric( volatility(input$top[,c('Open','High','Low','Close')],calc='rogers.satchell'), output$topRS )
  checkException( volatility(input$mid[,c('Open','High','Low','Close')],calc='rogers.satchell') )
}

# Chaikin Volatility
test.chaikin <- function() {
  ia <- as.matrix(input$all)
  rownames(ia) <- NULL
  checkEqualsNumeric( chaikinVolatility(ia[,c('High','Low')]), output$allChaikin )
  #checkEqualsNumeric( chaikinVolatility(input$top[,c('Open','High','Low','Close')],calc='rogers.satchell'), output$topRS )
  #checkException( volatility(input$mid[,c('Open','High','Low','Close')],calc='rogers.satchell') )
}
