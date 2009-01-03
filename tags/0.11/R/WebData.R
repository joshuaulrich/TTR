"get.symbols" <-
function(exchange=c("AMEX","NASDAQ","NYSE")) {

  y <- NULL

  for(i in exchange) {
    if(i=="NASDAQ") url <- "http://www.nasdaq.com/asp/symbols.asp?exchange=Q&start=0" else
    if(i=="AMEX")   url <- "http://www.nasdaq.com/asp/symbols.asp?exchange=1&start=0" else
    if(i=="NYSE")   url <- "http://www.nasdaq.com/asp/symbols.asp?exchange=N&start=0" else
    stop("Please choose exchange of: NASDAQ, AMEX, NYSE")

    nams <- c( scan(url, "", sep=",", skip=1, nlines=1, quiet=TRUE)[1:3], "Exchange" )
    x    <- do.call( "cbind" , scan(url, list("","","",NULL), sep=",", skip=2, quiet=TRUE) )
    x    <- cbind( x[-nrow(x),], rep(i,nrow(x)-1) )
    y    <- rbind( y, x )
    nams -> colnames(y)
  }
  return( y )
}
######################################################################################
#   TO DO:
######################################################################################
# There are many AMEX & NYSE tickers that probably should not be returned (preferred
# shares, warrant, etc.).  Maybe include an option so they will be returned?
# I don't anticipate using them myself, and I don't think many will be interested in
# them.
#
# See "NYSE "behind the dot" or Nasdaq 5th-letter codes and other special codes" here:
# http://en.wikipedia.org/wiki/Ticker_symbol
#
# I think I need to drop everything matching grep("(\\.|/|\\^|\\:)") EXCEPT _some_
# of the tickers with an A or B code.  Which shouldn't be dropped?  I'm not sure.
# I think it's safe to remove all the tickers that do not have any market value.
#
# Also, Yahoo no longer tacks the class at the end of the ticker; now they have a
# "-" seperating the ticker and the class (A or B).
#
######################################################################################



"yahoo.data" <-
function(symbol, start, end, freq="daily", adjust=c("price","volume"), file=NULL) {

  # _Documentation_
  # symbol:  Character, instrument symbol
  # start:   Numeric, starting date, in ISO-8601 format as ccyymmdd
  # end:     Numeric, ending date, in ISO-8601 format as ccyymmdd (default is yesterday)
  # freq:    Character, frequency of data
  #          either 'daily', 'weekly', 'monthly', or 'dividend' 
  # file:    Either NULL (default) or a location to save output to specified directory
  #          as a comma-delimited text file.
  # adjust:  Logical, adjusts the Open, High, Low, and Close prices for dividends and split,
  #          and adjusts Volume for dividends and split.
  #
  #          http://help.yahoo.com/l/us/yahoo/finance/quotes/quote-12.html

  # Check dates:
  if (missing(start)) start <- as.POSIXlt(Sys.Date()-365) else start <- as.POSIXlt( as.Date( as.character(start), "%Y%m%d" ) )
  if (missing(end))   end   <- as.POSIXlt(Sys.Date()-  1) else end   <- as.POSIXlt( as.Date( as.character( end ), "%Y%m%d" ) )

  if( start >= end ) stop("Start date >= end date.")
  if( start >= as.POSIXlt(Sys.Date()) ) stop("Start date is >= today's date.")
  flush.console()

  # Set freqeucy parameter
  if( substr(freq,1,2) == "di" )
    freq <- "v"
  else
    freq <- substr(freq,1,1)

  # Set up URL
  url <- paste( "http://ichart.yahoo.com/table.csv?s=" , symbol ,
                "&a=" , start$mon , "&b=" , start$mday , "&c=", start$year+1900 ,
                "&d=" , end$mon   , "&e=" , end$mday   , "&f=", end$year+1900   ,
                "&g=" , freq      , "&ignore=.csv" , sep="" )

  ohlc <- read.table(url, header=TRUE, sep=",")
  ohlc$Date <- as.Date(as.character(ohlc$Date), "%d-%b-%y")
  d.ratio <- ohlc[,7] / ohlc[,5]
  ohlc <- ohlc[,-7]

  ### I use the Adj_Close field in the Yahoo data to adjust the data, to aviod the problem of needing all the dividend and split
  ### data to adjust a portion of the historic data series.  E.g. if a sample of data from the 1980's was desired, and there are
  ### splits and dividends that occur after the end of the sample, those data would be needed to adjust the sample.
  ### It's more efficient to simply use the Adj_Close data that's already provided to adjust the prices, and use all the split
  ### data to make the ratios for each time frame to adjust the volume.

  ### This adjustment algorithm was tested on daily data, does it work on weekly / monthly / yearly data also?
  if( any(adjust=="price") & freq != "v") {

    ### Adjust price for dividends and splits
    ohlc[,2:5] <- round( ohlc[,2:5] * d.ratio, 3 )
  }

  if( any(adjust=="volume") & freq != "v") {
    ### Adjust volume data for splits
    # Get split data
    splt <- scan( paste("http://finance.yahoo.com/q/ta?s=",symbol,sep=""), what="", sep=">", quote="", quiet=TRUE)
    splt <- gsub("</nobr","",splt[ grep("/nobr",splt) ])
    splt <- data.frame ( Date   = as.Date( substr(splt,1,9), "%d-%b-%y" ),
                         Splits = as.numeric( substr(splt,12,12) ) / as.numeric( substr(splt,14,14) ) )

    # Make split ratios & line up with ohlc data
    for(i in nrow(splt):1) if(i==nrow(splt)) splt$s.ratio[i] <- 1/splt$Splits[i] else splt$s.ratio[i] <- splt$s.ratio[i+1]/splt$Splits[i]
    splt.locs <- match( ohlc$Date, splt$Date )
    if( length( na.omit(splt.locs) )==0 ) s.ratio <- rep(1,nrow(ohlc)) else {
       s.ratio <- NULL
       for(i in 1:nrow(ohlc)) {
          if(i==1) s.ratio[i] <- ifelse( max(splt.locs,na.rm=TRUE) == nrow(splt), 1, splt$s.ratio[max(splt.locs,na.rm=TRUE)+1] )
          else {
             if(is.na(splt.locs[i-1])) s.ratio[i] <- s.ratio[i-1] else s.ratio[i] <- splt$s.ratio[splt.locs[i-1]]
          }
       }
    }
    # Adjust volume for splits
    ohlc[,6] <- ohlc[,6] * s.ratio
  }

  ### Adjust & Order all Dates
  #splt$Date[ splt$Date > Sys.Date() ] = splt$Date[ splt$Date > Sys.Date() ]-100*365.25
  #splt$Date[ as.POSIXlt(splt$Date)$year > as.POSIXlt(Sys.Date())$year ] <- splt$Date[ as.POSIXlt(splt$Date)$year > as.POSIXlt(Sys.Date())$year ]-100*365.25
  ohlc$Date[ as.POSIXlt(ohlc$Date)$year > as.POSIXlt(Sys.Date())$year ] <- ohlc$Date[ as.POSIXlt(ohlc$Date)$year > as.POSIXlt(Sys.Date())$year ]-100*365.25
  ohlc <- ohlc[order(ohlc$Date),]
  row.names(ohlc) <- rev(row.names(ohlc))

  ### Check to see if supplied dates occur in data set
  if( max(ohlc$Date) != as.Date(end)   ) message("End date out of range, "  , max(ohlc$Date), " is last available date.")
  if( min(ohlc$Date) != as.Date(start) ) message("Start date out of range, ", min(ohlc$Date), " is first available date.")

  ### Need something to account for fields, if I want them... ###
  #fields <- c("Open","High","Low","Close","Volume","AdjClose")

  # Save data to file, or return as object
  ### Need something to ensue the file is writeable??? ###
  if( !is.null(file) ) write.table( ohlc, file.path(file,paste(symbol,".txt",sep="")), row.names=FALSE, col.names=TRUE, sep="," ) else return(ohlc)
}
