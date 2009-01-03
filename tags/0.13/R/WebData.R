"stockSymbols" <-
function(exchange=c("AMEX","NASDAQ","NYSE"), sort.by=c("Exchange","Symbol"), quiet=FALSE) {

  # Many thanks to Ion Georgiadis for helpful suggestions and testing.

  # See "NYSE "behind the dot" or Nasdaq 5th-letter codes and other special codes" here:
  # http://en.wikipedia.org/wiki/Ticker_symbol
  # http://help.yahoo.com/l/us/yahoo/finance/quotes/quote-02.html
  # 
  # AMEX / NYSE Mappings (NASDAQ doesn't need transformation?):
  # Exchanges -> Yahoo
  # /WS       -> -WT
  # /U        -> -U
  # .[A-Z]    -> NA (special notes/bonds - IG)
  # :[AP]     -> NA (after-hours / pre-market)
  # ^         -> -P
  # /         -> -
  # $         -> NA (NYSE Only)
  # ~         -> NA (NYSE Only)

  symbols  <- NULL
  symbols.colnames <- c("Name","Symbol","MarketCap","Exchange")
  exchange <- match.arg(exchange, several.ok=TRUE)
  sort.by  <- match.arg(sort.by, symbols.colnames, several.ok=TRUE)

  for(i in exchange) {
    if(!quiet) message("Fetching ",i," symbols...")
    flush.console()

    # Fetch Symbols
    if( i=="NASDAQ") {
      url  <- "http://www.nasdaq.com/asp/symbols.asp?exchange=Q&start=0"
      exch <- read.csv(url, skip=2, header=FALSE, as.is=TRUE,
                colClasses=c("character","character","NULL","NULL","character","NULL"))
    } else
    if( i=="AMEX") {
      url  <- "http://www.nasdaq.com/asp/symbols.asp?exchange=1&start=0"
      exch <- read.csv(url, skip=2, header=FALSE, as.is=TRUE,
                colClasses=c("character","character","character","NULL"))

      # Transform Symbols to Yahoo format
      exch[,2] <- gsub("/WS$", "-WT", exch[,2])  # AMEX, NYSE
      exch[,2] <- gsub("/WS/", "-WT", exch[,2])  # AMEX, NYSE
      exch[,2] <- gsub("/U",   "-U",  exch[,2])  # AMEX
      exch[,2] <- gsub("\\^",  "-P",  exch[,2])  # AMEX, NYSE
      exch[,2] <- gsub("/",    "-",   exch[,2])  # AMEX, NYSE

      # Drop symbols Yahoo doesn't provide
      drop <- c( grep("\\.", exch[,2]),   # AMEX
                 grep("\\$", exch[,2]),   # AMEX, NYSE
                 grep(":",   exch[,2]) )  # AMEX, NYSE
      if(NROW(drop)!=0) {
        exch <- exch[-drop,]
      }

    } else
    if( i=="NYSE") {
      url  <- "http://www.nasdaq.com/asp/symbols.asp?exchange=N&start=0"
      exch <- read.csv(url, skip=2, header=FALSE, as.is=TRUE,
                colClasses=c("character","character","character","NULL"))

      # Transform Symbols to Yahoo format
      exch[,2] <- gsub("/WS$", "-WT", exch[,2])  # AMEX, NYSE
      exch[,2] <- gsub("/WS/", "-WT", exch[,2])  # AMEX, NYSE
      exch[,2] <- gsub("\\^",  "-P",  exch[,2])  # AMEX, NYSE
      exch[,2] <- gsub("/",    "-",   exch[,2])  # AMEX, NYSE

      # Drop symbols Yahoo doesn't provide
      drop <- c( grep("\\$", exch[,2]),   # AMEX
                 grep(":",   exch[,2]),   # AMEX, NYSE
                 grep("~",   exch[,2]) )  # AMEX, NYSE
      if(NROW(drop)!=0) {
        exch <- exch[-drop,]
      }

    }

    # Remove last line, create "Exchange" column, and set column names
    exch    <- cbind( exch[-NROW(exch),], rep(i,NROW(exch)-1), stringsAsFactors=FALSE )
    colnames(exch) <- symbols.colnames
    symbols <- rbind( symbols, exch )
  }

  # Sort
  symbols <- symbols[do.call("order", symbols[,sort.by]),]

  return(symbols)
}

"getYahooData" <-
function(symbol, start, end, freq="daily", type="price", adjust=TRUE, quiet=FALSE) {

  # Thank you to Giorgio Beltrame for the URL to download dividends _and_ splits, and
  # for his correct adjustment procedure.
  # Many thanks to Ion Georgiadis for helpful suggestions and testing.

  # symbol:  Character, instrument symbol
  # start:   Numeric, starting date, in ISO-8601 format as ccyymmdd (default
  #          is series' first date)
  # end:     Numeric, ending date, in ISO-8601 format as ccyymmdd (default is today)
  # freq:    Character, frequency of data
  #          either 'daily', 'weekly', 'monthly'
  # type:    Character, either 'price' or 'split'
  # adjust:  Logical, adjusts the Open, High, Low, and Close prices for dividends and splits,
  #          and adjusts Volume for dividends.
  #
  #          http://help.yahoo.com/l/us/yahoo/finance/quotes/quote-12.html
  #          http://ichart.finance.yahoo.com/x?s=MSFT&g=d&y=0&z=30000
  #
  # Requires R-2.4.1

  # Check dates
  if (missing(start)) {
    beg <- as.POSIXlt( "1900-01-01" )
  } else {
    beg <- as.POSIXlt( as.Date( as.character(start), "%Y%m%d" ) )
  }
  if (missing(end)) {
    end <- as.POSIXlt(Sys.Date())
  } else {
    end <- as.POSIXlt( as.Date( as.character( end ), "%Y%m%d" ) )
  }

  if( beg > end )                    stop("Start date must be before end date.")
  if( beg > as.POSIXlt(Sys.Date()) ) stop("Start date is after today's date.")

  # Get freqeucy and type parameters
  freq <- match.arg( freq, c("daily","weekly","monthly") )
  type <- match.arg( type, c("price","split") )
  if(type=="price") {
    freq.url <- substr(freq,1,1)
  } else {
    freq.url <- "v"
    if(freq!="daily" & !quiet) message("Only freq=\"daily\" data available for type=\"split\".\n",
                                       "Setting freq=\"daily\"...")
  }

  flush.console()

  if(type=="price") {

    if(adjust) {

      if(freq=="daily") {

        # Get price, dividend, and split data from 'beg' to present
        ohlc   <- getYahooData(symbol, start, freq="daily", type="price", adjust=FALSE, quiet=TRUE)
        divspl <- getYahooData(symbol, start, freq="daily", type="split", adjust=FALSE, quiet=TRUE)
        ohlc   <- merge(ohlc, divspl, by.col="Date", all=TRUE)

        # Create split adjustment ratio, (always = 1 if no splits exist)
        s.ratio <- rep(1, NROW(ohlc))
        if( !all( is.na(ohlc$Split) ) ) {
          # Start loop at most recent data
          for( i in NROW(ohlc):2 ) {
            if( is.na( ohlc$Split[i] ) ) {
              s.ratio[i-1] <- s.ratio[i]
            } else {
              s.ratio[i-1] <- s.ratio[i] * ohlc$Split[i]
            } 
          }
        }

        # Un-adjust dividends for Splits
        ohlc$Div <- ohlc$Adj.Div * ( 1 / s.ratio )

        # Create dividend adjustment ratio, (always = 1 if no dividends exist)
        d.ratio <- rep(1, NROW(ohlc))
        if( !all( is.na(ohlc$Adj.Div) ) ) {
          # Start loop at most recent data
          for( i in NROW(ohlc):2 ) {
            if( is.na( ohlc$Adj.Div[i] ) ) {
              d.ratio[i-1] <- d.ratio[i]
            } else {
              d.ratio[i-1] <- d.ratio[i] * ( 1 - ohlc$Div[i] / ohlc$Close[i-1] ) 
            } 
          }
        }

        # Adjust OHLC and volume
        ohlc$Unadj.Close <- ohlc$Close
        ohlc$Open   <- ohlc$Open  * d.ratio * s.ratio
        ohlc$High   <- ohlc$High  * d.ratio * s.ratio
        ohlc$Low    <- ohlc$Low   * d.ratio * s.ratio
        ohlc$Close  <- ohlc$Close * d.ratio * s.ratio
        ohlc$Volume <- ohlc$Volume * ( 1 / d.ratio )

        # Order columns
        ohlc <- ohlc[,c("Date","Open","High","Low","Close","Volume","Unadj.Close","Div","Split","Adj.Div")]

      } else stop("Only freq=\"daily\" adjusted data is currently supported.")
      # For other frequencies, get daily data and use a routine to aggregate to desired frequency.

    } else {

    # Construct URL for 'beg' to 'end'
    url <- paste( "http://ichart.finance.yahoo.com/table.csv?s=", symbol,
                  "&a=", beg$mon, "&b=", beg$mday, "&c=", beg$year+1900,
                  "&d=", end$mon, "&e=", end$mday, "&f=", end$year+1900,
                  "&g=", freq.url, "&ignore=.csv", sep="" )

    # Fetch data
    ohlc <- read.table(url, header=TRUE, sep=",")
    ohlc$Date <- as.Date(as.character(ohlc$Date), "%Y-%m-%d")
    ohlc$Adj.Close <- NULL

    }

  } else {

      if(!quiet) message("Unadjusted and adjusted dividend data are always returned.")

      # Construct URL for 'beg' to 'end'
      url <- paste( "http://ichart.finance.yahoo.com/x?s=", symbol,
                    "&a=", beg$mon, "&b=", beg$mday, "&c=", beg$year+1900,
                    "&d=", end$mon, "&e=", end$mday, "&f=", end$year+1900,
                    "&g=", freq.url, "&y=0&z=30000", sep="" )

      # Fetch data
      ohlc <- read.table(url, skip=1, sep=",", fill=TRUE, as.is=TRUE)
      div  <- data.frame( Date=   ohlc$V2[ohlc$V1=="DIVIDEND"],
                          Adj.Div=as.numeric(ohlc$V3[ohlc$V1=="DIVIDEND"]),
                          stringsAsFactors=FALSE )
      spl  <- data.frame( Date=   ohlc$V2[ohlc$V1=="SPLIT"],
                          Split=as.character(ohlc$V3[ohlc$V1=="SPLIT"]),
                          stringsAsFactors=FALSE )

      ohlc <- merge(div, spl, by.col="Date", all=TRUE)
      ohlc$Date <- as.Date(as.character(ohlc$Date), "%Y%m%d")

      # Create split adjustment ratio, (always = 1 if no splits exist)
      s.ratio <- rep(1, NROW(ohlc))
      if(NROW(spl)!=0) {
        ohlc$Split <- sub(":","/", ohlc$Split)
        ohlc$Split <- 1 / sapply( parse( text=ohlc$Split ), eval )
        # Start loop at most recent data
        for( i in NROW(ohlc):2 ) {
          if( is.na( ohlc$Split[i] ) ) {
            s.ratio[i-1] <- s.ratio[i]
          } else {
            s.ratio[i-1] <- s.ratio[i] * ohlc$Split[i]
          } 
        }
      }

      # Un-adjust dividends for Splits
      ohlc$Div <- ohlc$Adj.Div * ( 1 / s.ratio )
      ohlc$Split <- as.numeric(ohlc$Split)

      # Order data columns
      ohlc <- ohlc[,c("Date","Div","Split","Adj.Div")]

      # Return (empty) data
      if(NROW(ohlc)==0) return(ohlc)
    }

  # Order Dates, and only return requested data (drop 'end' to present)
  ohlc <- ohlc[order(ohlc$Date),]
  row.names(ohlc) <- 1:NROW(ohlc)
  ohlc <- ohlc[ ( ohlc$Date >= as.Date(beg) & ohlc$Date <= as.Date(end) ), ]

  ### Check to see if supplied dates occur in data set
  if( max(ohlc$Date) != as.Date(end) ) {
    if(!quiet) message("End date out of range, "  , max(ohlc$Date), " is last available date.")
  }
  if( min(ohlc$Date) != as.Date(beg) ) {
    if(!quiet) message("Start date out of range, ", min(ohlc$Date), " is first available date.")
  }

  return(ohlc)
}
