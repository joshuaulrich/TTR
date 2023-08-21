#
#   TTR: Technical Trading Rules
#
#   Copyright (C) 2007-2013  Joshua M. Ulrich
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 2 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

#' Fetch Internet Data
#'
#' Get investment data from the internet.
#'
#' \code{getYahooData} fetches individual stock data from the Yahoo! Finance
#' website.  It also adjusts price for splits and dividends, and volume for
#' splits.  See the Warning section, and note that it is deprecated in favor
#' of getSymbols in the quantmod package.
#'
#' \code{stockSymbols} fetches instrument symbols from the nasdaq.com website,
#' and adjusts the symbols to be compatible with the Yahoo! Finance website.
#'
#' @aliases WebData getYahooData stockSymbols
#' @param symbol Yahoo! Finance instrument symbol.
#' @param start Numeric; first date of desired data, in YYYYMMDD format.
#' Default is first date of series.
#' @param end Numeric; last date of desired data, in YYYYMMDD format.  Default
#' is last date of series.
#' @param freq Desired data frequency.  One of \code{"daily"}, \code{"weekly"},
#' \code{"monthly"}.
#' @param type Type of data to return.  One of \code{"price"}, or
#' \code{"split"}.  \code{type="split"} will return both split and dividend
#' data.
#' @param adjust Logical; if \code{TRUE}, the Open, High, Low, and Close prices
#' will be adjusted for dividends and splits, and Volume will be adjusted for
#' dividends.
#' @param quiet Logical; if \code{TRUE}, status messages will be printed to the
#' console.
#' @param exchange Character vector of exchange names on which desired
#' instrument symbols are traded.
#' @param sort.by Character vector of columns by which returned data will be
#' sorted.  Must be one or more of \code{"Name"}, \code{"Symbol"},
#' \code{"Market.Cap"}, or \code{"Exchange"}.
#' @return \code{getYahooData} returns an xts object containing the columns:
#'
#' \code{stockSymbols} returns a character vector containing all the listed
#' symbols for the given exchanges.
#'  \describe{
#'     \item{ Date }{ Trade date, in CCYYMMDD format. }
#'     \item{ Open }{ Open price. }
#'     \item{ High }{ High price. }
#'     \item{ Low }{ Low price. }
#'     \item{ Close }{ Close price. }
#'     \item{ Volume }{ Volume. }
#'  }
#' @note The symbols returned by \code{stockSymbols} may not be in the format
#' necessary to retrieve data using \code{getYahooData}.
#'
#' \code{getYahooData} has only been tested on daily data.  It isn't known if
#' the function correctly adjusts data for any other frequency.
#' @author Joshua Ulrich
#' @keywords ts
#' @examples
#'
#'  ### Note: you must have a working internet
#'  ### connection for these examples to work!
#'  if (interactive()) {
#'    ge <- getYahooData("GE", 19990404, 20050607, adjust = FALSE)
#'
#'    nyse.symbols <- stockSymbols("NYSE")
#'  }
#'
#' @section Warning:
#' As of TTR 0.23-2, \code{getYahooData} has been patched to work with changes
#' to Yahoo Finance, which also included the following changes to the raw data:
#'   \itemize{
#'     \item The adjusted close column appears to no longer include dividend adjustments
#'     \item The open, high, and low columns are adjusted for splits, and
#'     \item The raw data may contain missing values.
#'     \item The raw data may contain errors.
#'   }
#'
#' As of TTR 0.24.2, \code{stockSymbols} began using data from NASDAQ's FTP
#' site because the data from the original site is no longer available. This
#' new file does not contain data for the columns: LastSale, MarketCap,
#' IPOyear, Sector, and Industry. All the columns still appear in the results,#' but all the values in the columns are set to \code{NA}.
#'
#' @references
#'
#'  \itemize{
#'    \item \href{https://quant.stackexchange.com/questions/1640/where-to-download-list-of-all-common-stocks-traded-on-nyse-nasdaq-and-amex/1862}{Quant StackExchange: Download list of all stock symbols?}
#'    \item \href{https://www.nasdaqtrader.com/trader.aspx?id=CQSsymbolconvention}{CQS symbol convention}
#'    \item \href{https://web.archive.org/web/20111023221931/http://help.yahoo.com/l/us/yahoo/finance/quotes/quote-02.html}{Yahoo Finance symbol conventions}
#'  }
#'
#' @rdname WebData
"stockSymbols" <-
function(exchange = c("AMEX", "NASDAQ", "NYSE", "ARCA", "BATS", "IEX"),
         sort.by = c("Exchange", "Symbol"),
         quiet = FALSE)
{

  # Many thanks to Ion Georgiadis for helpful suggestions and testing.

  # See "NYSE "behind the dot" or Nasdaq 5th-letter codes and other special
  # codes" here:
  # http://en.wikipedia.org/wiki/Ticker_symbol
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

  symbols.colnames <-
    c("Symbol","Name","LastSale","MarketCap","IPOyear","Sector","Industry",
      "Exchange", "Test.Issue", "Round.Lot.Size", "ETF",
      "Market.Category", "Financial.Status", "Next.Shares", "ACT.Symbol",
      "CQS.Symbol")

  exchange <- match.arg(exchange, several.ok=TRUE)
  sort.by  <- match.arg(sort.by, symbols.colnames, several.ok=TRUE)

  ### nasdaqlisted.txt
  ##nasdaq.colnames <-
  ##  c("Symbol",
  ##    "Security.Name",
  ##    "Market.Category",
  ##    "Test.Issue",
  ##    "Financial.Status",
  ##    "Round.Lot.Size",
  ##    "ETF",
  ##    "NextShares")

  .market.category <-
    c(Q = "NASDAQ Global Select MarketSM",
      G = "NASDAQ Global MarketSM",
      S = "NASDAQ Capital Market")

  ### otherlisted.txt
  ##other.colnames <-
  ##  c("ACT.Symbol",
  ##    "Security.Name",
  ##    "Exchange",
  ##    "CQS.Symbol",
  ##    "ETF",
  ##    "Round.Lot.Size",
  ##    "Test.Issue",
  ##    "NASDAQ.Symbol")

  .exchange <-
    c(A = "AMEX",
      N = "NYSE",
      P = "ARCA",
      Z = "BATS",
      V = "IEX")

  .financial.status <-
    c(D = "Deficient",
      E = "Delinquent",
      Q = "Bankrupt",
      N = "Normal (Default)",
      G = "Deficient and Bankrupt",
      H = "Deficient and Delinquent",
      J = "Delinquent and Bankrupt",
      K = "Deficient, Delinquent, and Bankrupt")

  tmp <- tempfile()

  base.url <- "ftp://ftp.nasdaqtrader.com/SymbolDirectory/"
  nasdaq.url <- paste0(base.url, "nasdaqlisted.txt")
  other.url <- paste0(base.url, "otherlisted.txt")

  nasdaq <- NULL
  if ("NASDAQ" %in% exchange) {
    if (!quiet) {
      message("Fetching NASDAQ symbols...")
      flush.console()
    }
    curl::curl_download(nasdaq.url, destfile = tmp)
    nasdaq <- read.table(tmp, header = TRUE, sep = "|", quote = "",
                         fill = TRUE, na.strings = NULL)

    # add symbols columns not in file
    nasdaq$Name <- nasdaq$Security.Name
    nasdaq$Exchange <- "NASDAQ"
    nasdaq[, setdiff(symbols.colnames, colnames(nasdaq))] <- NA

    # order columns
    nasdaq <- nasdaq[, symbols.colnames]

    # convert market category code to name
    nasdaq$Market.Category <- .market.category[nasdaq$Market.Category]

    # convert financial status code to name
    nasdaq$Financial.Status <- .financial.status[nasdaq$Financial.Status]
  }

  other <- NULL
  if (length(exchange) > 1L) {
    if (!quiet) {
      message("Fetching non-NASDAQ symbols...")
      flush.console()
    }

    curl::curl_download(other.url, destfile = tmp)
    other <- read.table(tmp, header = TRUE, sep = "|", quote = "",
                        fill = TRUE, na.strings = NULL)

    # remove last row (File creation time)
    other <- other[-nrow(other),]

    # add symbols columns not in file
    other$Name <- other$Security.Name
    other$Symbol <- other$NASDAQ.Symbol
    other[, setdiff(symbols.colnames, colnames(other))] <- NA

    # convert exchange code to name
    other$Exchange <- .exchange[other$Exchange]

    # order columns
    other <- other[, symbols.colnames]
  }

  # Append data from all exchanges
  symbols <- rbind(nasdaq, other)

  # Convert symbol from NASDAQ to Yahoo format
  # symbols[grep("[-.*$+!@%^=#].?$", symbols$NASDAQ.Symbol),c("Symbol", "NASDAQ.Symbol")]
  symbols$NASDAQ.Symbol <- symbols$Symbol
  # preferreds
  symbols$Symbol <- sub("-(.?)$", "-P\\1", symbols$Symbol)
  # classes
  symbols$Symbol <- sub("\\.(.?)$", "-\\1", symbols$Symbol)
  # warrants
  symbols$Symbol <- sub("\\+(.?)$", "-WT\\1", symbols$Symbol)
  # units
  symbols$Symbol <- sub("\\=$", "-UN", symbols$Symbol)
  # rights
  symbols$Symbol <- sub("\\^$", "-R", symbols$Symbol)

  # convert ETF and Test.Issue to logical
  symbols$ETF <- ("Y" == symbols$ETF)
  symbols$Test.Issue <- ("Y" == symbols$Test.Issue)

  # Sort
  symbols <- symbols[do.call("order", symbols[,sort.by]),]

  # Pretty rownames
  rownames(symbols) <- NULL

  return(symbols)
}

#-------------------------------------------------------------------------#

#' @rdname WebData
"getYahooData" <-
function(symbol, start, end, freq="daily", type="price", adjust=TRUE, quiet=FALSE) {

  warn.Deprecated <- function() {
    .Deprecated("quantmod::getSymbols", package = "TTR",
                paste("TTR::getYahooData is deprecated and will be removed in a",
                      "future release.\nPlease use quantmod::getSymbols instead."),
                old = "getYahooData")
  }

  callingFun <- sys.call(-1L)[[1]]
  if(is.null(callingFun)) {
    # Called from top level
    warn.Deprecated()
  } else {
    if(is.call(callingFun) && any(deparse(callingFun[[1]]) == c("::", ":::"))) {
       if("getYahooData" != as.character(callingFun[[3]])) warn.Deprecated()
    } else {
       if("getYahooData" != deparse(callingFun)) warn.Deprecated()
    }
  }

  # Thank you to Giorgio Beltrame for the URL to download dividends _and_
  # splits, and for his correct adjustment procedure.
  # Many thanks to Ion Georgiadis for helpful suggestions and testing.

  # symbol:  Character, instrument symbol
  # start:   Numeric, starting date, in ISO-8601 format as ccyymmdd (default
  #          is series' first date)
  # end:     Numeric, ending date, in ISO-8601 format as ccyymmdd (default is today)
  # freq:    Character, frequency of data
  #          either 'daily', 'weekly', 'monthly'
  # type:    Character, either 'price' or 'split'
  # adjust:  Logical, adjusts the Open, High, Low, and Close prices for
  #          dividends and splits, and adjusts Volume for dividends.
  #
  #          http://help.yahoo.com/l/us/yahoo/finance/quotes/quote-12.html
  #          http://ichart.finance.yahoo.com/x?s=MSFT&g=d&y=0&z=30000
  #
  # Requires R-2.4.1

  # Check dates
  if (missing(start)) {
    beg <- .dateToUNIX(as.Date("1900-01-01"))
  } else {
    beg <- .dateToUNIX(as.Date(as.character(start), "%Y%m%d"))
  }
  if (missing(end)) {
    end <- .dateToUNIX(Sys.Date())
  } else {
    end <- .dateToUNIX(as.Date(as.character(end), "%Y%m%d"))
  }

  if( beg > end )                     stop("Start date must be before end date.")
  if (beg > .dateToUNIX(Sys.Date()))  stop("Start date is after today's date.")

  # Get frequency and type parameters
  intervals <- c(daily = "1d", weekly = "1wk", monthly = "1mo")
  freq <- match.arg( freq, names(intervals) )
  interval <- intervals[freq]
  type <- match.arg( type, c("price","split") )
  if(type!="price") {
    if(freq!="daily" & !quiet)
      message("Only freq=\"daily\" data available for type=\"split\".\n",
              "Setting freq=\"daily\"...")
  }

  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)

  flush.console()

  if(type=="price") {

    if(adjust) {

      if(freq=="daily") {

        # Get price, dividend, and split data from 'beg' to present
        ohlc   <- getYahooData(symbol, start, freq="daily", type="price",
                    adjust=FALSE, quiet=TRUE)
        divspl <- getYahooData(symbol, start, freq="daily", type="split",
                    adjust=FALSE, quiet=TRUE)
        ohlc   <- merge(ohlc, divspl, all=TRUE)

        # If there are no div/spl, then ohlc is a zero-width xts object
        if(NROW(divspl) != 0) {

          adj <- adjRatios(ohlc[,'Split'],ohlc[,'Div'],ohlc[,'Close'])
          s.ratio <- adj[,1]
          d.ratio <- adj[,2]

          # Adjust OHLC and volume
          cn <- colnames(ohlc)
          ohlc <- cbind(ohlc,ohlc[,'Close'])
          colnames(ohlc) <- c(cn,'Unadj.Close')
          #ohlc[,'Unadj.Close'] <- ohlc[,'Close']
          ohlc[,'Open']   <- ohlc[,'Open']  * d.ratio * s.ratio
          ohlc[,'High']   <- ohlc[,'High']  * d.ratio * s.ratio
          ohlc[,'Low']    <- ohlc[,'Low']   * d.ratio * s.ratio
          ohlc[,'Close']  <- ohlc[,'Close'] * d.ratio * s.ratio
          ohlc[,'Volume'] <- ohlc[,'Volume'] * ( 1 / d.ratio )

          # Order columns
          #ohlc <- ohlc[,c("Date","Open","High","Low","Close","Volume",
          ohlc <- ohlc[,c("Open","High","Low","Close","Volume",
                          "Unadj.Close","Div","Split","Adj.Div")]
        }

      } else stop("Only freq=\"daily\" adjusted data is currently supported.")

      # For other frequencies, get daily data and use a routine to
      # aggregate to desired frequency.

    } else {

      handle <- .getHandle()

      # Construct URL for 'beg' to 'end'
      url <- .yahooURL(symbol, beg, end, interval, "history", handle)

      # Fetch data
      curl::curl_download(url, destfile=tmp, quiet=quiet, handle=handle$ch)

      # Read data
      ohlc <- read.csv(tmp, na.strings="null")

      # Re-order and set column names
      cnames <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
      corder <- pmatch(substr(cnames, 1, 3), colnames(ohlc))
      ohlc <- ohlc[,corder]
      colnames(ohlc) <- cnames

      ohlc[,'Adjusted'] <- NULL
      ohlc <- ohlc[order(ohlc[,"Date"]),]
      ohlc <- xts(ohlc[,-1], as.Date(as.character(ohlc[,1])))

    }

  } else {

      if(!quiet) message("Unadjusted and adjusted dividend data are always returned.")

      handle <- .getHandle()

      # Split data
      url <- .yahooURL(symbol, beg, end, "1d", "split", handle)
      curl::curl_download(url, destfile=tmp, quiet=quiet, handle=handle$ch)
      spl <- read.csv(tmp, as.is=TRUE)
      if(NROW(spl)==0) {
        spl <- NA
      } else {
        spl$V3 <- 1 / sapply(parse(text=spl[,2]), eval)
        spl <- xts(spl$V3, as.Date(spl[,1], "%Y-%m-%d"))
        colnames(spl) <- NULL
      }

      # Dividend data
      url <- .yahooURL(symbol, beg, end, "1d", "div", handle)
      curl::curl_download(url, destfile=tmp, quiet=quiet, handle=handle$ch)
      div <- read.csv(tmp, as.is=TRUE)
      div <- xts(div[,2],as.Date(div[,1]))
      colnames(div) <- NULL

      ohlc <- merge(Adj.Div = div, Split = spl)

      # Return (empty) data
      if(NROW(ohlc)==0) return(ohlc)

      if( all(is.na(ohlc[,'Split'])) ) {
        s.ratio <- rep(1,NROW(ohlc))
      } else {
        s.ratio <- adjRatios(splits=ohlc[,'Split'])[,1]
      }

      # Un-adjust dividends for Splits
      ohlc <- cbind(ohlc,ohlc[,"Adj.Div"] * ( 1 / s.ratio ))
      colnames(ohlc)[3] <- "Div"
      ohlc[,'Split'] <- as.numeric(ohlc[,'Split'])

      # Order data columns
      ohlc <- ohlc[,c("Div","Split","Adj.Div")]
    }

  # Only return requested data
  dateRange <- paste(as.Date(.POSIXct(beg, tz = "UTC")),
                     as.Date(.POSIXct(end, tz = "UTC")), sep = "/")
  ohlc <- ohlc[dateRange]

  ### Check to see if supplied dates occur in data set
#  if( max(ohlc[,'Date']) != as.Date(end) ) {
#    if(!quiet) message("End date out of range, "  , max(ohlc[,'Date']), " is last available date.")
#  }
#  if( min(ohlc[,'Date']) != as.Date(beg) ) {
#    if(!quiet) message("Start date out of range, ", min(ohlc[,'Date']), " is first available date.")
#  }

  return(ohlc)
}

.getHandle <- function(force.new = FALSE)
{
  h <- if (exists("_handle_", .env)) get("_handle_", .env) else NULL

  if (is.null(h) || force.new) {
    # create 'h' if it doesn't exist yet
    if (!force.new) {
      h <- list()
    }

    # establish session
    new.session <- function(h) {
      tmp <- tempfile()
      on.exit(unlink(tmp))

      for (i in 1:5) {
        h <- curl::new_handle()
        # random query to avoid cache
        ru <- paste(sample(c(letters, 0:9), 4), collapse = "")
        cu <- paste0("https://finance.yahoo.com?", ru)
        curl::curl_download(cu, tmp, handle = h)
        if (NROW(curl::handle_cookies(h)) > 0)
          break;
        Sys.sleep(0.1)
      }

      if (NROW(curl::handle_cookies(h)) == 0)
        stop("Could not establish session after 5 attempts.")

      return(h)
    }

    h$ch <- new.session()

    n <- if (unclass(Sys.time()) %% 1L >= 0.5) 1L else 2L
    query.srv <- paste0("https://query", n, ".finance.yahoo.com/",
                        "v1/test/getcrumb")
    cres <- curl::curl_fetch_memory(query.srv, handle = h$ch)

    h$cb <- rawToChar(cres$content)
    assign("_handle_", h, .env)
  }
  return(h)
}

.yahooURL <-
function(symbol, from, to, period, type, handle)
{
  p <- match.arg(period, c("1d", "1wk", "1mo"))
  e <- match.arg(type, c("history", "div", "split"))
  n <- if (unclass(Sys.time()) %% 1L >= 0.5) 1L else 2L
  u <- paste0("https://query", n, ".finance.yahoo.com/v7/finance/download/",
              symbol, "?period1=", from, "&period2=", to, "&interval=", p,
              "&events=", e, "&crumb=", handle$cb)
  return(u)
}

.dateToUNIX <- function(Date) {
  posixct <- as.POSIXct(as.Date(Date, origin = "1970-01-01"))
  trunc(as.numeric(posixct))
}
