\name{WebData}
\alias{WebData}
\alias{yahoo.data}
\alias{get.symbols}
\title{ Fetch Internet Data }
\description{
  Get investment data from the internet.
}
\usage{
  yahoo.data(symbol, start, end, freq = "daily", type = "price",
             adjust = TRUE, quiet = FALSE)
  get.symbols(exchange = c("AMEX","NASDAQ","NYSE"),
              sort.by = c("Exchange","Symbol"), quiet = FALSE)
}
\arguments{
  \item{symbol}{ Yahoo! Finance instrument symbol. }
  \item{start}{ Numeric; first date of desired data, in YYYYMMDD format. 
    Default is first date of series. }
  \item{end}{ Numeric; last date of desired data, in YYYYMMDD format. 
    Default is last date of series. }
  \item{freq}{ Desired data frequency.  One of \dQuote{daily}, \dQuote{weekly}, 
    \dQuote{monthly}. }
  \item{type}{ Type of data to return.  One of \dQuote{price}, or \dQuote{split}.
    \code{type = "split"} will return both split and dividend data. }
  \item{adjust}{ Logical; if \code{TRUE}, the Open, High, Low, and Close prices will
    be adjusted for dividends and splits, and Volume will be adjusted for dividends. }
  \item{quiet}{ Logical; if \code{TRUE}, status messages will be printed to the console. }
  \item{exchange}{ Character vector of exchange names on which desired instrument symbols are traded. }
  \item{sort.by}{ Character vector of columns by which returned data will be sorted.  Must be
    one or more of \dQuote{Name}, \dQuote{Symbol}, \dQuote{Market.Cap}, or \dQuote{Exchange}. }
}
\details{
  \code{yahoo.data} fetches individual stock data from the Yahoo! Finance website.
  It also adjusts price for splits and dividends, and volume for splits.

  \code{get.symbols} fetches instrument symbols from the nasdaq.com website, and
  adjusts the symbols to be compatible with the Yahoo! Finance website.
}
\value{
  \code{yahoo.data} returns a numeric matrix containing the columns:
  \item{ Date }{ Trade date, in CCYYMMDD format. }
  \item{ Open }{ Open price. }
  \item{ High }{ High price. }
  \item{ Low }{ Low price. }
  \item{ Close }{ Close price. }
  \item{ Volume }{ Volume. }

  \code{get.symbols} returns a character vector containing all the listed symbols for the
  given exchanges.
}
\author{ Josh Ulrich }
\note{
  The symbols returned by \code{get.symbols} may not be in the format necessary to retrive data
  using \code{yahoo.data}.

  \code{yahoo.data} has only been tested on daily data.  It isn't known if the function correctly
  adjusts data for any other frequency.
}
\examples{
  ### Note: you must have a working internet
  ### connection for these examples to work!
  ibm <- yahoo.data("IBM", 19990404, 20050607)

  nyse.symbols <- get.symbols("NYSE")
}
\keyword{ ts }
