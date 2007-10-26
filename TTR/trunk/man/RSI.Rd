\name{RSI}
\alias{RSI}
\title{ Relative Strength Index }
\description{
  The Relative Strength Index (RSI) calculates a ratio of the recent upward price
  movements to the absolute price movement.  Developed by J. Welles Wilder.
}
\usage{
  RSI(price, ma.up=list("EMA", n=14, wilder=TRUE), ma.down=ma.up)
}
\arguments{
  \item{price}{ Price series to use. }
  \item{ma.up}{ A \emph{list} whose first component is a string containing the upward price
    movement moving average function name; additional parameters may also be specified
    as \emph{named} components. }
  \item{ma.down}{ Similar to \code{ma.up}, but for the downward price movement moving average. }
}
\details{
  The RSI calculation is \code{RSI = 100 - 100 / ( 1 + RS )}, where \code{RS} is the smoothed
  ratio of \sQuote{average} gains over \sQuote{average} losses.  The \sQuote{averages} aren't true
  averages, since they're divided by the value of \code{n} not the number of gain/loss periods.
}
\value{
  A vector containing the RSI values.
}
\author{ Josh Ulrich }
\references{
  The following site(s) were used to code/document this indicator:\cr
  \url{http://www.fmlabs.com/reference/RSI.htm}\cr
  \url{http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=100}\cr
  \url{http://linnsoft.com/tour/techind/rsi.htm}\cr
  \url{http://stockcharts.com/education/IndicatorAnalysis/indic_RSI.html}\cr
}
\note{
  The RSI is usually interpreted as an overbought/oversold (over 70 / below 30) indicator.
  Divergence with price may also be useful.  For example, if price is making new highs/lows,
  but RSI is not, it could indicate a reversal.

  You can calculate a stochastic RSI by using the function \code{\link{stochastic}} on
  RSI values.
}
\seealso{
  See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average options; and note
  Warning section.  See \code{\link{CMO}} for a variation on RSI.
}
\examples{
  data(ttrc)
  rsi <- RSI(ttrc[,"Close"])
}
\keyword{ ts }

