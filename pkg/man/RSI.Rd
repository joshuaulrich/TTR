\name{RSI}
\alias{RSI}
\title{ Relative Strength Index }
\description{
  The Relative Strength Index (RSI) calculates a ratio of the recent upward price
  movements to the absolute price movement.  Developed by J. Welles Wilder.
}
\usage{
  RSI(price, n=14, maType, \dots)
}
\arguments{
  \item{price}{ Price series that is coercible to xts or matrix. }
  \item{n}{ Number of periods for moving averages. }
  \item{maType}{ Either: \cr(1) A function or a string naming the function to be called, or\cr
    (2) a \emph{list} with the first component like (1) above, and additional parameters
    specified as \emph{named} components.  See Examples. }
  \item{\dots}{ Other arguments to be passed to the \code{maType} function in case (1) above. }
}
\details{
  The RSI calculation is \code{RSI = 100 - 100 / ( 1 + RS )}, where \code{RS} is the smoothed
  ratio of 'average gains over 'average' losses.  The 'averages' aren't true
  averages, since they're divided by the value of \code{n} not the number of gain/loss periods.
}
\value{
  A object of the same class as \code{price} or a vector (if \code{try.xts}
  fails) containing the RSI values.
}
\author{ Joshua Ulrich }
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

  You can calculate a stochastic RSI by using the function \code{\link{stoch}} on
  RSI values.
}
\seealso{
  See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average options; and note
  Warning section.  See \code{\link{CMO}} for a variation on RSI.
}
\examples{
  data(ttrc)
  price <- ttrc[,"Close"]

  # Default case
  rsi <- RSI(price)

  # Case of one 'maType' for both MAs
  rsiMA1 <- RSI(price, n=14, maType="WMA", wts=ttrc[,"Volume"])

  # Case of two different 'maType's for both MAs
  rsiMA2 <- RSI(price, n=14, maType=list(maUp=list(EMA,ratio=1/5),
                maDown=list(WMA,wts=1:10)))

}
\keyword{ ts }

