\name{stochastic}
\alias{stochastic}
\alias{\%K}
\alias{\%D}
\title{ Stochastic Oscillator }
\description{
  The stochastic oscillator is a momentum indicator that relates the location of each day's
  close relative to the high/low range over the past \code{n} periods.  Developed by George C.
  Lane in the late 1950s.
}
\usage{
  stochastic(HLC, n.fastK=14, ma.fastD=list("SMA", n=3), ma.slowD=ma.fastD)
}
\arguments{
  \item{HLC}{ High-Low-Close price series to use.  If only a univariate series is given,
    it will be used.  See details. }
  \item{n.fastK}{ Number of periods for fast \%K (i.e. the number of past periods to use). }
  \item{ma.fastD}{ A \emph{list} whose first component is a string containing the fast \%D moving
    average function name; additional parameters may also be specified as \emph{named} components. }
  \item{ma.slowD}{ Similar to \code{ma.fastD}, but for the fast \%D moving average. }
}
\details{
  If an High-Low-Close series is provided, the indicator is calculated using the high/low
  values.  If a vector is provided, the calculation only uses that series.  This allows
  stochastics to be calculated for: (1) series that have no HLC definition (e.g. foreign
  exchange), and (2) stochastic indicators (e.g. stochastic RSI - see examples).
}\value{
  A matrix containing the columns:
  \item{ fastK }{ Fast \%K }
  \item{ fastD }{ Fast \%D }
  \item{ slowD }{ Slow \%D }
}
\author{ Josh Ulrich }
\references{
  The following site(s) were used to code/document this indicator:\cr
  \url{http://www.fmlabs.com/reference/StochasticOscillator.htm}\cr
  \url{http://www.equis.com/Customer/Resources/TAAZ?c=3&p=106}\cr
  \url{http://linnsoft.com/tour/techind/stoc.htm}\cr
  \url{http://stockcharts.com/education/IndicatorAnalysis/indic_stochasticOscillator.html}\cr
}
\note{ 
  The calculation for William's \%R is similar to that of stochastics' fast \%K.

  Readings below 20 (above 80) are considered oversold (overbought).  However, readings below 20
  (above 80) are not necessarily bearish (bullish).  Lane believed some of the best sell (buy)
  signals occurred when the oscillator moved from overbought (oversold) back below 80 (above 20).

  Buy (sell) signals can also be given when \%K crosses above (below) \%D.  Crossover signals are
  quite frequent however, which may result in whipsaws.
}
\seealso{
  See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average options; and note
  Warning section.  See \code{\link{WPR}} to compare it's results to fast \%K.
}
\examples{
  data(ttrc)
  stoch.osc <- stochastic(ttrc[,c("High","Low","Close")])
  stoch.wpr <- WPR(ttrc[,c("High","Low","Close")])

  plot(tail(stoch.osc[,"fastK"], 100), type="l", main="Fast \%K and Williams \%R",
    ylab="", ylim = range(cbind(stoch.osc, stoch.wpr), na.rm=TRUE) )
  lines(tail(stoch.wpr, 100), col="blue")
  lines(tail(1-stoch.wpr, 100), col="red", lty="dashed")

  stoch.rsi <- stochastic( RSI(ttrc[,"Close"]) )
}
\keyword{ ts }


