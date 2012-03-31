\name{stochastics}
\alias{stochastics}
\alias{stochastic}
\alias{stoch}
\alias{SMI}
\alias{\%K}
\alias{\%D}
\title{ Stochastic Oscillator / Stochastic Momentum Index }
\description{
  The stochastic oscillator is a momentum indicator that relates the location of each day's
  close relative to the high/low range over the past \code{n} periods.  Developed by George C.
  Lane in the late 1950s.  The SMI relates the close to the midpoint of the high/low range.
  Developed by William Blau in 1993.
}
\usage{
  stoch(HLC, nFastK=14, nFastD=3, nSlowD=3, maType, bounded=TRUE, smooth=1, \dots)

  SMI(HLC, n=13, nFast=2, nSlow=25, nSig=9, maType, bounded=TRUE, \dots)
}
\arguments{
  \item{HLC}{ Object that is coercible to xts or matrix and contains High-Low-Close prices.
   If only a univariate series is given, it will be used.  See details. }
  \item{n}{ Number of periods to use. }
  \item{nFastK}{ Number of periods for fast \%K (i.e. the number of past periods to use). }
  \item{nFastD}{ Number of periods for fast \%D (i.e. the number smoothing periods to apply
    to fast \%K). }
  \item{nSlowD}{ Number of periods for slow \%D (i.e. the number smoothing periods to apply
    to fast \%D). }
  \item{smooth}{ Number of internal smoothing periods to be applied before calculating
    FastK. See Details. }
  % What are the correct terms to describe these arguments???
  \item{nFast}{ Number of periods for initial smoothing. }
  \item{nSlow}{ Number of periods for double smoothing. }
  \item{nSig}{ Number of periods for signal line. }
  \item{maType}{ Either: \cr(1) A function or a string naming the function to be called, or\cr
    (2) a \emph{list} with the first component like (1) above, and additional parameters
    specified as \emph{named} components.  See Examples. }
  \item{bounded}{ Logical, should current period's values be used in the calculation? }
  \item{\dots}{ Other arguments to be passed to the \code{maType} function in case (1) above. }
}
\details{
  If a High-Low-Close series is provided, the indicator is calculated using the high/low
  values.  If a vector is provided, the calculation only uses that series.  This allows
  stochastics to be calculated for: (1) series that have no HLC definition (e.g. foreign
  exchange), and (2) stochastic indicators (e.g. stochastic RSI - see examples).

  The \code{smooth} argument is the number of periods of internal smoothing to apply
  to the differences in the high-low-close range before calculating Fast K.  Thanks to
  Stanley Neo for the suggestion.
}
\value{
  A object of the same class as \code{HLC} or a matrix (if \code{try.xts}
  fails) containing the columns:
  \item{ fastK }{ Stochastic Fast \%K }
  \item{ fastD }{ Stochastic Fast \%D }
  \item{ slowD }{ Stochastic Slow \%D }
  \item{ SMI }{ Stochastic Momentum Index }
  \item{ signal }{ Stochastic Momentum Index signal line }
}
\author{ Joshua Ulrich }
\references{
  The following site(s) were used to code/document these indicators:\cr
  Stochastic Oscillator:\cr
  \url{http://www.fmlabs.com/reference/StochasticOscillator.htm}\cr
  \url{http://www.equis.com/Customer/Resources/TAAZ?c=3&p=106}\cr
  \url{http://linnsoft.com/tour/techind/stoc.htm}\cr
  \url{http://stockcharts.com/education/IndicatorAnalysis/indic_stochasticOscillator.html}\cr
  SMI:\cr
  \url{http://www.fmlabs.com/reference/default.htm?url=SMI.htm}\cr
}
\note{ 
  The calculation for William's \%R is similar to that of stochastics' fast \%K.

  The stochastic oscillator and SMI calculate relative value of the close versus the high/low
  range and the midpoint of the high/low range, respectively.
  
  The stochastic oscillator and the stochastic momentum index are interpreted similarly.
  Readings below 20 (above 80) are considered oversold (overbought).  However, readings below 20
  (above 80) are not necessarily bearish (bullish).  Lane believed some of the best sell (buy)
  signals occurred when the oscillator moved from overbought (oversold) back below 80 (above 20).

  For the stochastic oscillator, buy (sell) signals can also be given when \%K crosses above
  (below) \%D.  Crossover signals are quite frequent however, which may result in whipsaws.
}
\seealso{
  See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average options; and note
  Warning section.  See \code{\link{WPR}} to compare it's results to fast \%K.
}
\examples{
  data(ttrc)
  stochOSC <- stoch(ttrc[,c("High","Low","Close")])
  stochWPR <- WPR(ttrc[,c("High","Low","Close")])

  plot(tail(stochOSC[,"fastK"], 100), type="l",
      main="Fast \%K and Williams \%R", ylab="",
      ylim=range(cbind(stochOSC, stochWPR), na.rm=TRUE) )
  lines(tail(stochWPR, 100), col="blue")
  lines(tail(1-stochWPR, 100), col="red", lty="dashed")

  stoch2MA <- stoch( ttrc[,c("High","Low","Close")],
      maType=list(list(SMA), list(EMA, wilder=TRUE), list(SMA)) )

  SMI3MA <- SMI(ttrc[,c("High","Low","Close")],
      maType=list(list(SMA), list(EMA, wilder=TRUE), list(SMA)) )

  stochRSI <- stoch( RSI(ttrc[,"Close"]) )
}
\keyword{ ts }
