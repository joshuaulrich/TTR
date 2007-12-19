\name{stochastic}
\alias{stochastic}
\alias{SMI}
\alias{\%K}
\alias{\%D}
\title{ Stochastic Oscillator }
\description{
  The stochastic oscillator is a momentum indicator that relates the location of each day's
  close relative to the high/low range over the past \code{n} periods.  Developed by George C.
  Lane in the late 1950s.  The SMI relates the close to the midpoint of the high/low range.
  Developed by William Blau in 1993.
}
\usage{
  stochastic(HLC, n.fastK=14, ma.fastD=list("SMA", n=3),
                  ma.slowD=ma.fastD)

  SMI(HLC, n=13, ma.slow=list("EMA", n=25),
           ma.fast=list("EMA", n=2), ma.sig=list("EMA", n=9))
}
\arguments{
  \item{HLC}{ High-Low-Close price series to use.  If only a univariate series is given,
    it will be used.  See details. }
  \item{n}{ Number of periods to use. }
  \item{n.fastK}{ Number of periods for fast \%K (i.e. the number of past periods to use). }
  \item{ma.fastD}{ A \emph{list} whose first component is a string containing the fast \%D moving
    average function name; additional parameters may also be specified as \emph{named} components. }
  \item{ma.slowD}{ Similar to \code{ma.fastD}, but for the fast \%D moving average. }
  \item{ma.fast}{ A \emph{list} whose first component is a string containing the fast moving
    average function name; additional parameters may also be specified as \emph{named} components. }
  \item{ma.slow}{ A \emph{list} whose first component is a string containing the slow moving
    average function name; additional parameters may also be specified as \emph{named} components. }
  \item{ma.sig}{ A \emph{list} whose first component is a string containing the signal moving
    average function name; additional parameters may also be specified as \emph{named} components. }
}
\details{
  If an High-Low-Close series is provided, the indicator is calculated using the high/low
  values.  If a vector is provided, the calculation only uses that series.  This allows
  stochastics to be calculated for: (1) series that have no HLC definition (e.g. foreign
  exchange), and (2) stochastic indicators (e.g. stochastic RSI - see examples).
}
\value{
  A matrix containing the columns:
  \item{ fastK }{ Stochastic Fast \%K }
  \item{ fastD }{ Stochastic Fast \%D }
  \item{ slowD }{ Stochastic Slow \%D }
  \item{ SMI }{ Stochastic Momentum Index }
  \item{ signal }{ Stochastic Momentum Index signal line }
}
\author{ Josh Ulrich }
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
  stochOSC <- stochastic(ttrc[,c("High","Low","Close")])
  stochWPR <- WPR(ttrc[,c("High","Low","Close")])

  plot(tail(stochOSC[,"fastK"], 100), type="l",
      main="Fast \%K and Williams \%R", ylab="",
      ylim=range(cbind(stochOSC, stochWPR), na.rm=TRUE) )
  lines(tail(stochWPR, 100), col="blue")
  lines(tail(1-stochWPR, 100), col="red", lty="dashed")

  stochRSI <- stochastic( RSI(ttrc[,"Close"]) )
}
\keyword{ ts }


