\name{WPR}
\alias{WPR}
\title{William's \%R}
\usage{
  WPR(HLC, n = 14)
}
\arguments{
  \item{HLC}{Object that is coercible to xts or matrix and
  contains High-Low-Close prices.  If only a univariate
  series is given, it will be used.  See details.}

  \item{n}{Number of periods to use.}
}
\value{
  A object of the same class as \code{HLC} or a vector (if
  \code{try.xts} fails) containing the William's \%R
  values.
}
\description{
  William's \% R.
}
\details{
  If an High-Low-Close series is provided, the indicator is
  calculated using the high/low values.  If a vector is
  provided, the calculation only uses that series.
}
\note{
  The William's \%R calculation is similar to stochastics'
  fast \%K.

  The value for William's \%R will be 0.5 whenever the
  highest high and lowest low are the same over the last
  \code{n} periods.
}
\examples{
data(ttrc)
stochOsc <- stoch(ttrc[,c("High","Low","Close")])
stochWPR<- WPR(ttrc[,c("High","Low","Close")])

plot(tail(stochOsc[,"fastK"], 100), type="l",
    main="Fast \%K and Williams \%R", ylab="",
    ylim=range(cbind(stochOsc, stochWPR), na.rm=TRUE) )
lines(tail(stochWPR, 100), col="blue")
lines(tail(1-stochWPR, 100), col="red", lty="dashed")
}
\author{
  Joshua Ulrich
}
\references{
  The following site(s) were used to code/document this
  indicator:\cr
  \url{http://www.fmlabs.com/reference/WilliamsR.htm}\cr
  \url{http://www.equis.com/Customer/Resources/TAAZ?c=3&p=126}\cr
  \url{http://linnsoft.com/tour/techind/willR.htm}\cr
  \url{http://stockcharts.com/education/IndicatorAnalysis/indic_williamsR.html}\cr
}
\seealso{
  See \code{\link{stoch}}.
}
\keyword{ts}

