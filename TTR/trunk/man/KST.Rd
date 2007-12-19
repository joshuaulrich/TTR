\name{KST}
\alias{KST}
\title{ Know Sure Thing }
\description{
  The Know Sure Thing (KST) is a smooth, summed, rate of change indicator.  Developed by Martin Pring.
}
\usage{
  KST(price, n=c(10, 15, 20, 30), ma1=list("SMA", n=10), ma2=ma1,
      ma3=ma1, ma4=list("SMA", n=15), ma.sig=list("SMA", n=10), wts=1:4)
}
\arguments{
  \item{price}{ Price series to use. }
  \item{n}{ A vector of the number of periods to use in the ROC calculations. }
  \item{ma1}{ A \emph{list} whose first component is a string containing the moving average
    function name; additional parameters may also be specified as \emph{named} components.
    There must be a \code{ma} (not including \code{ma.sig}) for each period in \code{n}. }
  \item{ma2}{ See \code{ma1}. }
  \item{ma3}{ See \code{ma1}. }
  \item{ma4}{ See \code{ma1}. }
  \item{ma.sig}{ A \emph{list} whose first component is a string containing the signal moving
    average function name; additional parameters may also be specified as \emph{named} components. }
  \item{wts}{ A vector the same length as \code{n}, of the weight for each period
    (need not sum to one). }
}
\details{
  For each day (week, month, etc.), the KST calculates the ROC over several periods.  Those
  ROCs are smoothed using the given moving averages, then multiplied by their respective
  weighting values.  The resulting values are summed for each day (month, week, etc.).
}
\value{
  A vector containing the Know Sure Thing values.
}
\author{ Josh Ulrich }
\references{
  The following site(s) were used to code/document this indicator:\cr
  \url{http://www.pring.com/index.html}\cr
  \url{http://www.pring.com/movieweb/daily_kst.htm}\cr
  \url{http://www.pring.com/articles/article28.htm}\cr
  \url{http://www.pring.com/movieweb/KST_MCM.htm}\cr
}
\note{
  The KST indicates bullish/bearish momentum as it crosses above/below its moving average.
  Because the KST tends to lead price action, look for trend confirmation in the price.

  The default arguments are for the daily KST.  There is also the Long-Term KST, with
  arguments: \code{n=c(9, 12, 18, 24)} - where the periods are months, not days - and the
  moving average periods are 6, 6, 6, and 9 months, respectively.
}
\seealso{
  See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average options; and note
  Warning section.  See \code{\link{ROC}} for the rate-of-change function.
  See \code{\link{oscillator}} for other oscillators.
}
\examples{
  data(ttrc)
  kst <- KST(ttrc[,"Close"])
}
\keyword{ ts }
