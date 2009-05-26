\name{KST}
\alias{KST}
\title{ Know Sure Thing }
\description{
  The Know Sure Thing (KST) is a smooth, summed, rate of change indicator.  Developed by Martin Pring.
}
\usage{
  KST(price, n=c(10,10,10,15), nROC=c(10,15,20,30), nSig=9,
         maType, wts=1:NROW(n), \dots)
}
\arguments{
  \item{price}{ Price series that is coercible to xts or matrix. }
  \item{n}{ A vector of the number of periods to use in the MA calculations. }
  \item{nROC}{ A vector of the number of periods to use in the ROC calculations. }
  \item{nSig}{ The number of periods to use for the KST signal line. }
  \item{maType}{ Either: \cr(1) A function or a string naming the function to be called, or\cr
    (2) a \emph{list} with the first component like (1) above, and additional parameters
    specified as \emph{named} components.  See Examples. }
  \item{wts}{ A vector the same length as \code{n}, of the weight for each period
    (need not sum to one). }
  \item{\dots}{ Other arguments to be passed to the \code{maType} function in case (1) above. }
}
\details{
  For each day (week, month, etc.), the KST calculates the ROC over several periods.  Those
  ROCs are smoothed using the given moving averages, then multiplied by their respective
  weighting values.  The resulting values are summed for each day (month, week, etc.).
}
\value{
  A object of the same class as \code{price} or a vector (if \code{try.xts}
  fails) containing the Know Sure Thing values.
}
\author{ Joshua Ulrich }
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
  See \code{\link{MACD}} for a generic oscillator.
}
\examples{
  data(ttrc)
  kst <- KST(ttrc[,"Close"])
  
  kst4MA <- KST(ttrc[,"Close"],
    maType=list(list(SMA),list(EMA),list(DEMA),list(WMA)))
}
\keyword{ ts }
