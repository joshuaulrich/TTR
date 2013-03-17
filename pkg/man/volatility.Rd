\name{volatility}
\alias{garman.klass}
\alias{gk.yz}
\alias{parkinson}
\alias{rogers.satchell}
\alias{volatility}
\alias{yang.zhang}
\title{Volatility}
\usage{
  volatility(OHLC, n = 10, calc = "close", N = 260, ...)
}
\arguments{
  \item{OHLC}{Object that is coercible to xts or matrix and
  contains Open-High-Low-Close prices (or only Close
  prices, if \code{calc="close"}).}

  \item{n}{Number of periods for the volatility estimate.}

  \item{calc}{The calculation (type) of estimator to use.}

  \item{N}{Number of periods per year.}

  \item{\dots}{Arguments to be passed to/from other
  methods.}
}
\value{
  A object of the same class as \code{OHLC} or a vector (if
  \code{try.xts} fails) containing the chosen volatility
  estimator values.
}
\description{
  Selected volatility estimators/indicators; various
  authors.
}
\details{
  \itemize{ \item Close-to-Close Volatility
  (\code{calc="close"})\cr \deqn{ \sigma_{cl} =
  \sqrt{\frac{Z}{n-2} \sum_{i=1}^{n-1}(r_i-\bar{r})^2}
  }{sqrt(N) * runSD(ROC(Cl), n-1)} \deqn{where\;\; r_i =
  \log \left(\frac{C_i}{C_{i-1}}\right) }{} \deqn{and\;\;
  \bar{r} = \frac{r_1+r_2+\ldots +r_{n-1}}{n-1} }{}

  \item OHLC Volatility: Garman and Klass
  (\code{calc="garman.klass"})\cr The Garman and Klass
  estimator for estimating historical volatility assumes
  Brownian motion with zero drift and no opening jumps
  (i.e. the opening = close of the previous period). This
  estimator is 7.4 times more efficient than the
  close-to-close estimator.\cr \deqn{ \sigma = \sqrt{
  \frac{Z}{n} \sum \left[
  \textstyle\frac{1}{2}\displaystyle \left( \log
  \frac{H_i}{L_i} \right)^2 - (2\log 2-1) \left( \log
  \frac{C_i}{O_i} \right)^2 \right] } }{sqrt(N/n *
  runSum(0.5 * log(Hi/Lo)^2 - (2*log(2)-1) * log(Cl/Op)^2,
  n))}

  \item High-Low Volatility: Parkinson
  (\code{calc="parkinson"})\cr The Parkinson formula for
  estimating the historical volatility of an underlying
  based on high and low prices.\cr \deqn{ \sigma = \sqrt{
  \frac{Z}{4 n \times \log 2} \sum_{i=1}^{n} \left(\log
  \frac{H_i}{L_i}\right)^2} }{sqrt(N/(4*n*log(2)) *
  runSum(log(Hi/Lo)^2, n))}

  \item OHLC Volatility: Rogers and Satchell
  (\code{calc="rogers.satchell"})\cr The Roger and Satchell
  historical volatility estimator allows for non-zero
  drift, but assumed no opening jump.\cr \deqn{ \sigma =
  \sqrt{ \textstyle\frac{Z}{n} \sum \left[ \log
  \textstyle\frac{H_i}{C_i} \times \log
  \textstyle\frac{H_i}{O_i} + \log
  \textstyle\frac{L_i}{C_i} \times \log
  \textstyle\frac{L_i}{O_i} \right] } }{sqrt(N/n *
  runSum(log(Hi/Cl) * log(Hi/Op) + log(Lo/Cl) * log(Lo/Op),
  n))}

  \item OHLC Volatility: Garman and Klass - Yang and Zhang
  (\code{calc="gk.yz"})\cr This estimator is a modified
  version of the Garman and Klass estimator that allows for
  opening gaps.\cr \deqn{ \sigma = \sqrt{
  \textstyle\frac{Z}{n} \sum \left[ \left( \log
  \textstyle\frac{O_i}{C_{i-1}} \right)^2 +
  \textstyle\frac{1}{2}\displaystyle \left( \log
  \textstyle\frac{H_i}{L_i} \right)^2 - (2 \times \log 2-1)
  \left( \log \textstyle\frac{C_i}{O_i} \right)^2 \right] }
  }{sqrt(N/n * runSum(log(Op/lag(Cl,1))^2 + 0.5 *
  log(Hi/Lo)^2 - (2*log(2)-1) * log(Cl/Op)^2 , n))}

  \item OHLC Volatility: Yang and Zhang
  (\code{calc="yang.zhang"})\cr The Yang and Zhang
  historical volatility estimator has minimum estimation
  error, and is independent of drift and opening gaps.  It
  can be interpreted as a weighted average of the Rogers
  and Satchell estimator, the close-open volatility, and
  the open-close volatility.

  Users may override the default values of \eqn{\alpha}
  (1.34 by default) or \eqn{k} used in the calculation by
  specifying \code{alpha} or \code{k} in \code{\dots},
  respectively. Specifying \code{k} will cause \code{alpha}
  to be ignored, if both are provided.\cr \deqn{ \sigma^2 =
  \sigma_o^2 + k\sigma_c^2 + (1-k)\sigma_{rs}^2 }{ s <-
  sqrt(s2o + k*s2c + (1-k)*(s2rs^2)) } \deqn{ \sigma_o^2
  =\textstyle \frac{Z}{n-1} \sum \left( \log
  \frac{O_i}{C_{i-1}}-\mu_o \right)^2 }{ s2o <- N *
  runVar(log(Op/lag(Cl,1)), n=n) } \deqn{ \mu_o=\textstyle
  \frac{1}{n} \sum \log \frac{O_i}{C_{i-1}} }{} \deqn{
  \sigma_c^2 =\textstyle \frac{Z}{n-1} \sum \left( \log
  \frac{C_i}{O_i}-\mu_c \right)^2 }{ s2c <- N *
  runVar(log(Cl/Op), n=n) } \deqn{ \mu_c=\textstyle
  \frac{1}{n} \sum \log \frac{C_i}{O_i} }{} \deqn{
  \sigma_{rs}^2 = \textstyle\frac{Z}{n} \sum \left( \log
  \textstyle\frac{H_i}{C_i} \times \log
  \textstyle\frac{H_i}{O_i} + \log
  \textstyle\frac{L_i}{C_i} \times \log
  \textstyle\frac{L_i}{O_i} \right) }{ s2rs <-
  volatility(OHLC, n, "rogers.satchell", N, ...) } \deqn{
  k=\frac{\alpha}{1+\frac{n+1}{n-1}} }{ k <- (alpha-1) /
  (alpha + (n+1)/(n-1)) } }
}
\examples{
data(ttrc)
ohlc <- ttrc[,c("Open","High","Low","Close")]
vClose <- volatility(ohlc, calc="close")
vGK <- volatility(ohlc, calc="garman")
vParkinson <- volatility(ohlc, calc="parkinson")
vRS <- volatility(ohlc, calc="rogers")
}
\author{
  Joshua Ulrich
}
\references{
  The following sites were used to code/document these
  indicators. All were created by Thijs van den Berg under
  the GNU Free Documentation License and were retrieved on
  2008-04-20. The links are currently dead, but can be
  accessed via internet archives.\cr \cr Close-to-Close
  Volatility (\code{calc="close"}):\cr
  \url{http://www.sitmo.com/eq/172}\cr \cr OHLC Volatility:
  Garman Klass (\code{calc="garman.klass"}):\cr
  \url{http://www.sitmo.com/eq/402}\cr \cr High-Low
  Volatility: Parkinson (\code{calc="parkinson"}):\cr
  \url{http://www.sitmo.com/eq/173}\cr \cr OHLC Volatility:
  Rogers Satchell (\code{calc="rogers.satchell"}):\cr
  \url{http://www.sitmo.com/eq/414}\cr \cr OHLC Volatility:
  Garman Klass - Yang Zhang (\code{calc="gk.yz"}):\cr
  \url{http://www.sitmo.com/eq/409}\cr \cr OHLC Volatility:
  Yang Zhang (\code{calc="yang.zhang"}):\cr
  \url{http://www.sitmo.com/eq/417}\cr
}
\seealso{
  See \code{\link{TR}} and \code{\link{chaikinVolatility}}
  for other volatility measures.
}
\keyword{ts}

