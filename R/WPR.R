#
#   TTR: Technical Trading Rules
#
#   Copyright (C) 2007-2013  Joshua M. Ulrich
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 2 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

#' William's \%R
#'
#' William's \% R.
#'
#' If an High-Low-Close series is provided, the indicator is calculated using
#' the high/low values.  If a vector is provided, the calculation only uses that
#' series.
#'
#' @param HLC Object that is coercible to xts or matrix and contains
#' High-Low-Close prices.  If only a univariate series is given, it will be
#' used.  See details.
#' @param n Number of periods to use.
#' @param scale Scale the result to be between 0 and -100.
#' @return A object of the same class as \code{HLC} or a vector (if
#' \code{try.xts} fails) containing the William's \%R values.
#' @note The William's \%R calculation is similar to stochastics' fast \%K,
#' and the result of \code{WPR} is equal to \code{1-fastK}.
#'
#' The value for William's \%R will be 0.5 whenever the highest high and
#' lowest low are the same over the last \code{n} periods.
#'
#' William's \%R is usually scaled to be between 0 and -100, which is not what
#' \code{WPR} returns by default. Set \code{scale = TRUE} to return the result
#' with the usual scaling.
#'
#' @author Joshua Ulrich
#' @seealso See \code{\link{stoch}}.
#' @references The following site(s) were used to code/document this
#' indicator:\cr
#' \url{https://www.fmlabs.com/reference/WilliamsR.htm}\cr
#' \url{https://www.metastock.com/Customer/Resources/TAAZ/?p=126}\cr
#' \url{https://www.linnsoft.com/techind/williams-r-wpr}\cr
#' \url{https://school.stockcharts.com/doku.php?id=technical_indicators:williams_r}\cr
#' @keywords ts
#' @examples
#'
#'  data(ttrc)
#'  hlc <- ttrc[,c("High","Low","Close")]
#'  stochOsc <- stoch(hlc)
#'  stochWPR <- WPR(hlc)
#'
#'  # WPR is a transformation of stochastics' fastK
#'  all.equal(stochWPR, 1-stochOsc[,'fastK'])  # TRUE
#'
#'  # WPR converted to the usual scaling between 0 and -100
#'  scaledWPR <- WPR(hlc, scale=TRUE)
#'
#'  plot(tail(stochOsc[,"fastK"], 100), type="l",
#'      main="Fast %K and Williams %R", ylab="",
#'      ylim=range(cbind(stochOsc, stochWPR), na.rm=TRUE) )
#'  lines(tail(stochWPR, 100), col="blue")
#'  lines(tail(1-stochWPR, 100), col="red", lty="dashed")
#'
"WPR" <-
function(HLC, n=14, scale=FALSE) {

  # William's Percent R (similar to Stochastics' fast %K)

  HLC <- try.xts(HLC, error=as.matrix)

  # Calculation if HLC series is given
  if(NCOL(HLC)==3) {
    high  <- HLC[,1]
    low   <- HLC[,2]
    close <- HLC[,3]
  } else

  # Calculation if price vector is given
  if(NCOL(HLC)==1) {
    high  <- HLC
    low   <- HLC
    close <- HLC
  } else

  stop("Price series must be either High-Low-Close, or Close")

  hmax <- runMax(high, n)
  lmin <- runMin( low, n)

  pctR <- (hmax - close) / (hmax - lmin)
  pctR[is.nan(pctR)] <- 0.5

  if(isTRUE(scale)) {
      pctR <- -100 * pctR
  }

  reclass( pctR, HLC )
}
