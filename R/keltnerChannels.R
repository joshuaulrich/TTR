#
#   TTR: Technical Trading Rules
#
#   Copyright (C) 2020  Joshua M. Ulrich
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

#' Keltner Channels
#'
#' Keltner Channels are volatility-based envelopes set above and below a moving
#' average. This indicator is similar to Bollinger Bands, but Keltner Channels
#' use the Average True Range (ATR) to set channel distance.
#'
#' Keltner Channels are a trend following indicator, and can also be used to
#' identify overbought and oversold levels when there is no trend.
#'
#' Chester Keltner is credited with the original version of Keltner Channels in
#' his 1960 book. Linda Bradford Raschke introduced the newer version of
#' Keltner Channels in the 1980s.
#'
#' @aliases keltnerChannels
#'
#' @param HLC Object that is coercible to xts or matrix and contains
#' High-Low-Close prices. If only a univariate series is given, it will be used.
#' See details.
#' @param n Number of periods for moving average.
#' @param maType A function or a string naming the function to be called.
#' @param atr The number of average true range distances to apply.
#' @param ... Other arguments to be passed to the maType function.
#'
#' @section Details : Keltner Channels consist of three lines:
#' The middle band is generally a 20-period EMA of the typical price
#' ([high + low + close]/3). The upper and lower bands are multiples of average
#' true range (usually 2) above and below the MA.
#'
#' The middle band is usually calculated using the typical price, but if a
#' univariate series (e.g. Close, Weighted Close, Median Price, etc.) is
#' provided, it will be used instead.
#'
#' @return A object of the same class as \code{HLC} or a matrix (if
#' \code{try.xts} fails) containing the columns:
#'  \describe{
#'     \item{SMA}{ Simple moving average. }
#'     \item{EMA}{ Exponential moving average. }
#'  }
#'
#' \item{dn}{ The lower Keltner Channel. }
#' \item{mavg}{ The middle moving average. }
#' \item{up}{ The upper Keltner Channel. }
#'
#' @author Nick Procyk, Joshua Ulrich
#'
#' References
#' @references The following site(s) were used to code/document this
#' indicator:\cr
#' \url{https://school.stockcharts.com/doku.php?id=technical_indicators:keltner_channels}\cr
#' \url{https://www.linnsoft.com/techind/keltner-channels-keltu-keltd}\cr
#' \url{https://www.investopedia.com/terms/k/keltnerchannel.asp}\cr
#'
#' @seealso See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average
#' options; and note Warning section.
#'
#' @examples
#'
#' data(ttrc)
#' kc <- keltnerChannels(ttrc[,c("High","Low","Close")])
#'
#' @keywords ts
#' @rdname keltnerChannels
keltnerChannels <-
function (HLC, n = 20, maType, atr = 2, ...)
{
  atrHLC <- HLC
  HLC <- try.xts(HLC, error = as.matrix)
  if (NCOL(HLC) == 3) {
    if (is.xts(HLC)) {
      xa <- xcoredata(HLC)
      HLC <- xts(apply(HLC, 1, mean), index(HLC))
      xcoredata(HLC) <- xa
    }
    else {
      HLC <- apply(HLC, 1, mean)
    }
  }
  else if (NCOL(HLC) != 1) {
    stop("Price series must be either High-Low-Close, or Close/univariate.")
  }
  maArgs <- list(n = n, ...)
  if (missing(maType)) {
    maType <- "EMA"
  }
  mavg <- do.call(maType, c(list(HLC), maArgs))
  avgtruerange <- ATR(atrHLC, n = n)

  up <- mavg + atr * avgtruerange[,2]
  dn <- mavg - atr * avgtruerange[,2]

  res <- cbind(dn, mavg, up)
  colnames(res) <- c("dn", "mavg", "up")
  reclass(res, HLC)
}
