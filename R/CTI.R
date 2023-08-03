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

#' Ehler's Correlation Trend Indicator
#'
#' Ehler's Correlation Trend Indicator (CTI) measures the Spearman correlation
#' of the price with the ideal trend line: a straight line with increasing
#' slope.
#'
#' The CTI measures the Spearman correlation between the price and the ideal
#' trend line with slope of \code{slope}, over the past \code{n} days.
#'
#' See URL in references section for further details.
#'
#' @param price Price series that is coercible to xts or matrix.
#' @param n Number of periods to use.
#' @param slope Slope of desired trend.
#' @return A object of the same class as \code{price} or a matrix (if
#' \code{try.xts} fails) with the column:
#'  \describe{
#'   \item{cti}{ The Correlation Trend Indicator. }
#'  }
#' @note Positive/negative CTI values signal positive/negative correlation with
#' the desired trend line slope. A simple strategy could be long when the CTI
#' is positive and, short when it is negative.
#' @author Ethan Smith, Joshua Ulrich
#' @seealso See \code{\link{aroon}}, \code{\link{CCI}}, \code{\link{ADX}},
#' \code{\link{VHF}}, \code{\link{GMMA}}, \code{\link{TDI}} for other
#' indicators that measure trend direction/strength.
#' @references
#' John Ehlers, Correlation Trend Indicator, Stocks & Commodities May-2020
#' The following site(s) were used to code/document this indicator:\cr
#' \url{https://financial-hacker.com/petra-on-programming-a-unique-trend-indicator/}\cr
#' @keywords ts
#' @examples
#'
#' data(ttrc)
#' cti <- CTI(ttrc[,"Close"], n = 20)
#'
CTI <-
function(price, n = 20, slope = 1)
{
  x <- try.xts(price, error = as.matrix)
  y <- slope * seq_along(x)

  f <- function(.) {
    cor(.[,1], .[,2], method = "spearman")
  }
  cti <- rollapplyr(cbind(x, y), n, f, by.column = FALSE, fill = NA)

  if(!is.null(dim(cti))) {
    colnames(cti) <- "cti"
  }

  reclass(cti, x)
}
