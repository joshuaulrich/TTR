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

#'Trend Detection Index
#'
#'The Trend Detection Index (TDI) attempts to identify starting and ending
#'trends.  Developed by M. H. Pee.
#'
#'The TDI is the (1) absolute value of the \code{n}-day sum of the \code{n}-day
#'momentum, minus the quantity of (2) \code{multiple}*\code{n}-day sum of the
#'absolute value of the \code{n}-day momentum, minus (3) \code{n}-day sum of
#'the absolute value of the \code{n}-day momentum.
#'
#'I.e. TDI = (1) - [ (2) - (3) ]
#'
#'The direction indicator is the sum of the \code{n}-day momentum over the last
#'\code{n} days.
#'
#'See URL in references section for further details.
#'
#'@param price Price series that is coercible to xts or matrix.
#'@param n Number of periods to use.
#'@param multiple Multiple used to calculate (2).
#'@return A object of the same class as \code{price} or a matrix (if
#'\code{try.xts} fails) containing the columns:
#' \describe{
#'  \item{ tdi }{ The Trend Detection Index. }
#'  \item{ di }{ The Direction Indicator. }
#' }
#'@note Positive/negative TDI values signal a trend/consolidation.  A positive/
#'negative direction indicator signals a up/down trend.  I.e. buy if the TDI
#'and the direction indicator are positive, and sell if the TDI is positive
#'while the direction indicator is negative.
#'@author Joshua Ulrich
#'@seealso See \code{\link{aroon}}, \code{\link{CCI}}, \code{\link{ADX}},
#'\code{\link{VHF}}, \code{\link{GMMA}} for other indicators that measure trend
#'direction/strength.
#'@references The following site(s) were used to code/document this
#'indicator:\cr
#'\url{http://www.linnsoft.com/tour/techind/tdi.htm}\cr
#'@keywords ts
#'@examples
#'
#' data(ttrc)
#' tdi <- TDI(ttrc[,"Close"], n=30)
#'
#'@export
"TDI" <-
function(price, n=20, multiple=2) {

  # Trend Detection Index

  price <- try.xts(price, error=as.matrix)
  
  mom <- momentum(price, n, na.pad=TRUE)
  mom[is.na(mom)] <- 0

  di  <- runSum(mom, n)
  abs.di <- abs(di)

  abs.mom.2n <- runSum(abs(mom), n*multiple)
  abs.mom.1n <- runSum(abs(mom), n  )

  tdi <- abs.di - (abs.mom.2n - abs.mom.1n)

  result <- cbind( tdi,di )
  colnames(result) <- c( "tdi","di" )

  reclass( result, price )
}
