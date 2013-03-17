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

#'Analysis of Running/Rolling/Moving Windows
#'
#'Various functions to analyze data over a moving window of periods.
#'
#'
#'@aliases rollFun rollSFM
#'@param Ra Object coercible to xts or matrix, containing the excess
#'return for an individual security
#'@param Rb Object coercible to xts or matrix, containing the market
#'/ benchmark return
#'@param n Number of periods to use in the window
#'
#'@return A object of the same class as \code{Ra} (and \code{Rb}?) or a vector
#'(if \code{try.xts} fails).
#' \describe{
#'  \item{rollSFM}{returns single-factor model parameters and R-squared
#'    over a n-period moving window.}
#' }
#' 
#'@author Joshua Ulrich
#'@references The following site(s) were used to code/document this
#'indicator:
#'\url{http://en.wikipedia.org/wiki/Simple_linear_regression}\cr
#'@keywords ts
#'@rdname rollFun

#'@export
rollSFM <- function(Ra, Rb, n = 60) {
  # Calculate a rolling single-factor model
#  stopifnot(is.xts(Ra) && is.xts(Rb))
  # calculate beta
  beta <- runCov(Ra, Rb, n) / runVar(Rb, n=n)
  # calculate alpha
  alpha <- runMean(Ra, n) - beta * runMean(Rb, n)
  # calculate R-squared
  se.resid <-
    1/(n*(n-2)) * (n*runSum(Ra^2,n)-runSum(Ra,n)^2
       - beta^2 * (n*runSum(Rb^2,n)-runSum(Rb,n)^2))
  se.Ra <- runVar(Ra, n=n) * (n-1)/(n-2)
  r.squared <- 1 - se.resid / se.Ra
  result <- merge(alpha, beta, r.squared)
  return(result)
}
