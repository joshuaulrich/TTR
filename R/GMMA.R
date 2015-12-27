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

#'Guppy Multiple Moving Averages
#'
#'Calculate the Guppy Multiple Moving Average of a series.
#'
#'The Guppy Multiple Moving Average signals a changing trend when the
#'\code{short} and \code{long} groups of moving averages intersect.  An up/down
#'trend exists when the short/long-term moving averages are greater than the
#'long/short-term averages.
#'
#'@aliases GMMA Guppy guppy
#'@param x Price, volume, etc. series that is coercible to xts or matrix.
#'@param short Vector of short-term periods.
#'@param long Vector of long-term periods.
#'@param maType Either:
#' \enumerate{
#'   \item A function or a string naming the function to be called.
#'   \item A \emph{list} with the first component like (1) above, and
#'     additional parameters specified as \emph{named} components.
#'     See Examples.
#' }
#'@return A object of the same class as \code{x} or \code{price} or a vector
#'(if \code{try.xts} fails) containing the Guppy Multiple Moving Average.
#'@author Joshua Ulrich
#'@seealso See \code{\link{aroon}}, \code{\link{CCI}}, \code{\link{ADX}},
#'\code{\link{VHF}}, \code{\link{TDI}} for other indicators that measure trend
#'direction/strength.
#'@references The following site(s) were used to code/document this
#'indicator:\cr
#'\url{http://www.investopedia.com/terms/g/guppy-multiple-moving-average.asp}\cr
#'@keywords ts
#'@examples
#'
#' data(ttrc)
#' gmma <- GMMA(ttrc[,"Close"])
#'
#'@export
"GMMA" <-
function(x, short=c(3,5,8,10,12,15), long=c(30,35,40,45,50,60), maType) {

  # Guppy Multiple Moving Average

  x <- try.xts(x, error=as.matrix)
  
  # Default MA
  if(missing(maType)) {
    maType <- 'EMA'
  }
  
  fn <- function(g) { do.call(maType, list(x,n=g)) }
  gmma <- do.call(cbind, lapply(c(short,long), fn))
  colnames(gmma) <- c(paste('short lag',short),paste('long lag',long))
  
  reclass(gmma, x)
}
