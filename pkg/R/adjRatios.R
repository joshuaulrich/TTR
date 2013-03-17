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

#'Split and dividend adjustment ratios
#'
#'Create split and dividend adjustment ratio vectors.
#'
#'@aliases adjRatios adjust
#'@param splits Split series that is coercible to xts.
#'@param dividends Dividend series that is coercible to xts.
#'@param close Close price series that is coercible to xts.
#'@return A xts object containing the columns:
#' \describe{
#'   \item{ Split }{ The split adjustment ratio. }
#'   \item{ Div }{ The dividend adjustment ratio. }
#' }
#'@details
#' \itemize{
#'   \item If only \code{splits} is provided, the resulting object will
#'     only have as many observations as \code{splits}.
#'   \item If \code{splits} and \code{close} are provided, the resulting
#'     object will have as many observations as \code{max(NROW(splits),
#'     NROW(close))}.
#'   \item \code{close} is required if \code{dividends} is provided.
#' }
#'
#'@author Joshua Ulrich
#'@keywords ts
#'@export
'adjRatios' <-
function(splits, dividends, close) {

  if( !missing(dividends) &&
       missing(close) )
    stop('"close" must be specified to adjust dividends')
       
  # Really need a better error message if as.xts fails... seriously
  if(missing(close) || all(is.na(close)) || NROW(close)==0) {
    close <- NA
  } else {
    if(NCOL(close)!=1) stop('"close" must be univariate')
    close <- try.xts(close,
      error=stop('"as.xts(close)" failed'))
  }
  if(missing(splits) || all(is.na(splits)) || NROW(splits)==0) {
    splits <- NA
  } else {
    if(NCOL(splits)!=1) stop('"splits" must be univariate')
    splits <- try.xts(splits,
      error=stop('"as.xts(splits)" failed'))
  }
  if(missing(dividends) || all(is.na(dividends)) || NROW(dividends)==0) {
    dividends <- NA
  } else {
    if(NCOL(dividends)!=1) stop('"dividends" must be univariate')
    dividends <- try.xts(dividends,
      error=stop('"as.xts(dividends)" failed'))
  }

  obj <- merge.xts(close,splits,dividends)
  if(!isTRUE(is.na(close))) {
    obj <- obj[!is.na(obj[,1]),]  # drop rows missing close prices
  }
  adj <- .Call('adjRatios',obj[,2],obj[,3],obj[,1],PACKAGE="TTR")
  adj <- xts(cbind(adj[[1]],adj[[2]]),index(obj))
  colnames(adj) <- c('Split','Div')
  
  return(adj)

}
