#
#   TTR: Technical Trading Rules
#
#   Copyright (C) 2007-2010  Joshua M. Ulrich
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
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

'adjRatios' <-
function(splits, dividends, close) {

  if( !missing(dividends) &&
       missing(close) )
    stop('\'close\' must be specified to adjust dividends')
       
  # Really need a better error message if as.xts fails... seriously
  if(missing(close) || all(is.na(close)) || NROW(close)==0) {
    close <- NA
  } else {
    close <- try.xts(close,
      error=stop('\'as.xts(close)\' failed'))
  }
  if(missing(splits) || all(is.na(splits)) || NROW(splits)==0) {
    splits <- NA
  } else {
    splits <- try.xts(splits,
      error=stop('\'as.xts(splits)\' failed'))
  }
  if(missing(dividends) || all(is.na(dividends)) || NROW(dividends)==0) {
    dividends <- NA
  } else {
    dividends <- try.xts(dividends,
      error=stop('\'as.xts(dividends)\' failed'))
  }

  obj <- merge.xts(close,splits,dividends)
  adj <- .Call('adjRatios',obj[,2],obj[,3],obj[,1])
  adj <- xts(cbind(adj[[1]],adj[[2]]),index(obj))
  colnames(adj) <- c('Split','Div')
  
  return(adj)

}
