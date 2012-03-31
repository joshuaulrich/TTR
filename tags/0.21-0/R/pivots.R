#
#   TTR: Technical Trading Rules
#
#   Copyright (C) 2007-2011  Joshua M. Ulrich
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

"pivots" <-
function(data, lagts=TRUE) {

  # Author: Brian G. Peterson
  # http://www.investopedia.com/articles/forex/05/FXpivots.asp
  # http://www.investopedia.com/articles/technical/04/041404.asp

  # CentralPivot Point (P) = (High + Low + Close) / 3
  center <- xts(rowSums(HLC(data))/3,order.by=index(data))

  R1 <- (2*center)-Lo(data)  # First Resistance (R1) = (2*P) - Low
  S1 <- (2*center)-Hi(data)  # First Support (S1) = (2*P) - High
  R2 <- center + (R1 - S1)   # Second Resistance (R2) = P + (R1-S1)
  S2 <- center - (R1 - S1)   # Second Support (S2) = P - (R1- S1)
  ret <- cbind(center,R1,R2,S1,S2)
  colnames(ret) <- c('center','R1','R2','S1','S2')
  if(lagts){
    newrow <- xts(t(rep(NA,5)), order.by=last(index(data))+1)
    ret <- rbind(ret,newrow)
    ret <- lag(ret)
  }
  return(ret)
}

