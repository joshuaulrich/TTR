#
#   TTR: Technical Trading Rules
#
#   Copyright (C) 2007-2008  Joshua M. Ulrich
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

"CMO" <-
function(x, n=14) {

  # Chande Momentum Oscillator

  # http://www.fmlabs.com/reference/CMO.htm

  x <- try.xts(x, error=as.matrix)
  
  up <- momentum(x, n=1)
  dn <- ifelse(up<0, abs(up), 0)
  up <- ifelse(up>0,     up , 0)

  up <- runSum(up, n)
  dn <- runSum(dn, n)

  cmo <- 100 * (up-dn)/(up+dn)

  reclass( cmo, x )
}
