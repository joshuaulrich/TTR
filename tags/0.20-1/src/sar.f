c
c   TTR: Technical Trading Rules
c
c   Copyright (C) 2007-2008  Joshua M. Ulrich
c
c   This program is free software: you can redistribute it and/or modify
c   it under the terms of the GNU General Public License as published by
c   the Free Software Foundation, either version 3 of the License, or
c   (at your option) any later version.
c
c   This program is distributed in the hope that it will be useful,
c   but WITHOUT ANY WARRANTY; without even the implied warranty of
c   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c   GNU General Public License for more details.
c
c   You should have received a copy of the GNU General Public License
c   along with this program.  If not, see <http://www.gnu.org/licenses/>.
c

c
c     Parabolic Stop and Reverse
c
c     iha : input array of high prices
c     ila : input array of low prices
c     la  : length of arrays
c     af  : acceleration factor
c     maf : maximum acceleration factor
c     sar : sar (output) array
c
      subroutine psar(iha, ila, la, af, maf, sar)
      implicit none

      integer la, i, sig(la) 
      double precision iha(la), ila(la), af, maf
      double precision sar(la), afv(la), ep(la), lmin, lmax

c
c     Initialize vectors used by the routine
c
      sig(1) = 1
      ep(1)  = iha(1)
      sar(1) = ila(1)-0.01
      afv(1) = af

      do 10 i=2,la

        lmin = MIN( ila(i-1), ila(i) )
        lmax = MAX( iha(i-1), iha(i) )
c
c       Create signal and extreme price vectors
c

c
c       Previous buy signal
c
        if( sig(i-1) .EQ. 1 ) then
c
c           New signal
c
            if( ila(i) .GT. sar(i-1) ) then
                sig(i) = 1
            else
                sig(i) = -1
            endif
c
c           New extreme price
c
            if( iha(i) .GT. ep(i-1) ) then
                ep(i) = iha(i)
            else
                ep(i) = ep(i-1)
            endif
c
c       Previous sell signal
c
        else 
c
c           New signal
c
            if( iha(i) .LT. sar(i-1) ) then
                sig(i) = -1
            else
                sig(i) = 1
            endif
c
c           New extreme price
c
            if( ila(i) .LT. ep(i-1) ) then
                ep(i) = ila(i)
            else
                ep(i) = ep(i-1)
            endif
        endif
c
c       Create acceleration factor (afv) vector
c       and stop-and-reverse (sar) vector
c

c
c       No signal change
c
        if( sig(i) .EQ. sig(i-1) ) then
            sar(i) = sar(i-1) + ( ep(i-1) - sar(i-1) ) * afv(i-1)
c
c           Current buy signal
c
            if( sig(i) .EQ. 1 ) then
c
c                   Determine new acceleration factor vector value
c
                if( ep(i) .GT. ep(i-1) ) then
                    if( afv(i-1) .EQ. maf ) then
                        afv(i) = maf
                    else
                        afv(i) = af + afv(i-1)
                    endif
                else
                    afv(i) = afv(i-1)
                endif
c
c               Determine sar vector value
c
                if( sar(i) .GT. lmin ) then
                    sar(i) = lmin
                endif
c
c           Current sell signal
c
            else
                if( ep(i) .LT. ep(i-1) ) then
                    if( afv(i-1) .EQ. maf ) then
                        afv(i) = maf
                    else
                        afv(i) = af + afv(i-1)
                    endif
                else
                    afv(i) = afv(i-1)
                endif
c
c               Determine sar vector value
c
                if( sar(i) .LT. lmax ) then
                    sar(i) = lmax
                endif
            endif
c
c       New signal
c
        else
            afv(i) = af
            sar(i) = ep(i-1)
        endif

   10 continue
      end
