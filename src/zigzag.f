c-----------------------------------------------------------------------c
c TTR, copyright (C) Joshua M. Ulrich, 2007                             c
c Distributed under GNU GPL version 3                                   c
c-----------------------------------------------------------------------c

c
c     Zig Zig
c
c     iha   : input array of high prices
c     ila   : input array of low prices
c     la    : length of arrays
c     ch    : minimum change to cause a zig/zag
c     pct   : logical, % (1) or $ (0) change
c     rtr   : logical, retrace (1) or absolute (0) change
c     lex   : logical, last (1) or first (0) extreme value
c     zz    : Zig/Zag vector
c
c     refpos: Reference (first) price position
c     refval: Reference (first) price value
c     infpos: Inflection (second) price position
c     infval: Inflection (second) price value
c     *min  : [L]ocal and [E]xtreme minimums
c     *max  : [L]ocal and [E]xtreme maximums
c
      subroutine zigzag(iha, ila, la, ch, pct, rtr, lex, zz)
      implicit none

      integer la, pct, rtr, lex, i, refpos, infpos, sig
      double precision iha(la), ila(la), zz(la)
      double precision refval, infval, ch, one, two
      double precision lmin, lmax, emin, emax
c
c     Initialize values
c
      one = (iha(1) + ila(1)) / 2
      two = (iha(2) + ila(2)) / 2
      refval = one
      infval = two
      refpos = 1
      infpos = 2
      sig = 0
c
c     Begin Loop
c
      do 10 i=2,la

      if( pct .EQ. 1 ) then
c     If % change given (absolute move)
          emin = infval * ( 1 - ch )
          emax = infval * ( 1 + ch )
      else
c     If $ change given (only absolute moves make sense)
          emin = infval - ch
          emax = infval + ch
      endif
c
c     Find local maximum and minimum
c
      lmax = MAX( infval, iha(i) )
      lmin = MIN( infval, ila(i) )
c
c     Find first trend
c
      if( sig .EQ. 0 ) then
          if( rtr .EQ. 1 ) then
c         Retrace prior move
              if( two .GE. one ) then
                  sig = 1
              else
                  sig = -1
              endif
          else
c         Absolute move
              if( lmin .LE. emin ) then
c             Confirmed Downtrend
                  sig = -1
              endif
              if( lmax .GE. emax ) then
c             Confirmed Uptrend
                  sig = 1
              endif
          endif
      endif
c
c     Downtrend
c
      if( sig .EQ. -1 ) then
c
c         New Minimum
c
          if( ila(i) .EQ. lmin ) then
c             Last Extreme
              if( lex .EQ. 1 ) then
                  infval = ila(i)
                  infpos = i
              else
c             First Extreme
                  if( ila(i) .NE. ila(i-1) ) then
                      infval = ila(i)
                      infpos = i
                  endif
              endif
          endif
c
c         Retrace prior move
c
          if( rtr .EQ. 1 ) then
              emax = infval + ((refval - infval) * ch)
          endif
c
c         Trend Reversal
c
          if( iha(i) .GE. emax ) then
              zz(refpos) = refval
              refval = infval
              refpos = infpos
              infval = iha(i)
              infpos = i
              sig = 1
          endif
      endif
c
c     Uptrend
c
      if( sig .EQ. 1 ) then
c
c         New Maximum
c
          if( iha(i) .EQ. lmax ) then
c             Last Extreme
              if( lex .EQ. 1 ) then
                  infval = iha(i)
                  infpos = i
              else
c             First Extreme
                  if( iha(i) .NE. iha(i-1) ) then
                      infval = iha(i)
                      infpos = i
                  endif
              endif
          endif
c
c         Retrace prior move
c
          if( rtr .EQ. 1 ) then
              emin = infval - ((infval - refval) * ch)
          endif
c
c         Trend Reversal
c
          if( ila(i) .LE. emin ) then
              zz(refpos) = refval
              refval = infval
              refpos = infpos
              infval = ila(i)
              infpos = i
              sig = -1
          endif
      endif

   10 continue
c
c     Set final values
c     
      zz(refpos) = refval
      zz(infpos) = infval
      end
