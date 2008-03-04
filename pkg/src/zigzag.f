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
c     minch : minimum change to cause a zig/zag
c     ch    : logical % (1) or $ (0) change?
c     minch : The minimum change amount
c     zz    : Zig/Zag vector
c     refpos: Reference (first) price position
c     refval: Reference (first) price value
c     infpos: Inflection (second) price position
c     infval: Inflection (second) price value
c     Xmin  : [L]ocal and [E]xtreme minimums
c     Xmax  : [L]ocal and [E]xtreme maximums
c
      subroutine zigzag(iha, ila, la, minch, ch, zz)
      implicit none

      integer la, i, ch, refpos, infpos, sig
      double precision iha(la), ila(la), zz(la)
      double precision refval, infval, minch
      double precision lmin, lmax, emin, emax
c
c     Initialize values
c
      refval = (iha(1) + ila(1)) / 2
      infval = refval
      refpos = 1
      infpos = 1
      sig = 0
c
c     Begin Loop
c
      do 10 i=2,la

      if( ch .EQ. 1 ) then
c     If % change given
          emin = infval * ( 1 - minch )
          emax = infval * ( 1 + minch )
      else
c     If $ change given
          emin = infval - minch
          emax = infval + minch
      endif
c
c     Find local maximum and minimum
c
      lmax = MAX( infval, iha(i) )
      lmin = MIN( infval, ila(i) )
c
c     Find first trend
      if( sig .EQ. 0 ) then
c         Confirmed Downtrend
          if( lmin .LE. emin ) then
              sig = -1
          endif
c         Confirmed Uptrend
          if( lmax .GE. emax ) then
              sig = 1
          endif
      endif
c
c     Downtrend
c
      if( sig .EQ. -1 ) then
c
c         New Minimum
c
          if( ila(i) .LE. lmin ) then
              infval = lmin
              infpos = i
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
          if( iha(i) .GE. lmax ) then
              infval = lmax
              infpos = i
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
      end
