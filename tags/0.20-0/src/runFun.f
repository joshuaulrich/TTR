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
c     Calculate a running/rolling SUM
c
c     ia  : input array
c     lia : length of input array
c     n   : size of window
c     oa  : output array
c     loa : length of output array
c
      subroutine runsum(ia, lia, n, oa, loa)
      implicit none

      integer lia, n, loa, i
      double precision ia(lia), oa(loa)

      do 10 i=n+1,lia

        oa(i) = oa(i-1) + ia(i) - ia(i-n)

   10 continue
      end
c
c-----------------------------------------------------------------------c
c
c     Calculate a Welles Wilder SUM
c
c     ia  : input array
c     lia : length of input array
c     n   : size of window
c     oa  : output array
c     loa : length of output array
c
      subroutine wilder(ia, lia, n, oa, loa)
      implicit none

      integer lia, n, loa, i
      double precision ia(lia), oa(loa)

      do 10 i=2,lia

        oa(i) = ia(i) + oa(i-1) * (n-1)/n

   10 continue
      end
c
c-----------------------------------------------------------------------c
c
c     Calculate a running/rolling MIN
c     (Not very elegant, but _much_ faster
c     than a pure R implementation)
c
c     ia   : input array
c     lia  : length of input array
c     n    : size of window
c     oa   : output array
c     loa  : length of output array
c     lmin : window minimum
c
      subroutine runmin(ia, lia, n, oa, loa)
      implicit none

      integer lia, n, loa, i, j
      double precision ia(lia), oa(loa), lmin

      do 10 i=n,lia
        
        lmin = ia(i)

        do 20 j=i-n+1,i-1
          
          lmin = min( ia(j), lmin )

   20 continue

        oa(i) = lmin

   10 continue
      end
c
c-----------------------------------------------------------------------c
c
c     Calculate a running/rolling MAX
c     (Not very elegant, but _much_ faster
c     than a pure R implementation)
c
c     ia   : input array
c     lia  : length of input array
c     n    : size of window
c     oa   : output array
c     loa  : length of output array
c     lmax : window maximum
c
      subroutine runmax(ia, lia, n, oa, loa)
      implicit none

      integer lia, n, loa, i, j
      double precision ia(lia), oa(loa), lmax

      do 10 i=n,lia
        
        lmax = ia(i)

        do 20 j=i-n+1,i-1
          
          lmax = max( ia(j), lmax )

   20 continue

        oa(i) = lmax

   10 continue
      end
c
c-----------------------------------------------------------------------c
c
c     Calculate a running/rolling MEDIAN
c     (Not very elegant, but _much_ faster
c     than a pure R implementation)
c
c     ia   : input array
c     lia  : length of input array
c     n    : size of window
c     oa   : output array
c     la   : length of output array
c     win  : window
c     lmed : local median
c     ver  : median type to use if 'n' is even (min, avg, max)
c     cu   : cumulative flag
c
      subroutine runmedian(ia, n, oa, la, ver, cu)
      implicit none

      integer n, la, i, j, k, mid, flag, ver, cu
      double precision ia(la), oa(la), win(la), lmed

      do 10 i=n,la
        
        if( cu .EQ. 1 ) n = i
        j = i-n+1

        do 20 k=1,n

          win(k) = ia(j+k-1)

   20 continue

        call sort( win, n )
        flag = n-(n/2)*2
        mid = n/2
        lmed = win(mid+1)
        if( flag .EQ. 0 ) then
            if( ver .LT. 0 ) lmed = min( win(mid) , win(mid+1) )
            if( ver .EQ. 0 ) lmed = ( win(mid) + win(mid+1) )/2.0D0
            if( ver .GT. 0 ) lmed = max( win(mid) , win(mid+1) )
        endif
        oa(i) = lmed

   10 continue
      end
c
c---------------------------------------------------------------------
c
c     Calculate a running/rolling mean/median Absolute Deviation
c     (Not very elegant, but _much_ faster
c     than a pure R implementation)
c
c     rs  : input - raw series
c     cs  : input - center series
c     la  : length of input arrays
c     n   : size of window
c     oa  : output array
c     ad  : absolute deviation
c     stat: statistic to calculate (0=mean, 1=median)
c     ver  : median type to use if 'n' is even (min, avg, max)
c
      subroutine runMAD(rs, cs, la, n, oa, stat, ver, cu)
      implicit none

      integer la, n, i, j, k, l, mid, stat, ver, flag, cu
      double precision rs(la), cs(la), oa(la), win(la), ad
      
      do 10 i=n,la
        
        if( cu .EQ. 1 ) n = i
        j = i-n+1

        do 20 k=1,n

          win(k) = abs( rs(j+k-1) - cs(i) )

   20 continue

        if( stat .EQ. 1 ) then
          call sort( win, n )
          flag = n-(n/2)*2
          mid = n/2
          ad = win(mid+1)
          if( flag .EQ. 0 ) then
            if( ver .LT. 0 ) ad = min( win(mid) , win(mid+1) )
            if( ver .EQ. 0 ) ad = ( win(mid) + win(mid+1) )/2.0D0
            if( ver .GT. 0 ) ad = max( win(mid) , win(mid+1) )
          endif
        else
          ad = win(1) / n
          do 30 l=2,n
            ad = ad + win(l) / n
   30 continue
        endif
          oa(i) = ad

   10 continue
      end
c
c---------------------------------------------------------------------
c
c     Calculate a running/rolling Covariance
c
c     rs1   : input - raw series (1)
c     avg1  : input - average of rs1
c     rs2   : input - raw series (2)
c     avg2  : input - average of rs2
c     la    : length of input arrays
c     n     : size of window
c     oa    : output array
c     cov   : covariance
c     samp  : sample (1=true)
c
      subroutine runCov(rs1, avg1, rs2, avg2, la, n, samp, oa, cu)
      implicit none

      integer la, n, i, j, k, l, samp, cu
      double precision rs1(la), avg1(la), rs2(la), avg2(la)
      double precision oa(la), cov
      
      do 10 i=n,la

        if( cu .EQ. 1 ) n = i
        j = i-n+1

        cov = 0.0D0

        do 20 k=1,n
          
          l = j+k-1
          cov = ( rs1(l)-avg1(i) ) * ( rs2(l)-avg2(i) ) + cov

   20 continue
          
        if( samp .EQ. 1 ) then
          oa(i) = cov / (n-1)
        else
          oa(i) = cov / n
        endif

   10 continue
      end
c
c---------------------------------------------------------------------
c
c     v : input array
c     n : number of observations
c     a : array of indicies of v in ascending order
c
c     Puts into a the permutation vector which sorts v into
c     increasing order.  Only elements from ii to jj are considered.
c     Arrays iu(k) and il(k) permit sorting up to 2**(k+1)-1 elements
c
c     This is a modification of CACM algorithm #347 by R. C. Singleton,
c     which is a modified Hoare quicksort.
c
      subroutine sort(v, n)
      implicit none

      integer i, j, k, l, m, n
      integer a(n), ij, il(20), iu(20), t, tt
      double precision v(n)
      double precision vt, vtt

      m=1
      i=1
      j=n
 10   if (i.ge.j) go to 80
 20   k=i
      ij=(j+i)/2
      t=a(ij)
      vt=v(ij)
      if (v(i).le.vt) go to 30
      a(ij)=a(i)
      a(i)=t
      t=a(ij)
      v(ij)=v(i)
      v(i)=vt
      vt=v(ij)
 30   l=j
      if (v(j).ge.vt) go to 50
      a(ij)=a(j)
      a(j)=t
      t=a(ij)
      v(ij)=v(j)
      v(j)=vt
      vt=v(ij)
      if (v(i).le.vt) go to 50
      a(ij)=a(i)
      a(i)=t
      t=a(ij)
      v(ij)=v(i)
      v(i)=vt
      vt=v(ij)
      go to 50
 40   a(l)=a(k)
      a(k)=tt
      v(l)=v(k)
      v(k)=vtt
 50   l=l-1
      if (v(l).gt.vt) go to 50
      tt=a(l)
      vtt=v(l)
 60   k=k+1
      if (v(k).lt.vt) go to 60
      if (k.le.l) go to 40
      if (l-i.le.j-k) go to 70
      il(m)=i
      iu(m)=l
      i=k
      m=m+1
      go to 90
 70   il(m)=k
      iu(m)=j
      j=l
      m=m+1
      go to 90
 80   m=m-1
      if (m.eq.0) return
      i=il(m)
      j=iu(m)
 90   if (j-i.gt.10) go to 20
      if (i.eq.1) go to 10
      i=i-1
 100  i=i+1
      if (i.eq.j) go to 80
      t=a(i+1)
      vt=v(i+1)
      if (v(i).le.vt) go to 100
      k=i
 110  a(k+1)=a(k)
      v(k+1)=v(k)
      k=k-1
      if (vt.lt.v(k)) go to 110
      a(k+1)=t
      v(k+1)=vt
      go to 100
      end
