c-----------------------------------------------------------------------c
c TTR, copyright (C) Joshua M. Ulrich, 2007                             c
c Distributed under GNU GPL version 3                                   c
c-----------------------------------------------------------------------c

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

      integer lia, n, loa, i
      double precision ia(lia), oa(loa)

      do 10 i=n+1,lia

        oa(i) = oa(i-1) + ia(i) - ia(i-n)

   10 continue
      end
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

      integer lia, n, loa, i
      double precision ia(lia), oa(loa)

      do 10 i=2,lia

        oa(i) = ia(i) + oa(i-1) * (n-1)/n

   10 continue
      end
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
c     Calculate a running/rolling SUM of PRODUCTS
c     (Not very elegant, but _much_ faster
c     than a pure R implementation)
c
c     ia1  : input array #1
c     ia2  : input array #2
c     lia  : length of input arrays
c     n    : size of window
c     oa   : output array
c     loa  : length of output array
c     prod : window product
c
      subroutine runSP(ia1, ia2, lia, n, oa, loa)

      integer lia, n, loa, i, j
      double precision ia1(lia), ia2(lia), oa(loa)
      
      do 10 i=n,lia

        prod = ia1(i) * ia2(i)

        do 20 j=i-n+1,i-1
          
          prod = prod + ( ia1(j) * ia2(j) )

   20 continue

        oa(i) = prod

   10 continue
      end
