c
c-----------------------------------------------------------------------c
c
c     Calculate a running/rolling percent rank
c
c     ia   : input array
c     lia  : length of input array
c     n    : size of window
c     xmlt : exact value multiplier 
c     oa   : output array
c
      subroutine runprnk(ia, lia, n, xmlt, oa)
      implicit none

      integer lia, n, i, j
      double precision ia(lia), oa(lia), xmlt, nless 

      do 10 i=n,lia
       
        nless = xmlt

        do 20 j=i-n+1,i-1
          
          if (ia(j) < ia(i)) then
            nless = nless + 1
          else if (ia(j) == ia(i)) then 
            nless = nless + xmlt
          end if
          
   20 continue

        oa(i) = nless / n

   10 continue
      end

c
c-----------------------------------------------------------------------c
c
c     Calculate a cumulative percent rank
c
c     ia   : input array
c     lia  : length of input array
c     xmlt : exact value multiplier 
c     oa   : output array
c
      subroutine cumprnk(ia, lia, xmlt, oa)
      implicit none

      integer lia, i, j
      double precision ia(lia), oa(lia), xmlt, nless

      do 10 i=2,lia
       
        nless = xmlt

        do 20 j=1,i-1
          
          if (ia(j) < ia(i)) then
            nless = nless + 1
          else if (ia(j) == ia(i)) then 
            nless = nless + xmlt
          end if
          
   20 continue

        oa(i) = nless / i

   10 continue
      end
