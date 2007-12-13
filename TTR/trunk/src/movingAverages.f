c
c     Exponential Moving Average
c     Thanks to Jeff Ryan for the Fortran implementation.
c
      subroutine ema(n,ma,x,ratio,lenma,lenx)
      
      integer lenx, lenma, n, i
      double precision ma(lenma), x(lenx)
      double precision ratio

      do 20 i=(n+1),lenx

        ma(i) = x(i) * ratio + ma(i-1) * (1-ratio)

   20 continue
      end

c
c     Simple Moving Average
c     http://en.wikipedia.org/wiki/Moving_average
c     I think Wikipedia is wrong. The MA shouldn't have
c     anything to do with _tomorrow's_ value.
c
      subroutine sma(n,ma,x,lenma,lenx)

      integer lenx, lenma, n, i
      double precision ma(lenma), x(lenx)

      do 20 i=(n+1),lenx

        ma(i) = ma(i-1) + (x(i) - x(i-n))/n

   20 continue
      end

