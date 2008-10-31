c-----------------------------------------------------------------------c
c TTR, copyright (C) Joshua M. Ulrich, 2007                             c
c Distributed under GNU GPL version 3                                   c
c-----------------------------------------------------------------------c

c     Calculate Simple Moving Average (SMA)
c
c http://www.fmlabs.com/reference/SimpleMA.htm
c http://www.equis.com/Customer/Resources/TAAZ/Default.aspx?c=3&p=74
c http://linnsoft.com/tour/techind/movAvg.htm
c http://stockcharts.com/education/IndicatorAnalysis/indic_movingAvg.html
c 
c     http://en.wikipedia.org/wiki/Moving_average
c     I think Wikipedia is wrong. The MA shouldn't have
c     anything to do with _tomorrow's_ value.
c
c      subroutine sma(ia, lia, n, oa, loa)
c
c      integer lia, n, loa, i
c      double precision ia(lia), oa(loa)
c
c      do 10 i=n+1,lia
c
c        oa(i) = oa(i-1) + (ia(i) - ia(i-n))/n
c      
c   10 continue
c      end

c-----------------------------------------------------------------------c
c
c     Calculate Exponential Moving Average (EMA)
c
c http://www.fmlabs.com/reference/ExpMA.htm
c http://www.equis.com/Customer/Resources/TAAZ/Default.aspx?c=3&p=74
c http://linnsoft.com/tour/techind/movAvg.htm
c http://stockcharts.com/education/IndicatorAnalysis/indic_movingAvg.html
c
c     Thanks to Jeff Ryan for the Fortran implementation,
c     and more importantly, the encouragement!
c
c     ia    : input array
c     lia   : length of input array
c     n     : number of periods
c     oa    : output array
c     loa   : length of output array
c     ratio : weighting/decay ratio
c
      subroutine ema(ia, lia, n, oa, loa, ratio)

      integer lia, n, loa, i
      double precision ia(lia), oa(loa)
      double precision ratio

      do 10 i=n+1,lia

        oa(i) = ia(i) * ratio + oa(i-1) * (1-ratio)

   10 continue
      end

c-----------------------------------------------------------------------c
c
c     Calculate Weighted Moving Average (WMA)
c
c http://www.fmlabs.com/reference/WeightedMA.htm
c http://www.equis.com/Customer/Resources/TAAZ/Default.aspx?c=3&p=74
c http://linnsoft.com/tour/techind/movAvg.htm
c
c     Only for the case of wts(n). If wts(lia), the
c     WMA can be calculated with 'runSP' ('runSumProd' in R).
c
c     ia   : input array
c     lia  : length of input array
c     wts  : weight array
c     n    : number of periods (= length of weight array)
c     oa   : output array
c     loa  : length of output array
c     num  : window numerator
c     den  : window denominator
c
      subroutine wma(ia, lia, wts, n, oa, loa)

      integer lia, n, loa, i, j
      double precision ia(lia), wts(n), oa(loa)
      double precision num, den
      
      do 10 i=n,lia

         num = 0.0D0
         den = 0.0D0

        do 20 j=i-n+1,i
          
          num = num + ( ia(j) * wts(n-i+j) )
          den = den + wts(n-i+j)

   20 continue

        oa(i) = num / den

   10 continue
      end

c-----------------------------------------------------------------------c
c
c     Calculate Elastic, Volume-Weighted Moving Average (EVWMA)
c
c http://linnsoft.com/tour/techind/evwma.htm
c  
c     ip    : input array, price
c     iv    : input array, volume
c     ivs   : input array, volume sum
c     lia   : length of input arrays
c     n     : number of periods
c     oa    : output array
c     loa   : length of output array
c
      subroutine evwma(ip, iv, ivs, lia, n, oa, loa)

      integer lia, n, loa, i
      double precision ip(lia), iv(lia), ivs(lia), oa(loa)

      do 10 i=n+1,lia

        oa(i) = ( (ivs(i) - iv(i)) * oa(i-1) + iv(i) * ip(i) ) / ivs(i)

   10 continue
      end

c-----------------------------------------------------------------------c
c
c     Calculate Zero Lag Exponential Moving Average (ZLEMA)
c
c http://www.fmlabs.com/reference/ZeroLagExpMA.htm
c http://linnsoft.com/tour/techind/movAvg.htm
c
c     The above sites do not account for the possibility of 'lag' being
c     non-integer.  I implement a weighted average of the two nearest
c     'ia' values if 'lag' is non-integer.
c
c     ia    : input array
c     lia   : length of input array
c     n     : number of periods
c     oa    : output array
c     loa   : length of output array
c     ratio : weighting/decay ratio
c
      subroutine zlema(ia, lia, n, oa, loa, ratio)

      integer lia, n, loa, i, loc 
      double precision ia(lia), oa(loa)
      double precision ratio, lag, wt

      lag = 1/ratio
      wt = DMOD( lag, 1.0D0 )

      do 10 i=n+1,lia

        loc = DINT(i-(lag))
        oa(i) = ratio * ( 2*ia(i) - (ia(loc)*(1-wt) + ia(loc+1)*wt ) )
     C         + ( 1 - ratio ) * oa(i-1)

   10 continue
      end
