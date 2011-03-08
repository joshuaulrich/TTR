DVI <- function(price, n=252, wts=c(0.8,0.2), smooth=3,
  magnitude=c(5,100,5), stretch=c(10,100,2)) {

  # Implementation of David Varadi's DVI indicator, based on:
  # http://marketsci.wordpress.com/2010/07/27/css-analytics%E2%80%99-dvi-indicator-revealed/

  # try to convert 'price' to xts
  price <- try.xts(price, error=as.matrix)

  # ensure magnitude + stretch = 1
  wts.sum <- sum(wts)
  wts[1] <- wts[1] / wts.sum
  wts[2] <- wts[2] / wts.sum

  # calculate magnitude, based on average price return
  r <- price/SMA(price,smooth)-1
  mag <- SMA( ( SMA(r,magnitude[1]) + SMA(r,magnitude[2])/10 )/2, magnitude[3] )

  # calculate stretch, based on whether return is +/-
  b <- ifelse( price > lag(price), 1, -1 )
  str <- SMA( ( runSum(b,stretch[1]) + runSum(b,stretch[2])/10 )/2, stretch[3] )

  # A simple percent rank function, possibly different
  # than Excel's percentrank function.
  pctRank <- function(x,i) match(x[i], sort(coredata(x[(i-(n-1)):i])))

  # calculate the DVI magnitude and stretch for each period
  dvi.mag <- dvi.str <- rep(NA,NROW(price))
  for(i in n:NROW(price)) {
    dvi.mag[i] <- pctRank(mag, i) / n
    dvi.str[i] <- pctRank(str, i) / n
  }

  # calculate final DVI value
  dvi <- wts[1] * dvi.mag + wts[2] * dvi.str

  # convert final DVI, magnitude, and stretch back to
  # original class of 'price'
  reclass(cbind(dvi.mag,dvi.str,dvi), price)
}

