"KST" <-
function(price, n = c(10, 15, 20, 30), ma1 = list("SMA", n=10), ma2 = ma1, ma3 = ma1, 
         ma4 = list("SMA", n=15), ma.sig = list("SMA", n=10), wts = 1:4) {

  # Know Sure Thing

  # Technical Analysis Explained: The Successful Investor's Guide to Spotting Investment Trends and Turning Points
  # Martin J. Pring
  # http://www.pring.com/index.html
  # Daily:	http://www.pring.com/movieweb/daily_kst.htm
  # Long-Term:	http://www.pring.com/articles/article28.htm

  # Daily KST
  # MA(ROC(10)10) + MA(ROC(15)10) + MA(ROC(20)10) + MA(ROC(30)15)
  #
  # Intermediate KST
  # MA(ROC(10)10) + MA(ROC(13)13) + MA(ROC(15)15) + MA(ROC(20)20)
  #
  # Long-Term Monthly KST
  # MA(ROC(9)6) + MA(ROC(12)6) + MA(ROC(18)6) + MA(ROC(24)9)

  if(NROW(n) != NROW(wts))  stop("'n' and 'wts' must be the same length.")

  price <- as.vector(price)

  ret <- NULL

  for(i in 1:NROW(n)) {
    mai <- get(paste("ma",i,sep=""))
    roc <- do.call( mai[[1]], c( list( ROC(price, n[i], na=0) ), mai[-1] ) ) * wts[i]
    ret <- cbind( ret, roc )
  }

  kst    <- 100 * rowSums(ret)
  signal <- do.call( ma.sig[[1]], c( list(kst), ma.sig[-1] ) )

  return( cbind( kst, signal ) )
}

