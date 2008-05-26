#-------------------------------------------------------------------------#
# TTR, copyright (C) Joshua M. Ulrich, 2007                               #
# Distributed under GNU GPL version 3                                     #
#-------------------------------------------------------------------------#

"runSum" <-
function(x, n=10) {

  x <- use.xts(x, error=FALSE)

  if( n < 1 || n > NROW(x) ) stop("Invalid 'n'")

  # Count NAs, ensure they're only at beginning of data.
  NAs <- sum(is.na(x))
  if( NAs > 0 ) {
    if( any( is.na(x[-(1:NAs)]) ) ) stop("Series contains non-leading NAs")
  }
  beg <- 1 + NAs
  len <- NROW(x) - NAs

  # Initialize result vector 
  result <- double(NROW(x))
  result[(n+beg-1)] <- sum(x[beg:(n+beg-1)])

  # Call Fortran routine
  result <- .Fortran( "runsum",
                   ia = as.double(x[beg:NROW(x)]),
                   lia = as.integer(len),
                   n = as.integer(n),
                   oa = as.double(result[beg:NROW(x)]),
                   loa = as.integer(len),
                   PACKAGE = "TTR" )$oa

  # Replace 1:(n-1) with NAs and prepend NAs from original data
  is.na(result) <- c(1:(n-1))
  result <- c( rep( NA, NAs ), result )
  
  # Convert back to original class
  reclass(result, x)
}

#-------------------------------------------------------------------------#

"wilderSum" <-
function(x, n=10) {

  x <- use.xts(x, error=FALSE)

  if( n < 1 || n > NROW(x) ) stop("Invalid 'n'")

  # Count NAs, ensure they're only at beginning of data, then remove.
  NAs <- sum( is.na(x) )
  if( NAs > 0 ) {
    if( any( is.na(x[-(1:NAs)]) ) ) stop("Series contains non-leading NAs")
  }
  beg <- 1 + NAs
  len <- NROW(x) - NAs

  result <- .Fortran( "wilder",
                   ia  = as.double(x[beg:NROW(x)]),
                   lia = as.integer(len),
                   n   = as.integer(n),
                   oa  = as.double(x[beg:NROW(x)]),
                   loa = as.integer(len),
                   PACKAGE = "TTR" )$oa

  # Replace 1:(n-1) with NAs and prepend NAs from original data
  is.na(result) <- c(1:(n-1))
  result <- c( rep( NA, NAs ), result )

  # Convert back to original class
  reclass(result, x)
}

#-------------------------------------------------------------------------#

"runMin" <-
function(x, n=10) {

  x <- use.xts(x, error=FALSE)

  if( n < 1 || n > NROW(x) ) stop("Invalid 'n'")

  # Count NAs, ensure they're only at beginning of data, then remove.
  NAs <- sum( is.na(x) )
  if( NAs > 0 ) {
    if( any( is.na(x[-(1:NAs)]) ) ) stop("Series contains non-leading NAs")
  }
  beg <- 1 + NAs
  len <- NROW(x) - NAs

  # Initialize result vector 
  result <- double(NROW(x))
  result[(n+beg-1)] <- min(x[beg:(n+beg-1)])

  result <- .Fortran( "runmin",
                   ia = as.double(x[beg:NROW(x)]),
                   lia = as.integer(len),
                   n = as.integer(n),
                   oa = as.double(result[beg:NROW(x)]),
                   loa = as.integer(len),
                   PACKAGE = "TTR" )$oa

  # Replace 1:(n-1) with NAs and prepend NAs from original data
  is.na(result) <- c(1:(n-1))
  result <- c( rep( NA, NAs ), result )

  # Convert back to original class
  reclass(result, x)
}

#-------------------------------------------------------------------------#

"runMax" <-
function(x, n=10) {

  x <- use.xts(x, error=FALSE)
  
  if( n < 1 || n > NROW(x) ) stop("Invalid 'n'")

  # Count NAs, ensure they're only at beginning of data, then remove.
  NAs <- sum( is.na(x) )
  if( NAs > 0 ) {
    if( any( is.na(x[-(1:NAs)]) ) ) stop("Series contains non-leading NAs")
  }
  beg <- 1 + NAs
  len <- NROW(x) - NAs

  # Initialize result vector 
  result <- double(NROW(x))
  result[(n+beg-1)] <- max(x[beg:(n+beg-1)])

  result <- .Fortran( "runmax",
                   ia = as.double(x[beg:NROW(x)]),
                   lia = as.integer(len),
                   n = as.integer(n),
                   oa = as.double(result[beg:NROW(x)]),
                   loa = as.integer(len),
                   PACKAGE = "TTR" )$oa

  # Replace 1:(n-1) with NAs and prepend NAs from original data
  is.na(result) <- c(1:(n-1))
  result <- c( rep( NA, NAs ), result )

  # Convert back to original class
  reclass(result, x)
}

#-------------------------------------------------------------------------#

"runMean" <-
function(x, n=10) {

  result <- runSum(x, n) / n

  return(result)
}

#-------------------------------------------------------------------------#

"runMedian" <-
function(x, n=10, non.unique="mean") {

  x <- use.xts(x, error=FALSE)

  if( n < 1 || n > NROW(x) ) stop("Invalid 'n'")

  # Count NAs, ensure they're only at beginning of data, then remove.
  NAs <- sum( is.na(x) )
  if( NAs > 0 ) {
    if( any( is.na(x[-(1:NAs)]) ) ) stop("Series contains non-leading NAs")
  }
  beg <- 1 + NAs
  len <- NROW(x) - NAs

  # Non-unique median
  non.unique <- match.arg(non.unique, c('mean','max','min'))
  non.unique <- switch( non.unique, mean=0, max=1, min=-1 )
  
  # Call Fortran routine
  result <- .Fortran( "runmedian",
                   ia = as.double(x[beg:NROW(x)]),
                   n = as.integer(n),
                   oa = double(len),
                   la = as.integer(len),
                   ver = as.integer(non.unique),
                   PACKAGE = "TTR" )$oa

  # Replace 1:(n-1) with NAs and prepend NAs from original data
  is.na(result) <- c(1:(n-1))
  result <- c( rep( NA, NAs ), result )

  # Convert back to original class
  reclass(result, x)
}

#-------------------------------------------------------------------------#

"runCov" <-
function(x, y, n=10, use="all.obs", sample=TRUE) {

  x <- use.xts(x, error=FALSE)
  y <- use.xts(y, error=FALSE)
  if(is.xts(x) && is.xts(y)) {
    xy <- cbind(x,y)
  } else {
    xy <- cbind( as.vector(x), as.vector(y) )
  }

  if( n < 1 || n > NROW(x) ) stop("Invalid 'n'")

  # "all.obs", "complete.obs", "pairwise.complete.obs"

  # Count NAs, ensure they're only at beginning of data, then remove.
  xNAs <- sum( is.na(x) )
  yNAs <- sum( is.na(y) )
  NAs <- max( xNAs, yNAs )
  if( NAs > 0 ) {
    if( any( is.na(xy[-(1:NAs),]) ) ) stop("Series contain non-leading NAs")
  }
  beg <- 1 + NAs
  len <- NROW(xy) - NAs
  
  xCenter <- runSum(x, n)/n
  xCenter[1:(NAs+n-1)] <- 0
  yCenter <- runSum(y, n)/n
  yCenter[1:(NAs+n-1)] <- 0

  # Call Fortran routine
  result <- .Fortran( "runCov",
                   rs1 = as.double(x[beg:NROW(xy)]),
                   avg1 = as.double(xCenter[beg:NROW(xy)]),
                   rs2 = as.double(y[beg:NROW(xy)]),
                   avg2 = as.double(yCenter[beg:NROW(xy)]),
                   la = as.integer(len),
                   n = as.integer(n),
                   samp = as.integer(sample),
                   oa = double(len),
                   PACKAGE = "TTR" )$oa

  # Replace 1:(n-1) with NAs and prepend NAs from original data
  is.na(result) <- c(1:(n-1))
  result <- c( rep( NA, NAs ), result )

  # Convert back to original class
  # Should the attributes of *both* x and y be retained?
  reclass(result, x)
}

#-------------------------------------------------------------------------#

"runCor" <-
function(x, y, n=10, use="all.obs", sample=TRUE) {

  result <- runCov(x, y, n, use=use, sample=sample ) /
            ( runSD(x, n, sample=sample) * runSD(y, n, sample=sample) )

  return( result )
}

#-------------------------------------------------------------------------#

"runVar" <-
function(x, n=10, sample=TRUE) {

  result <- runCov(x, x, n, use="all.obs", sample=sample)

  return( result )
}

#-------------------------------------------------------------------------#

"runSD" <-
function(x, n=10, sample=TRUE) {

  result <- sqrt( runCov(x, x, n, use="all.obs", sample=sample) )

  return( result )
}

#-------------------------------------------------------------------------#

"runMAD" <-
function(x, n=10, center=runMedian(x, n), stat="median",
         constant=1.4826, non.unique="mean") {

  x <- use.xts(x, error=FALSE)

  if( n < 1 || n > NROW(x) ) stop("Invalid 'n'")

  # Count NAs, ensure they're only at beginning of data, then remove.
  NAs <- sum( is.na(x) )
  if( NAs > 0 ) {
    if( any( is.na(x[-(1:NAs)]) ) ) stop("Series contains non-leading NAs")
  }
  beg <- 1 + NAs
  len <- NROW(x) - NAs
  center[1:(NAs+n-1)] <- 0

  # Mean or Median absolute deviation?
  median <- match.arg(stat, c("mean","median"))
  median <- switch( stat, median=TRUE, mean=FALSE )

  # Non-unique median
  non.unique <- match.arg(non.unique, c('mean','max','min'))
  non.unique <- switch( non.unique, mean=0, max=1, min=-1 )
  
  # Call Fortran routine
  result <- .Fortran( "runMAD",
                   rs = as.double(x[beg:NROW(x)]),      # raw series
                   cs = as.double(center[beg:NROW(x)]), # center series
                   la = as.integer(len),                # length of input arrays
                   n = as.integer(n),                   # size of rolling window
                   oa = double(len),                    # output array
                   stat = as.integer(median),           # center statistic
                   ver = as.integer(non.unique),        # median type
                   PACKAGE = "TTR" )$oa

  if( median ) result <- result * constant

  # Replace 1:(n-1) with NAs and prepend NAs from original data
  is.na(result) <- c(1:(n-1))
  result <- c( rep( NA, NAs ), result )

  # Convert back to original class
  reclass(result, x)
}
