#-------------------------------------------------------------------------#
# TTR, copyright (C) Joshua M. Ulrich, 2007                               #
# Distributed under GNU GPL version 3                                     #
#-------------------------------------------------------------------------#

"runSum" <-
function(x, n=10) {

  x <- use.xts(x, error=FALSE)

  if( n < 1 || n > NROW(x) ) stop("Invalid 'n'")

  # Count NAs, ensure they're only at beginning of data, then remove.
  NAs <- sum( is.na(x) )
  if( NAs > 0 ) {
    if( any( is.na(x[-(1:NAs)]) ) ) stop("Series contains non-leading NAs")
    x <- x[-(1:NAs)]
  }

  # Initialize result vector 
  result <- rep(0,NROW(x))
  result[n] <- sum(x[1:n])

  # Call Fortran routine
  result <- .Fortran( "runsum",
                   ia = as.double(x),
                   lia = as.integer(NROW(x)),
                   n = as.integer(n),
                   oa = as.double(result),
                   loa = as.integer(NROW(result)),
                   PACKAGE = "TTR" )$oa

  # Replace 1:(n-1) with NAs and prepend NAs from original data
  result[1:(n-1)] <- NA
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
    x <- x[-(1:NAs)]
  }

  # Initialize result vector 
  result <- x

  result <- .Fortran( "wilder",
                   ia  = as.double(x),
                   lia = as.integer(NROW(x)),
                   n   = as.integer(n),
                   oa  = as.double(result),
                   loa = as.integer(NROW(result)),
                   PACKAGE = "TTR" )$oa

  # Replace 1:(n-1) with NAs and prepend NAs from original data
  result[1:(n-1)] <- NA
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
    x <- x[-(1:NAs)]
  }

  # Initialize result vector 
  result <- rep(0,NROW(x))
  result[n] <- min(x[1:n])

  result <- .Fortran( "runmin",
                   ia = as.double(x),
                   lia = as.integer(NROW(x)),
                   n = as.integer(n),
                   oa = as.double(result),
                   loa = as.integer(NROW(result)),
                   PACKAGE = "TTR" )$oa

  # Replace 1:(n-1) with NAs and prepend NAs from original data
  result[1:(n-1)] <- NA
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
    x <- x[-(1:NAs)]
  }

  # Initialize result vector 
  result <- rep(0,NROW(x))
  result[n] <- max(x[1:n])

  result <- .Fortran( "runmax",
                   ia = as.double(x),
                   lia = as.integer(NROW(x)),
                   n = as.integer(n),
                   oa = as.double(result),
                   loa = as.integer(NROW(result)),
                   PACKAGE = "TTR" )$oa

  # Replace 1:(n-1) with NAs and prepend NAs from original data
  result[1:(n-1)] <- NA
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
    x <- x[-(1:NAs)]
  }

  # Non-unique median
  non.unique <- match.arg(non.unique, c('mean','max','min'))
  non.unique <- switch( non.unique, mean=0, max=1, min=-1 )
  
  # Initialize result vector 
  result <- rep(0,NROW(x))

  # Call Fortran routine
  result <- .Fortran( "runmedian",
                   ia = as.double(x),
                   n = as.integer(n),
                   oa = double(NROW(x)),
                   la = as.integer(NROW(x)),
                   ver = as.integer(non.unique),
                   PACKAGE = "TTR" )$oa

  # Replace 1:(n-1) with NAs and prepend NAs from original data
  result[1:(n-1)] <- NA
  result <- c( rep( NA, NAs ), result )

  # Convert back to original class
  reclass(result, x)
}

#-------------------------------------------------------------------------#

"runCov" <-
function(x, y, n=10, use="all.obs", sample=TRUE) {

  xy <- cbind( as.vector(x), as.vector(y) )

  if( n < 1 || n > NROW(x) ) stop("Invalid 'n'")

  # "all.obs", "complete.obs", "pairwise.complete.obs"

  # Count NAs, ensure they're only at beginning of data, then remove.
  xNAs <- sum( is.na(x) )
  yNAs <- sum( is.na(y) )
  NAs <- max( xNAs, yNAs )
  if( NAs > 0 ) {
    if( any( is.na(xy[-(1:NAs),]) ) ) stop("Series contain non-leading NAs")
    xy <- xy[-(1:NAs)]
  }
  
  xCenter <- runSum(x, n)/n
  xCenter[1:(n-1)] <- 0
  yCenter <- runSum(y, n)/n
  yCenter[1:(n-1)] <- 0

  # Initialize result vector 
  result <- rep(0,NROW(xy))

  # Call Fortran routine
  result <- .Fortran( "runCov",
                   rs1 = as.double(x),
                   avg1 = as.double(xCenter),
                   rs2 = as.double(y),
                   avg2 = as.double(yCenter),
                   la = as.integer(NROW(x)),
                   n = as.integer(n),
                   samp = as.integer(sample),
                   oa = as.double(result),
                   PACKAGE = "TTR" )$oa

  # Replace 1:(n-1) with NAs and prepend NAs from original data
  result[1:(n-1)] <- NA
  result <- c( rep( NA, NAs ), result )

  return( result )
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
    x <- x[-(1:NAs)]
  }
  center[1:(n-1)] <- 0

  # Initialize result vector 
  result <- rep(0,NROW(x))

  # Mean or Median absolute deviation?
  median <- match.arg(stat, c("mean","median"))
  median <- switch( stat, median=TRUE, mean=FALSE )

  # Non-unique median
  non.unique <- match.arg(non.unique, c('mean','max','min'))
  non.unique <- switch( non.unique, mean=0, max=1, min=-1 )
  
  # Call Fortran routine
  result <- .Fortran( "runMAD",
                   rs = as.double(x),
                   cs = as.double(center),
                   la = as.integer(NROW(x)),
                   n = as.integer(n),
                   oa = as.double(result),
                   stat = as.integer(median),
                   ver = as.integer(non.unique),
                   PACKAGE = "TTR" )$oa

  if( median ) result <- result * constant

  # Replace 1:(n-1) with NAs and prepend NAs from original data
  result[1:(n-1)] <- NA
  result <- c( rep( NA, NAs ), result )

  # Convert back to original class
  reclass(result, x)
}
