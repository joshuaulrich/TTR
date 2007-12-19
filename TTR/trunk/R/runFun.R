#-------------------------------------------------------------------------#
# TTR, copyright (C) Joshua M. Ulrich, 2007                               #
# Distributed under GNU GPL version 3                                     #
#-------------------------------------------------------------------------#

"runSum" <-
function(x, n=10) {

  x   <- as.vector(x)

  if( n < 1 | n > NROW(x) ) stop("Invalid 'n'")

  # Count NAs, ensure they're only at beginning of data, then remove.
  NAs <- sum( is.na(x) )
  if( NAs > 0 ) {
    if( any( is.na(x[-(1:NAs)]) ) ) stop("Series contains non-leading NAs")
  }
  x   <- na.omit(x)

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

  return( result )
}

#-------------------------------------------------------------------------#

"wilderSum" <-
function(x, n=10) {

  x   <- as.vector(x)

  if( n < 1 | n > NROW(x) ) stop("Invalid 'n'")

  # Count NAs, ensure they're only at beginning of data, then remove.
  NAs <- sum( is.na(x) )
  if( NAs > 0 ) {
    if( any( is.na(x[-(1:NAs)]) ) ) stop("Series contains non-leading NAs")
  }
  x   <- na.omit(x)

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

  return( result )
}

#-------------------------------------------------------------------------#

"runMin" <-
function(x, n=10) {

  x   <- as.vector(x)

  if( n < 1 | n > NROW(x) ) stop("Invalid 'n'")

  # Count NAs, ensure they're only at beginning of data, then remove.
  NAs <- sum( is.na(x) )
  if( NAs > 0 ) {
    if( any( is.na(x[-(1:NAs)]) ) ) stop("Series contains non-leading NAs")
  }
  x   <- na.omit(x)

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

  return( result )
}

#-------------------------------------------------------------------------#

"runMax" <-
function(x, n=10) {

  x   <- as.vector(x)
  
  if( n < 1 | n > NROW(x) ) stop("Invalid 'n'")

  # Count NAs, ensure they're only at beginning of data, then remove.
  NAs <- sum( is.na(x) )
  if( NAs > 0 ) {
    if( any( is.na(x[-(1:NAs)]) ) ) stop("Series contains non-leading NAs")
  }
  x   <- na.omit(x)

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

  return( result )
}

#-------------------------------------------------------------------------#

"runVar" <-
function(x, n=10, sample=TRUE) {

  x <- as.double( as.vector(x) )

  result <- runSum(x*x, n)/n - (runSum(x, n)/n)^2

  if(sample) {
    result <- result * n/(n+1)
  }

  return(result)
}
