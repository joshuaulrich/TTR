#'Percent Rank over a Moving Window
#'
#'This function computes a running/rolling percentage rank.
#'
#'The computation for a percentage rank can vary depending on the weight given
#'to values in the window identical to the value being ranked.  This weight can
#'be set using the \code{exact.multiplier} argument which defaults to 0.5.
#'
#'@aliases runPercentRank percentRank PercentRank
#'@param x Object coercible to xts or matrix.
#'@param n Number of periods to use in the window or, if
#'\code{cumulative=TRUE}, the number of obversations to use before the first
#'result is returned.
#'@param cumulative Logical, use from-inception calculation?
#'@param exact.multiplier The weight applied to identical values in the window.
#'See details.
#'@return A object of percent ranks over a n-period moving window of the same
#'class as \code{x} and \code{y} or a vector (if \code{try.xts} fails).
#'@note It may be important to note that this computation is different from the
#'one used in Microsoft Excel's PERCENTRANK formula.  Excel's computation is
#'rather strange and gives inconsistent results as it uses interpolation to
#'rank values that are not found within the lookback window.
#'@author Charlie Friedemann
#'@references The following site(s) were used to code/document this
#'indicator:\cr \url{http://en.wikipedia.org/wiki/Percentile_rank}\cr
#'@keywords ts
runPercentRank <- function(x, n=260, cumulative = FALSE, exact.multiplier = 0.5) {
	x <- try.xts(x, error = as.matrix)
	
	if (n < 1 || n > NROW(x)) stop("Invalid 'n'")
	if (0 > exact.multiplier || exact.multiplier > 1) stop("Invalid 'exact.multiplier'")
		
	NAs <- sum(is.na(x))
	if (NAs > 0) {
		if (any(is.na(x[-(1:NAs)]))) stop("Series contains non-leading NAs")
	}
	beg <- 1 + NAs
	
	len <- NROW(x) - NAs
	result <- double(NROW(x))
	
	if (cumulative) {
		result <- .Fortran("cumprnk", ia = as.double(x[beg:NROW(x)]), lia = as.integer(len), 
							xmlt = as.double(exact.multiplier), oa = as.double(result[beg:NROW(x)]), 
							PACKAGE = "TTR", DUP = FALSE)$oa
	} else if (identical(as.integer(n),1L)) {
		result[] <- exact.multiplier
	} else {
		result <- .Fortran("runprnk", ia = as.double(x[beg:NROW(x)]), lia = as.integer(len), 
							n = as.integer(n), xmlt = as.double(exact.multiplier), 
							oa = as.double(result[beg:NROW(x)]), PACKAGE = "TTR", DUP = FALSE)$oa
		is.na(result) <- c(1:(n - 1))
	}
	result <- c(rep(NA, NAs), result)
	reclass(result, x)
}
