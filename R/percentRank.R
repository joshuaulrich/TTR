
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