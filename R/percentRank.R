#
#   TTR: Technical Trading Rules
#
#   Copyright (C) 2007-2013  Joshua M. Ulrich
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 2 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

#'Percent Rank over a Moving Window
#'
#'This function computes a running/rolling percentage rank.
#'
#'The computation for a percentage rank can vary depending on the weight given
#'to values in the window that are equal to the value being ranked. This weight
#'can be set using the \code{exact.multiplier} argument which defaults to 0.5.
#'
#'\code{exact.multiplier = 0} scores equal values in the lookback window as
#'always being greater than the value being ranked. \code{exact.multiplier = 1}
#'scores equal values as being below the value being ranked. Any multiplier
#'between 0 and 1 counts that proportion of the equal values as being below
#'the value being ranked.
#'
#'The value of \code{exact.multiplier} has the most impact when the window is
#'relatively small or when the number of discrete values in the window is
#'small. For non-repeating values, changing \code{exact.multiplier = 0} to
#'\code{exact.multiplier = 1} for a window of size \code{N} will shift the
#'resulting percentile rankings by \code{1/N}. It is equivalent to changing
#'the question from, "how many values are < the value" to "how many values
#'are <= the value".
#'
#'@aliases runPercentRank percentRank PercentRank
#'@param x Object coercible to xts or matrix.
#'@param n Number of periods to use in the window or, if
#'\code{cumulative=TRUE}, the number of observations to use before the first
#'result is returned. Must be between 1 and \code{nrow(x)}, inclusive.
#'@param cumulative Logical, use from-inception calculation?
#'@param exact.multiplier The weight applied to identical values in the window.
#'Must be between 0 and 1, inclusive. See details.
#'
#'@return A object of percent ranks over a n-period moving window of the same
#'class as \code{x} and \code{y} or a vector (if \code{try.xts} fails).
#'
#'@note This computation is different from the one used in Microsoft Excel's
#'\code{PERCENTRANK} formula. Excel's computation is rather strange and gives
#'inconsistent results as it uses interpolation to rank values that are not
#'found within the lookback window.
#'
#'@author Charlie Friedemann
#'
#'@references The following site(s) were used to code/document this
#'indicator:\cr \url{https://en.wikipedia.org/wiki/Percentile_rank}\cr
#'
#'@keywords ts
runPercentRank <- function(x, n=260, cumulative = FALSE, exact.multiplier = 0.5) {
  x <- try.xts(x, error = as.matrix)

  if (n < 1 || n > NROW(x))
    stop(sprintf("n = %d is outside valid range: [1, %d]", n, NROW(x)))
  if (exact.multiplier < 0 || exact.multiplier > 1)
    stop(sprintf("exact.multiplier = %d is outside valid range: [0, 1]", exact.multiplier))

  NAs <- sum(is.na(x))
  if (NAs > 0) {
    if (any(is.na(x[-(1:NAs)]))) stop("Series contains non-leading NAs")
  }

  if (!isTRUE(cumulative) && identical(as.integer(n), 1L)) {
    result <- double(NROW(x))
    result[] <- exact.multiplier
  } else {
    result <- .Call(C_ttr_rollPercentRank, x, n, isTRUE(cumulative),
                    exact.multiplier)
  }

  reclass(result, x)
}
