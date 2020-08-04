/*
 *  TTR: Technical Trading Rules
 *
 *  Copyright (C) 2012-2017  Charlie Friedemann, Joshua M. Ulrich
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <R.h>
#include <Rinternals.h>

double calc_n_less(double* x, double mult, int i, int j1)
{
  double n_less = mult;
  /* Loop over window */
  for (int j = j1; j < i; j++) {
    double diff = x[j] - x[i];
    if (diff < 0) {
      n_less = n_less + 1.0;
    } else if (fabs(diff) < 1e-8) {
      n_less = n_less + mult;
    }
  }
  return n_less;
}

/* Calculate a running/rolling percent rank,
 * or a cumulative version */
SEXP ttr_rollPercentRank(SEXP _x, SEXP _n, SEXP _cumul, SEXP _mult)
{
  int i, P = 0;

  /* ensure correct types */
  if (TYPEOF(_x) != REALSXP) {
    PROTECT(_x = coerceVector(_x, REALSXP)); P++;
  }
  double *d_x = REAL(_x);
  int n = asInteger(_n);
  int cumul = asLogical(_cumul);
  double mult = asReal(_mult);

  int nr = nrows(_x);

  /* Initialize result R object */
  SEXP result;
  PROTECT(result = allocVector(REALSXP, nr)); P++;
  double *d_result = REAL(result);

  /* Find first non-NA input value */
  int beg = n - 1;
  for (i = 0; i <= beg; i++) {
    /* Account for leading NAs in input */
    if (ISNA(d_x[i])) {
      d_result[i] = NA_REAL;
      beg++;
      continue;
    }
    /* Set leading NAs in output */
    if (i < beg) {
      d_result[i] = NA_REAL;
    }
  }

  /* Loop over non-NA input values */
  if (cumul) {
    d_result[beg] = mult;
    for (i = beg+1; i < nr; i++) {
      double n_less = calc_n_less(d_x, mult, i, 0);
      d_result[i] = n_less / (i + 1);
    }
  } else {
    for (i = beg; i < nr; i++) {
      double n_less = calc_n_less(d_x, mult, i, i-n+1);
      d_result[i] = n_less / n;
    }
  }

  UNPROTECT(P);
  return(result);
}
