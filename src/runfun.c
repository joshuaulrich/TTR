/*
 *  TTR: Technical Trading Rules
 *
 *  Copyright (C) 2007-2018  Joshua M. Ulrich
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

#include <string.h>  /* for memcpy */
#include "ttr.h"

SEXP runsum(SEXP _x, SEXP _n)
{
  int i, P = 0;

  /* ensure that 'x' is double */
  if (TYPEOF(_x) != REALSXP) {
    _x = PROTECT(coerceVector(_x, REALSXP)); P++;
  }
  double *x = REAL(_x);
  int n = asInteger(_n);

  /* Input object length */
  int nr = nrows(_x);

  /* Initalize result R object */
  SEXP _result = PROTECT(allocVector(REALSXP, nr)); P++;
  double *result = REAL(_result);

  /* check for non-leading NAs and get first non-NA location */
  SEXP _first = PROTECT(xts_na_check(_x, ScalarLogical(TRUE))); P++;
  int first = INTEGER(_first)[0];
  if (n + first > nr) {
    error("not enough non-NA values");
  }

  /* Set leading NAs in output */
  for (i = 0; i < first; i++) {
    result[i] = NA_REAL;
  }

  /* Raw sum to start running sum */
  double seed = 0.0;
  for (i = first; i < first + n; i++) {
    result[i] = NA_REAL;
    seed += x[i];
  }
  result[first + n - 1] = seed;

  /* Loop over non-NA input values */
  for (i = first + n; i < nr; i++) {
    result[i] = result[i-1] + x[i] - x[i-n];
  }

  /* UNPROTECT R objects and return result */
  UNPROTECT(P);
  return _result;
}

SEXP runmin(SEXP _x, SEXP _n)
{
  int i, j, P = 0;

  /* ensure that 'x' is double */
  if (TYPEOF(_x) != REALSXP) {
    _x = PROTECT(coerceVector(_x, REALSXP)); P++;
  }
  double *x = REAL(_x);
  int n = asInteger(_n);

  /* Input object length */
  int nr = nrows(_x);

  /* Initalize result R object */
  SEXP _result = PROTECT(allocVector(REALSXP, nr)); P++;
  double *result = REAL(_result);

  /* check for non-leading NAs and get first non-NA location */
  SEXP _first = PROTECT(xts_na_check(_x, ScalarLogical(TRUE))); P++;
  int first = INTEGER(_first)[0];
  if (n + first > nr) {
    error("not enough non-NA values");
  }

  /* Set leading NAs in output */
  for (i = 0; i < first; i++) {
    result[i] = NA_REAL;
  }

  /* start running min */
  double lmin = x[first];
  for (i = first; i < first + n; i++) {
    result[i] = NA_REAL;
    if (x[i] < lmin) {
      lmin = x[i];
    }
  }
  result[first + n - 1] = lmin;

  /* Loop over non-NA input values */
  for (i = first + n; i < nr; i++) {
    lmin = x[i];
    for (j = 1; j < n; j++) {
      if (x[i-j] < lmin) {
        lmin = x[i-j];
      }
    }
    result[i] = lmin;
  }

  /* UNPROTECT R objects and return result */
  UNPROTECT(P);
  return _result;
}

SEXP runmax(SEXP _x, SEXP _n)
{
  int i, j, P = 0;

  /* ensure that 'x' is double */
  if (TYPEOF(_x) != REALSXP) {
    _x = PROTECT(coerceVector(_x, REALSXP)); P++;
  }
  double *x = REAL(_x);
  int n = asInteger(_n);

  /* Input object length */
  int nr = nrows(_x);

  /* Initalize result R object */
  SEXP _result = PROTECT(allocVector(REALSXP, nr)); P++;
  double *result = REAL(_result);

  /* check for non-leading NAs and get first non-NA location */
  SEXP _first = PROTECT(xts_na_check(_x, ScalarLogical(TRUE))); P++;
  int first = INTEGER(_first)[0];
  if (n + first > nr) {
    error("not enough non-NA values");
  }

  /* Set leading NAs in output */
  for (i = 0; i < first; i++) {
    result[i] = NA_REAL;
  }

  /* start running max */
  double lmax = x[first];
  for (i = first; i < first + n; i++) {
    result[i] = NA_REAL;
    if (x[i] > lmax) {
      lmax = x[i];
    }
  }
  result[first + n - 1] = lmax;

  /* Loop over non-NA input values */
  for (i = first + n; i < nr; i++) {
    lmax = x[i];
    for (j = 1; j < n; j++) {
      if (x[i-j] > lmax) {
        lmax = x[i-j];
      }
    }
    result[i] = lmax;
  }

  /* UNPROTECT R objects and return result */
  UNPROTECT(P);
  return _result;
}

typedef double (*tiebreaker)(const double, const double);

static inline double
tiebreaker_lt(const double a, const double b)
{
  return (a < b) ? a : b;
}
static inline double
tiebreaker_gt(const double a, const double b)
{
  return (a > b) ? a : b;
}
static inline double
tiebreaker_eq(const double a, const double b)
{
  return (a + b) / 2.0;
}

static inline double
ttr_median(double *x, int n, tiebreaker tie_func)
{
  int flag = n-2*(n/2);
  int mid = n/2-1;
  R_qsort(x, 1, n);

  double median = (flag) ? x[mid+1] : tie_func(x[mid] , x[mid+1]);
  return median;
}

SEXP runmedian(SEXP _x, SEXP _n, SEXP _tiebreak, SEXP _cumulative)
{
  int i, P = 0;

  /* ensure that 'x' is double */
  if (TYPEOF(_x) != REALSXP) {
    _x = PROTECT(coerceVector(_x, REALSXP)); P++;
  }
  double *x = REAL(_x);
  int n = asInteger(_n);
  int tiebreak = asInteger(_tiebreak);
  if (TYPEOF(_cumulative) != LGLSXP) {
    _cumulative = PROTECT(coerceVector(_cumulative, LGLSXP)); P++;
  }
  int cumulative = asLogical(_cumulative);

  /* Input object length */
  int nr = nrows(_x);

  /* Initalize result R object */
  SEXP _result = PROTECT(allocVector(REALSXP, nr)); P++;
  double *result = REAL(_result);

  /* check for non-leading NAs and get first non-NA location */
  SEXP _first = PROTECT(xts_na_check(_x, ScalarLogical(TRUE))); P++;
  int first = INTEGER(_first)[0];
  if (n + first > nr) {
    error("not enough non-NA values");
  }

  /* Set leading NAs in output */
  for (i = 0; i < first + n; i++) {
    result[i] = NA_REAL;
  }

  tiebreaker tie_func = NULL;
  if (tiebreak == 0) {
    tie_func = tiebreaker_eq;
  }
  else if (tiebreak < 0) {
    tie_func = tiebreaker_lt;
  }
  else if (tiebreak > 0) {
    tie_func = tiebreaker_gt;
  }

  SEXP _window;
  double *window;
  int first_i = first + n - 1;

  if (cumulative) {
    _window = PROTECT(duplicate(_x)); P++;
    window = REAL(_window);

    for (i = first_i; i < nr; i++) {
      result[i] = ttr_median(window, i, tie_func);
    }
  } else {
    _window = PROTECT(allocVector(REALSXP, n)); P++;
    window = REAL(_window);

    for (i = first_i; i < nr; i++) {
      memcpy(window, &x[i-n+1], n * sizeof(double));
      result[i] = ttr_median(window, n, tie_func);
    }
  }

  /* UNPROTECT R objects and return result */
  UNPROTECT(P);
  return _result;
}

static inline double
ttr_mean(double *x, int n)
{
  double mean = x[0] / n;
  int i;
  for (i = 1; i < n; i++) {
    mean += x[i] / n;
  }
  return mean;
}

SEXP runmad(SEXP _x, SEXP _center, SEXP _n, SEXP _type,
    SEXP _tiebreak, SEXP _cumulative)
{
  int i, j, P = 0;

  /* ensure 'x' and 'center' are double */
  if (TYPEOF(_x) != REALSXP) {
    _x = PROTECT(coerceVector(_x, REALSXP)); P++;
  }
  if (TYPEOF(_center) != REALSXP) {
    _center = PROTECT(coerceVector(_center, REALSXP)); P++;
  }
  double *x = REAL(_x);
  double *center = REAL(_center);
  int n = asInteger(_n);
  int type = asInteger(_type);
  int tiebreak = asInteger(_tiebreak);
  if (TYPEOF(_cumulative) != LGLSXP) {
    _cumulative = PROTECT(coerceVector(_cumulative, LGLSXP)); P++;
  }
  int cumulative = asLogical(_cumulative);

  /* Input object length */
  int nr = nrows(_x);
  if (nr != nrows(_center)) {
    error("'x' and 'center' must have the same number of observations");
  }

  /* Initalize result R object */
  SEXP _result = PROTECT(allocVector(REALSXP, nr)); P++;
  double *result = REAL(_result);

  /* check for non-leading NAs and get first non-NA location */
  SEXP _first = PROTECT(xts_na_check(_x, ScalarLogical(TRUE))); P++;
  int first = INTEGER(_first)[0];
  if (n + first > nr) {
    error("not enough non-NA values in 'x'");
  }

  /* Set leading NAs in output */
  for (i = 0; i < first + n; i++) {
    result[i] = NA_REAL;
  }

  tiebreaker tie_func = NULL;
  if (tiebreak == 0) {
    tie_func = tiebreaker_eq;
  }
  else if (tiebreak < 0) {
    tie_func = tiebreaker_lt;
  }
  else if (tiebreak > 0) {
    tie_func = tiebreaker_gt;
  }

  SEXP _window;
  double *window;
  int first_i = first + n - 1;

  if (cumulative) {
    _window = PROTECT(duplicate(_x)); P++;
    window = REAL(_window);

    if (type) {
      for (i = first_i; i < nr; i++) {
        for (j = 0; j < i; j++) {
          window[j] = fabs(x[i-j] - center[i]);
        }
        result[i] = ttr_median(window, i, tie_func);
      }
    } else {
      for (i = first_i; i < nr; i++) {
        for (j = 0; j < i; j++) {
          window[j] = fabs(x[i-j] - center[i]);
        }
        result[i] = ttr_mean(window, i);
      }
    }
  } else {
    _window = PROTECT(allocVector(REALSXP, n)); P++;
    window = REAL(_window);

    if (type) {
      for (i = first_i; i < nr; i++) {
        for (j = 0; j < n; j++) {
          window[j] = fabs(x[i-j] - center[i]);
        }
        result[i] = ttr_median(window, n, tie_func);
      }
    } else {
      for (i = first_i; i < nr; i++) {
        for (j = 0; j < n; j++) {
          window[j] = fabs(x[i-j] - center[i]);
        }
        result[i] = ttr_mean(window, n);
      }
    }
  }

  /* UNPROTECT R objects and return result */
  UNPROTECT(P);
  return _result;
}
