/*
 *  TTR: Technical Trading Rules
 *
 *  Copyright (C) 2007-2013  Joshua M. Ulrich
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

#include "ttr.h"

SEXP ema (SEXP x, SEXP n, SEXP ratio, SEXP wilder) {
    
    /* Initialize loop and PROTECT counters */
    int i, P=0;

    /* ensure that 'x' is double */
    if(TYPEOF(x) != REALSXP) {
      PROTECT(x = coerceVector(x, REALSXP)); P++;
    }
    double *d_x = REAL(x);

    if(ncols(x) > 1) {
      error("ncol(x) > 1; EMA only supports univariate 'x'");
    }

    int i_n = asInteger(n);
    double d_ratio = asReal(ratio);

    if(R_NilValue == n || i_n <= 0) {
      if(R_NilValue == ratio || d_ratio <= 0.0) {
        error("either 'n' or 'ratio' must be specified and > 0\n",
              "'n' is ", n, " 'ratio' is ", ratio);
      } else {
        /* If ratio is specified, and n is not, set n to approx 'correct'
         * value backed out from ratio */
        i_n  = (int)(2.0 / d_ratio - 1.0);
      }
    } else {
      /* Determine decay ratio */
      if(R_NilValue == ratio) {
        int isWilder = asInteger(wilder);
        d_ratio = (isWilder) ? 1.0 / i_n : 2.0 / (i_n + 1);
      } else {
        /* ratio != NULL -> warn that 'n' will be used instead */
        warning("both 'n' and 'ratio' are specified; using 'n'");
      }
    }

    /* Input object length */
    int nr = nrows(x);

    /* Initialize result R object */
    SEXP result;
    PROTECT(result = allocVector(REALSXP,nr)); P++;
    double *d_result = REAL(result);

    /* check for non-leading NAs and get first non-NA location */
    SEXP _first = PROTECT(xts_na_check(x, ScalarLogical(TRUE))); P++;
    int first = INTEGER(_first)[0];
    if(i_n + first > nr) {
      error("not enough non-NA values");
    }

    /* Set leading NAs in output */
    for(i = 0; i < first; i++) {
      d_result[i] = NA_REAL;
    }

    /* Raw mean to start EMA */
    double seed = 0.0;
    for(i = first; i < first + i_n; i++) {
      d_result[i] = NA_REAL;
      seed += d_x[i] / i_n;
    }
    d_result[first + i_n - 1] = seed;

    /* Loop over non-NA input values */
    for(i = first + i_n; i < nr; i++) {
        d_result[i] = d_x[i] * d_ratio + d_result[i-1] * (1-d_ratio);
    }

    /* UNPROTECT R objects and return result */
    UNPROTECT(P);
    return(result);
}

SEXP evwma (SEXP pr, SEXP vo, SEXP n) {
    
    /* Initialize loop and PROTECT counters */
    int i, P=0;

    /* ensure that 'pr' is double */
    if(TYPEOF(pr) != REALSXP) {
      PROTECT(pr = coerceVector(pr, REALSXP)); P++;
    }
    /* ensure that 'vo' is double */
    if(TYPEOF(vo) != REALSXP) {
      PROTECT(vo = coerceVector(vo, REALSXP)); P++;
    }

    /* Pointers to function arguments */
    double *d_pr = REAL(pr);
    double *d_vo = REAL(vo);
    int i_n = asInteger(n);
    
    /* Input object length */
    int nr = nrows(pr);

    /* Initialize result R object */
    SEXP result;
    PROTECT(result = allocVector(REALSXP,nr)); P++;
    double *d_result = REAL(result);

    /* check for non-leading NAs and get first non-NA location */
    SEXP _first_pr = PROTECT(xts_na_check(pr, ScalarLogical(TRUE))); P++;
    int first_pr = asInteger(_first_pr);
    if(i_n + first_pr > nr) {
      error("not enough non-NA values in 'price'");
    }
    SEXP _first_vo = PROTECT(xts_na_check(vo, ScalarLogical(TRUE))); P++;
    int first_vo = asInteger(_first_vo);
    if(i_n + first_vo > nr) {
      error("not enough non-NA values in 'volume'");
    }

    int first = first_pr > first_vo ? first_pr : first_vo;
    int begin = first + i_n - 1;

    /* Set leading NAs in output */
    for(i = 0; i < begin; i++) {
      d_result[i] = NA_REAL;
    }

    /* First non-NA result is the first non-NA value of 'x' */
    d_result[begin] = d_pr[begin];

    /* Initialize volume sum */
    double volSum = 0.0;
    for(i = first; i < begin+1; i++) {
      volSum += d_vo[i];
    }

    /* Loop over the rest of the values */
    for(i = begin + 1; i < nr; i++) {
        volSum = volSum + d_vo[i] - d_vo[i-i_n];
        d_result[i] = ((volSum-d_vo[i])*d_result[i-1]+d_vo[i]*d_pr[i])/volSum;
    }

    /* UNPROTECT R objects and return result */
    UNPROTECT(P);
    return(result);
}

SEXP wma (SEXP x, SEXP w, SEXP n) {

    /* Initialize loop and PROTECT counters */
    int i, j, P=0;

    /* ensure that 'x' is double */
    if(TYPEOF(x) != REALSXP) {
      PROTECT(x = coerceVector(x, REALSXP)); P++;
    }
    /* ensure that 'w' is double */
    if(TYPEOF(w) != REALSXP) {
      PROTECT(w = coerceVector(w, REALSXP)); P++;
    }
    int i_n = asInteger(n);

    /* Pointers to function arguments */
    double *d_x = REAL(x);
    double *d_w = REAL(w);

    /* Input object length */
    int nr = nrows(x);

    /* Initialize result R object */
    SEXP result;
    PROTECT(result = allocVector(REALSXP,nr)); P++;
    double *d_result = REAL(result);

    /* check for non-leading NAs and get first non-NA location */
    SEXP _first = PROTECT(xts_na_check(x, ScalarLogical(TRUE))); P++;
    int first = INTEGER(_first)[0];
    if(i_n + first > nr) {
      error("not enough non-NA values");
    }

    int begin = first + i_n - 1;
    /* Set leading NAs in output */
    for(i = 0; i < begin; i++) {
      d_result[i] = NA_REAL;
    }

    /* Sum of weights (w does not have NA) */
    double wtsum = 0.0;
    for(j = 0; j < i_n; j++) {
      if(ISNA(d_w[j])) {
          error("wts cannot contain NA");
      }
      wtsum += d_w[j];
    }

    /* Loop over non-NA input values */
    for(i = begin; i < nr; i++) {
      double num = 0.0;
      int ni = i - i_n + 1;
      for(j = 0; j < i_n; j++) {
        num += d_x[ni+j] * d_w[j];
      }
      d_result[i] = num / wtsum;
    }

    /* UNPROTECT R objects and return result */
    UNPROTECT(P);
    return(result);
}

SEXP zlema (SEXP x, SEXP n, SEXP ratio) {

    /* Initialize loop and PROTECT counters */
    int i, P=0;

    /* ensure that 'x' is double */
    if(TYPEOF(x) != REALSXP) {
      PROTECT(x = coerceVector(x, REALSXP)); P++;
    }
    double *d_x = REAL(x);

    if(ncols(x) > 1) {
      error("ncol(x) > 1; ZLEMA only supports univariate 'x'");
    }

    int i_n = asInteger(n);
    double d_ratio = asReal(ratio);

    if(R_NilValue == n || i_n <= 0) {
      if(R_NilValue == ratio || d_ratio <= 0.0) {
        error("either 'n' or 'ratio' must be specified and > 0\n",
              "'n' is ", n, " 'ratio' is ", ratio);
      } else {
        /* If ratio is specified, and n is not, set n to approx 'correct'
         * value backed out from ratio */
        i_n  = (int)(2.0 / d_ratio - 1.0);
      }
    } else {
      /* Determine decay ratio */
      if(R_NilValue == ratio) {
        d_ratio = 2.0 / (i_n + 1);
      } else {
        /* ratio != NULL -> warn that 'n' will be used instead */
        warning("both 'n' and 'ratio' are specified; using 'n'");
      }
    }

    /* Input object length */
    int nr = nrows(x);

    /* Initialize result R object */
    SEXP result;
    PROTECT(result = allocVector(REALSXP,nr)); P++;
    double *d_result = REAL(result);

    /* check for non-leading NAs and get first non-NA location */
    SEXP _first = PROTECT(xts_na_check(x, ScalarLogical(TRUE))); P++;
    int first = INTEGER(_first)[0];
    if(i_n + first > nr) {
      error("not enough non-NA values");
    }

    /* Set leading NAs in output */
    for(i = 0; i < first; i++) {
      d_result[i] = NA_REAL;
    }

    /* Raw mean to start EMA */
    double seed = 0.0;
    for(i = first; i < first + i_n; i++) {
      d_result[i] = NA_REAL;
      seed += d_x[i] / i_n;
    }
    d_result[first + i_n - 1] = seed;

    double lag = 1.0 / d_ratio;
    double wt = fmod(lag, 1.0);
    double w1 = 1.0 - wt;
    double r1 = 1.0 - d_ratio;

    /* Loop over non-NA input values */
    for(i = first + i_n; i < nr; i++) {
      int loc = (int)(i - lag);
      double value = 2 * d_x[i] - (w1 * d_x[loc] + wt * d_x[loc+1]);
      d_result[i] = d_ratio * value + r1 * d_result[i-1];
    }

    /* UNPROTECT R objects and return result */
    UNPROTECT(P);
    return(result);
}
