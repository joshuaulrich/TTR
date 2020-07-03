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
    
    /* Initalize loop and PROTECT counters */
    int i, P=0;

    /* ensure that 'x' is double */
    if(TYPEOF(x) != REALSXP) {
      PROTECT(x = coerceVector(x, REALSXP)); P++;
    }
    double *d_x = REAL(x);

    /* If ratio is specified, and n is not, set n to approx 'correct'
     * value backed out from ratio
     */
    int i_n;
    if(R_NilValue == n && R_NilValue != ratio) {
      i_n = (int)(2.0 / asReal(ratio) - 1.0);
    } else {
      i_n = asInteger(n);
    }

    /* Determine decay ratio */
    int isWilder = LOGICAL(wilder)[0];
    double d_ratio;
    if(R_NilValue == ratio) {
      d_ratio = (isWilder) ? 1.0 / i_n : 2.0 / (i_n + 1);
    } else {
      d_ratio = asReal(ratio);
    }

    /* Input object length */
    int nr = nrows(x);

    /* Initalize result R object */
    SEXP result;
    PROTECT(result = allocVector(REALSXP,nr)); P++;
    double *d_result = REAL(result);

    /* check for non-leading NAs and get first non-NA location */
    SEXP _first = PROTECT(xts_na_check(x, ScalarLogical(TRUE))); P++;
    int first = INTEGER(_first)[0];
    if(i_n + 1 + first > nr) {
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
    
    /* Initalize loop and PROTECT counters */
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

    /* Initalize result R object */
    SEXP result;
    PROTECT(result = allocVector(REALSXP,nr)); P++;
    double *d_result = REAL(result);

    /* Volume Sum */
    double volSum = 0;

    /* Find first non-NA input value */
    int beg = i_n - 1;
    for(i = 0; i <= beg; i++) {
        /* Account for leading NAs in input */
        if(ISNA(d_pr[i]) || ISNA(d_vo[i])) {
            d_result[i] = NA_REAL;
            beg++;
            continue;
        }
        /* Set leading NAs in output */
        if(i < beg) {
            d_result[i] = NA_REAL;
        /* First result value is first price value */
        } else {
            d_result[i] = d_pr[i];
        }
        /* Keep track of volume Sum */
        volSum += d_vo[i];
    }

    /* Loop over non-NA input values */
    for(i = beg+1; i < nr; i++) {
        volSum = volSum + d_vo[i] - d_vo[i-i_n];
        d_result[i] = ((volSum-d_vo[i])*d_result[i-1]+d_vo[i]*d_pr[i])/volSum;
    }

    /* UNPROTECT R objects and return result */
    UNPROTECT(P);
    return(result);
}

SEXP vma (SEXP x, SEXP w, SEXP ratio) {
    
    /* Initalize loop and PROTECT counters */
    int i, P=0;

    /* ensure that 'x' is double */
    if(TYPEOF(x) != REALSXP) {
      PROTECT(x = coerceVector(x, REALSXP)); P++;
    }
    /* ensure that 'w' is double */
    if(TYPEOF(w) != REALSXP) {
      PROTECT(w = coerceVector(w, REALSXP)); P++;
    }

    /* Pointers to function arguments */
    double *d_x = REAL(x);
    double *d_w = REAL(w);
    double d_ratio = asReal(ratio);
    
    /* Input object length */
    int nr = nrows(x);

    /* Initalize result R object */
    SEXP result;
    PROTECT(result = allocVector(REALSXP,nr)); P++;
    double *d_result = REAL(result);

    /* Find first non-NA input value */
    int beg = 0;
    d_result[beg] = 0;
    for(i = 0; i <= beg; i++) {
        /* Account for leading NAs in input */
        if(ISNA(d_x[i]) || ISNA(d_w[i])) {
            d_result[i] = NA_REAL;
            beg++;
            d_result[beg] = 0;
            continue;
        }
        /* Set leading NAs in output */
        if(i < beg) {
            d_result[i] = NA_REAL;
        }
        /* Raw mean to start VMA */
        d_result[beg] += d_x[i];
    }

    /* Loop over non-NA input values */
    for(i = beg+1; i < nr; i++) {
        d_result[i] = d_x[i] * d_w[i] * d_ratio + d_result[i-1] * (1-d_ratio*d_w[i]);
    }

    /* UNPROTECT R objects and return result */
    UNPROTECT(P);
    return(result);
}

SEXP wma (SEXP x, SEXP w, SEXP n) {

    /* Initalize loop and PROTECT counters */
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

    /* Initalize result R object */
    SEXP result;
    PROTECT(result = allocVector(REALSXP,nr)); P++;
    double *d_result = REAL(result);

    /* Loop over non-NA input values */
    double wtsum = 0.0;
    for(j = 0; j < i_n; j++) {
      wtsum += d_w[j];
    }
    for(i = (i_n-1); i < nr; i++) {
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

    /* Initalize loop and PROTECT counters */
    int i, P=0;

    /* ensure that 'x' is double */
    if(TYPEOF(x) != REALSXP) {
      PROTECT(x = coerceVector(x, REALSXP)); P++;
    }
    double *d_x = REAL(x);

    /* If ratio is specified, and n is not, then
     * set n to approx 'correct' value backed out from ratio
     */
    int i_n;
    double d_ratio;
    if(R_NilValue == n && R_NilValue != ratio) {
      d_ratio = asReal(ratio);
      i_n = (int)(2.0 / d_ratio - 1.0);
    } else {
      i_n = asInteger(n);
    }

    /* Determine decay ratio */
    if(R_NilValue == ratio) {
      d_ratio = 2.0 / (i_n + 1);
    } else {
      d_ratio = asReal(ratio);
    }

    /* Input object length */
    int nr = nrows(x);

    /* Initalize result R object */
    SEXP result;
    PROTECT(result = allocVector(REALSXP,nr)); P++;
    double *d_result = REAL(result);

    /* check for non-leading NAs and get first non-NA location */
    SEXP _first = PROTECT(xts_na_check(x, ScalarLogical(TRUE))); P++;
    int first = INTEGER(_first)[0];
    if(i_n + 1 + first > nr) {
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
