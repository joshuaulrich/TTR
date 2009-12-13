/*
 *
 *  TTR: Technical Trading Rules
 *
 *  Copyright (C) 2007-2009  Joshua M. Ulrich
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
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

SEXP ema (SEXP x, SEXP n, SEXP ratio) {
    
    // Initalize loop and PROTECT counters
    int i, P=0;

    // assure that 'x' is double
    if(TYPEOF(x) != REALSXP) {
      PROTECT(x = coerceVector(x, REALSXP)); P++;
    }
    // assure that 'n' is integer
    if(TYPEOF(n) != INTSXP) {
      PROTECT(n = coerceVector(n, INTSXP)); P++;
    }

    // Pointers to function arguments
    double *d_x = REAL(x);
    int i_n = INTEGER(n)[0];
    double d_ratio = REAL(ratio)[0];
    
    // Input object length
    int nr = nrows(x);

    // Initalize result R object
    SEXP result; PROTECT(result = allocVector(REALSXP,nr)); P++;
    double *d_result = REAL(result);

    // Find first non-NA input value
    int beg = i_n - 1;
    d_result[beg] = 0;
    for(i = 0; i <= beg; i++) {
        // Account for leading NAs in input
        if(ISNA(d_x[i])) {
            d_result[i] = NA_REAL;
            beg++;
            d_result[beg] = 0;
            continue;
        }
        // Set leading NAs in output
        if(i < beg) {
            d_result[i] = NA_REAL;
        }
        // Raw mean to start EMA
        d_result[beg] += d_x[i] / i_n;
    }

    // Loop over non-NA input values
    for(i = beg+1; i < nr; i++) {
        d_result[i] = d_x[i] * d_ratio + d_result[i-1] * (1-d_ratio);
    }

    // UNPROTECT R objects and return result
    UNPROTECT(P);
    return(result);
}

SEXP evwma (SEXP pr, SEXP vo, SEXP n) {
    
    // Initalize loop and PROTECT counters
    int i, P=0;

    // assure that 'pr' is double
    if(TYPEOF(pr) != REALSXP) {
      PROTECT(pr = coerceVector(pr, REALSXP)); P++;
    }
    // assure that 'vo' is double
    if(TYPEOF(vo) != REALSXP) {
      PROTECT(vo = coerceVector(vo, REALSXP)); P++;
    }
    // assure that 'n' is integer
    if(TYPEOF(n) != INTSXP) {
      PROTECT(n = coerceVector(n, INTSXP)); P++;
    }

    // Pointers to function arguments
    double *d_pr = REAL(pr);
    double *d_vo = REAL(vo);
    int i_n = INTEGER(n)[0];
    
    // Input object length
    int nr = nrows(pr);

    // Initalize result R object
    SEXP result; PROTECT(result = allocVector(REALSXP,nr)); P++;
    double *d_result = REAL(result);

    // Volume Sum
    double volSum = 0;

    // Find first non-NA input value
    int beg = i_n - 1;
    for(i = 0; i <= beg; i++) {
        // Account for leading NAs in input
        if(ISNA(d_pr[i]) || ISNA(d_vo[i])) {
            d_result[i] = NA_REAL;
            beg++;
            continue;
        }
        // Set leading NAs in output
        if(i < beg) {
            d_result[i] = NA_REAL;
        // First result value is first price value
        } else {
            d_result[i] = d_pr[i];
        }
        // Keep track of volume Sum
        volSum += d_vo[i];
    }

    // Loop over non-NA input values
    for(i = beg+1; i < nr; i++) {
        volSum = volSum + d_vo[i] - d_vo[i-i_n];
        d_result[i] = ((volSum-d_vo[i])*d_result[i-1]+d_vo[i]*d_pr[i])/volSum;
    }

    // UNPROTECT R objects and return result
    UNPROTECT(P);
    return(result);
}

