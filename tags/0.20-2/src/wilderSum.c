/*
 *  TTR: Technical Trading Rules
 *
 *  Copyright (C) 2007-2010  Joshua M. Ulrich
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

SEXP wilderSum (SEXP x, SEXP n) {

    /* Initalize loop and PROTECT counters */
    int i, P=0;

    /* assure that 'x' is double */
    if(TYPEOF(x) != REALSXP) {
      PROTECT(x = coerceVector(x, REALSXP)); P++;
    }
    /* assure that 'n' is integer */
    if(TYPEOF(n) != INTSXP) {
      PROTECT(n = coerceVector(n, INTSXP)); P++;
    }

    /* Pointers to function arguments */
    double *d_x = REAL(x);
    int i_n = INTEGER(n)[0];

    /* Input object length */
    int nr = nrows(x);

    /* Initalize result R object */
    SEXP result; PROTECT(result = allocVector(REALSXP,nr)); P++;
    double *d_result = REAL(result);

    int beg = i_n - 1;
    d_result[0] = d_x[0];

    /* Loop over input, starting at 2nd element */
    for(i = 1; i < nr; i++) {
        /* Account for leading NAs in input */
        if(ISNA(d_x[i-1])) {
            d_result[i-1] = NA_REAL;
            beg++;
            d_result[i] = d_x[i];
            continue;
        }
        /* Calculate sum */
        d_result[i] = d_x[i] + d_result[i-1] * (i_n-1)/i_n;
        /* Set leading NAs in output */
        if(i <= beg) {
            d_result[i-1] = NA_REAL;
        }
    }

    /* UNPROTECT R objects and return result */
    UNPROTECT(P);
    return(result);
}
