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

#include <R.h>
#include <Rinternals.h>

SEXP adjRatios (SEXP split, SEXP div, SEXP close) {

    /* Initialize REAL pointers to function arguments */
    double *real_close = REAL(close);
    double *real_split = REAL(split);
    double *real_div   = REAL(div);
    
    /* Initalize loop and PROTECT counters */
    int i, P = 0;
    /* Initalize object length (NOTE: all arguments are the same length) */
    int N = length(close);

    /* Initalize result R objects */
    SEXP result;    PROTECT(result  = allocVector(VECSXP, 2)); P++;
    SEXP s_ratio;   PROTECT(s_ratio = allocVector(REALSXP,N)); P++;
    SEXP d_ratio;   PROTECT(d_ratio = allocVector(REALSXP,N)); P++;
    
    /* Initialize REAL pointers to R objects and set their last value to '1' */
    double *rs_ratio = REAL(s_ratio);
    double *rd_ratio = REAL(d_ratio);
    rs_ratio[N-1] = 1;
    rd_ratio[N-1] = 1;

    /* Loop over split/div vectors from newest period to oldest */
    for(i = N-1; i > 0; i--) {
        /* Carry newer ratio value backward */
        if(ISNA(real_split[i])) {
            rs_ratio[i-1] = rs_ratio[i];
        /* Update split ratio */
        } else {
            rs_ratio[i-1] = rs_ratio[i] * real_split[i];
        }
        /* Carry newer ratio value backward */
        if(ISNA(real_div[i])) {
            rd_ratio[i-1] = rd_ratio[i];
        } else {
        /* Update dividend ratio */
            rd_ratio[i-1] = rd_ratio[i] *
                (1.0 - real_div[i] / real_close[i-1]);
        }
    }
    
    /* Assign results to list */
    SET_VECTOR_ELT(result, 0, s_ratio);
    SET_VECTOR_ELT(result, 1, d_ratio);

    /* UNPROTECT R objects and return result */
    UNPROTECT(P);
    return(result);
}
