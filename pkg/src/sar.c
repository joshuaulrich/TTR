/*
 *  TTR: Technical Trading Rules
 *
 *  Copyright (C) 2007-2012  Joshua M. Ulrich
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

SEXP sar (SEXP hi, SEXP lo, SEXP xl) {

    /* Initalize loop and PROTECT counters */
    int i, P=0;

    /* Ensure all arguments are double */
    if(TYPEOF(hi) != REALSXP) {
      PROTECT(hi = coerceVector(hi, REALSXP)); P++;
    }
    if(TYPEOF(lo) != REALSXP) {
      PROTECT(lo = coerceVector(lo, REALSXP)); P++;
    }
    if(TYPEOF(xl) != REALSXP) {
      PROTECT(xl = coerceVector(xl, REALSXP)); P++;
    }

    /* Pointers to function arguments */
    double *d_hi = REAL(hi);
    double *d_lo = REAL(lo);
    double *d_xl = REAL(xl);

    /* Input object length */
    int nr = nrows(hi);

    /* Initalize result R object */
    SEXP sar; PROTECT(sar = allocVector(REALSXP,nr)); P++;
    double *d_sar = REAL(sar);

    /* Find first non-NA value */
    int beg = 1;
    for(i=0; i < nr; i++) {
      if( ISNA(d_hi[i]) || ISNA(d_lo[i]) ) {
        d_sar[i] = NA_REAL;
        beg++;
      } else {
        break;
      }
    }

    /* Initialize values needed by the routine */
    int sig0 = 1, sig1 = 0;
    double xpt0 = d_hi[beg-1], xpt1 = 0;
    double af0 = d_xl[0], af1 = 0;
    double lmin, lmax;
    d_sar[beg-1] = d_lo[beg-1]-0.01;

    for(i=beg; i < nr; i++) {
      /* Increment signal, extreme point, and acceleration factor */
      sig1 = sig0;
      xpt1 = xpt0;
      af1 = af0;

      /* Local extrema */
      lmin = (d_lo[i-1] < d_lo[i]) ? d_lo[i-1] : d_lo[i];
      lmax = (d_hi[i-1] > d_hi[i]) ? d_hi[i-1] : d_hi[i];

      /*
       * Create signal and extreme price vectors
       */

      /* Previous buy signal */
      if( sig1 == 1 ) {

        sig0 = (d_lo[i] > d_sar[i-1]) ? 1 : -1;   /* New signal */
        xpt0 = fmax(lmax, xpt1); /* New extreme price */

      /* Previous sell signal */
      } else {

        sig0 = (d_hi[i] < d_sar[i-1]) ? -1 : 1;   /* New signal */
        xpt0 = fmin(lmin, xpt1); /* New extreme price */

      }

      /*
       * Calculate acceleration factor (af)
       * and stop-and-reverse (sar) vector
       */

      /* No signal change */
      if( sig0 == sig1 ) {

        d_sar[i] = d_sar[i-1] + ( xpt1 - d_sar[i-1] ) * af1;

        /* Current buy signal */
        if( sig0 == 1 ) {

          /* Determine new acceleration factor vector value */
          if( xpt0 > xpt1 ) {
            af0 = (af1 == d_xl[1]) ? d_xl[1] : (d_xl[0] + af1);
          } else {
            af0 = af1;
          }

          /* Determine sar vector value */
          if( d_sar[i] > lmin ) {
            d_sar[i] = lmin;
          }

        /* Current sell signal */
        } else {

          /* Determine new acceleration factor vector value */
          if( xpt0 < xpt1 ) {
            af0 = (af1 == d_xl[1]) ? d_xl[1] : (d_xl[0] + af1);
          } else {
            af0 = af1;
          }

          /* Determine sar vector value */
          if( d_sar[i] < lmax ) {
            d_sar[i] = lmax;
          }
        }

      /* New signal */
      } else {
        af0 = d_xl[0];
        d_sar[i] = xpt0;
      }
    }
    
    /* UNPROTECT R objects and return result */
    UNPROTECT(P);
    return(sar);
}

