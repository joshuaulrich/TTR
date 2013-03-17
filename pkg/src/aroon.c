/*
 *  TTR: Technical Trading Rules
 *
 *  Copyright (C) 2007-2013  Joshua M. Ulrich
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

/*#include <R.h>
#include <Rinternals.h>*/
#include <xtsAPI.h>    		/* xts exported functions */
//#include <xts.h>    		/* xts exported functions */

SEXP aroon_max (SEXP x, SEXP n) {

  /* Initalize loop, loc, and PROTECT counters */
  int i, j, loc=0, P=0;

  /* Ensure x argument is double */
  if(TYPEOF(x) != REALSXP) {
    PROTECT(x = coerceVector(x, REALSXP)); P++;
  }

  /* Pointers to function arguments */
  double *real_x = REAL(x);
  int int_n = asInteger(n);

  /* Input object length */
  int nr = length(x);

  /* Initalize result R object */
  SEXP result;
  PROTECT(result = allocVector(REALSXP, nr)); P++;
  double *real_result = REAL(result);

  /* check for non-leading NAs and get first non-NA location */
  SEXP first;
  //PROTECT(first = naCheck(x, ScalarLogical(TRUE))); P++; // xts.h
  PROTECT(first = xtsNaCheck(x, ScalarLogical(TRUE))); P++; // xtsAPI.h
  int int_first = asInteger(first);
  if(int_n + 1 + int_first > nr)
    error("not enough non-NA values");

  double real_max = real_x[0];

  /* This portion is a modified version of roll_max from xts/zoo */
  for(i=0; i<nr; i++) {
    /* set leading NAs and find initial max value */
    //if(i < int_first + int_n - 1) {  // roll_max
    if(i < int_first + int_n) {
      real_result[i] = NA_REAL;
      if(real_x[i] >= real_max) {
        real_max = real_x[i];  /* set max value */
        loc = 0;               /* set max location in window */
      }
      loc++;
      continue;
    } else {
      /* if the max leaves the window */
      //if(loc >= int_n-1) {  // roll_max
      if(loc > int_n) {
        /* find the max over the (n+1) window */
        real_max = real_x[i];
        loc = 0;
        //for(j=0; j<int_n; j++) {  // roll_max
        for(j=1; j<int_n+1; j++) {
          if(real_x[i-j] > real_max) {
            real_max = real_x[i-j];
            loc = j;
          }
        }
      } else {
        /* if the new value is the new max */
        if(real_x[i] >= real_max) {
          real_max = real_x[i];
          loc = 0;
        }
      }
    }
    /* set result, increment location */
    real_result[i] = (100.0 * (int_n - loc)) / int_n;
    loc++;
  }

  /* UNPROTECT R objects and return result */
  UNPROTECT(P);
  return(result);
}

