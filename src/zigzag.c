/*
 *  TTR: Technical Trading Rules
 *
 *  Copyright (C) 2007-2017  Joshua M. Ulrich
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

SEXP ttr_zigzag
(SEXP hi, SEXP lo, SEXP chg, SEXP pct, SEXP retr, SEXP lastex)
{
  int refpos = 0;
  int infpos = 1;
  int sig = 0;

  int use_percent = asLogical(pct);
  int use_retrace = asLogical(retr);
  int use_last_ex = asLogical(lastex);

  double* high = REAL(hi);
  double* low = REAL(lo);
  double change = asReal(chg);
  if (use_percent)
    change = change / 100.0;

  double refval = (high[0] + low[0]) / 2; /* reference */
  double infval = (high[1] + low[1]) / 2; /* inflection */

  double emin, emax, lmin, lmax;

  SEXP _result = PROTECT(allocVector(REALSXP, length(hi)));
  double* result = REAL(_result);

  for (int i = 1; i < length(hi); i++) {
    /* Initialize all result values to NA */
    result[i] = NA_REAL;

    if (use_percent) {
      /* If % change given (absolute move) */
      emin = infval * (1.0 - change);
      emax = infval * (1.0 + change);
    } else {
      /* If $ change given (only absolute moves make sense) */
      emin = infval - change;
      emax = infval + change;
    }
    /* Find local maximum and minimum */
    lmax = infval > high[i] ? infval : high[i];
    lmin = infval < low[i] ? infval : low[i];

    /* Find first trend */
    if (sig == 0) {
      if (use_retrace) {
        /* Retrace prior move */
        sig = (infval >= refval) ? 1 : -1;
      } else {
        /* Absolute move */
        if (lmin <= emin) {
          /* Confirmed Downtrend */
          sig = -1;
        }
        if (lmax >= emax) {
          /* Confirmed Uptrend */
          sig = 1;
        }
      }
    }
    /* Downtrend */
    if (sig == -1) {
      /* New Minimum */
      if (low[i] == lmin) {
        /* Last Extreme */
        if (use_last_ex) {
          infval = low[i];
          infpos = i;
        } else {
          /* First Extreme */
          if (low[i] != low[i-1]) {
            infval = low[i];
            infpos = i;
          }
        }
      }
      /* Retrace prior move */
      if (use_retrace) {
        emax = infval + ((refval - infval) * change);
      }
      /* Trend Reversal */
      if (high[i] >= emax) {
        result[refpos] = refval;
        refval = infval;
        refpos = infpos;
        infval = high[i];
        infpos = i;
        sig = 1;
        continue;
      }
    }
    /* Uptrend */
    if (sig == 1) {
      /* New Maximum */
      if (high[i] == lmax) {
        /* Last Extreme */
        if (use_last_ex) {
          infval = high[i];
          infpos = i;
        } else {
          /* First Extreme */
          if (high[i] != high[i-1]) {
            infval = highighigh[i];
            infpos = i;
          }
        }
      }
      /* Retrace prior move */
      if (use_retrace) {
        emin = infval - ((infval - refval) * change);
      }
      /* Trend Reversal */
      if (low[i] <= emin) {
        result[refpos] = refval;
        refval = infval;
        refpos = infpos;
        infval = low[i];
        infpos = i;
        sig = -1;
        continue;
      }
    }
  }
  result[refpos] = refval;
  result[infpos] = infval;

  UNPROTECT(1);
  return _result;
}
