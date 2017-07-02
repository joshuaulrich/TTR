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

/* price and its array index */
typedef struct {
  double price;
  int index;
} price_and_index;

SEXP ttr_zigzag
(SEXP _high, SEXP _low, SEXP _change, SEXP _percent, SEXP _retrace,
 SEXP _last_extreme)
{
  double* high = REAL(_high);
  double* low = REAL(_low);
  double change = asReal(_change);
  int use_percent = asLogical(_percent);
  int use_retrace = asLogical(_retrace);
  int use_last_ex = asLogical(_last_extreme);

  if (use_percent)
    change = change / 100.0;

  int n = length(_high);
  SEXP _zigzag = PROTECT(allocVector(REALSXP, n));
  double* zigzag = REAL(_zigzag);

  price_and_index reference, inflection;
  reference.price = (high[0] + low[0]) / 2;
  reference.index = 0;
  inflection.price = (high[1] + low[1]) / 2;
  inflection.index = 1;

  double extreme_min, extreme_max, local_min, local_max;
  int signal = 0;

  for (int i = 1; i < n; i++) {
    /* Initialize all zigzag values to NA */
    zigzag[i] = NA_REAL;

    if (use_percent) {
      /* If % change given (absolute move) */
      extreme_min = inflection.price * (1.0 - change);
      extreme_max = inflection.price * (1.0 + change);
    } else {
      /* If $ change given (only absolute moves make sense) */
      extreme_min = inflection.price - change;
      extreme_max = inflection.price + change;
    }
    /* Find local maximum and minimum */
    local_max = inflection.price > high[i] ? inflection.price : high[i];
    local_min = inflection.price < low[i] ? inflection.price : low[i];

    /* Find first trend */
    if (signal == 0) {
      if (use_retrace) {
        /* Retrace prior move */
        signal = (inflection.price >= reference.price) ? 1 : -1;
      } else {
        /* Absolute move */
        if (local_min <= extreme_min) {
          /* Confirmed Downtrend */
          signal = -1;
        }
        if (local_max >= extreme_max) {
          /* Confirmed Uptrend */
          signal = 1;
        }
      }
    }
    /* Downtrend */
    if (signal == -1) {
      /* New Minimum */
      if (low[i] == local_min) {
        /* Last Extreme */
        if (use_last_ex) {
          inflection.price = low[i];
          inflection.index = i;
        } else {
          /* First Extreme */
          if (low[i] != low[i-1]) {
            inflection.price = low[i];
            inflection.index = i;
          }
        }
      }
      /* Retrace prior move */
      if (use_retrace) {
        extreme_max = inflection.price +
          ((reference.price - inflection.price) * change);
      }
      /* Trend Reversal */
      if (high[i] >= extreme_max) {
        zigzag[reference.index] = reference.price;
        reference = inflection;
        inflection.price = high[i];
        inflection.index = i;
        signal = 1;
        continue;
      }
    }
    /* Uptrend */
    if (signal == 1) {
      /* New Maximum */
      if (high[i] == local_max) {
        /* Last Extreme */
        if (use_last_ex) {
          inflection.price = high[i];
          inflection.index = i;
        } else {
          /* First Extreme */
          if (high[i] != high[i-1]) {
            inflection.price = high[i];
            inflection.index = i;
          }
        }
      }
      /* Retrace prior move */
      if (use_retrace) {
        extreme_min = inflection.price -
          ((inflection.price - reference.price) * change);
      }
      /* Trend Reversal */
      if (low[i] <= extreme_min) {
        zigzag[reference.index] = reference.price;
        reference = inflection;
        inflection.price = low[i];
        inflection.index = i;
        signal = -1;
        continue;
      }
    }
  }
  zigzag[reference.index] = reference.price;
  zigzag[inflection.index] = inflection.price;

  UNPROTECT(1);
  return _zigzag;
}
