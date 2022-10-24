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

/* Includes and defines from WRE Section 5.4.2 */
#include "ttr.h"
#include <R.h>
#include <stdlib.h>  /* for NULL */
#include <R_ext/Rdynload.h>

#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}

/* define xts imports */
SEXP (*xts_na_check)(SEXP,SEXP);

static const R_CallMethodDef CallEntries[] = {
  CALLDEF(adjRatios,            3),
  CALLDEF(aroon_max,            2),
  CALLDEF(ema,                  4),
  CALLDEF(evwma,                3),
  CALLDEF(sar,                  4),
  CALLDEF(ttr_rollPercentRank,  4),
  CALLDEF(ttr_zigzag,           6),
  CALLDEF(vma,                  3),
  CALLDEF(wilderSum,            2),
  CALLDEF(wma,                  3),
  CALLDEF(zlema,                3),
  CALLDEF(runsum,               2),
  CALLDEF(runmin,               2),
  CALLDEF(runmax,               2),
  CALLDEF(runmedian,            4),
  CALLDEF(runmad,               6),
  CALLDEF(runcov,               5),
  {NULL, NULL, 0}
};

/* Restrict .Call etc to use only registered symbols */
void R_init_TTR(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);

  /* imports from xts C code */
  xts_na_check = (SEXP(*)(SEXP,SEXP)) R_GetCCallable("xts", "naCheck");
}
