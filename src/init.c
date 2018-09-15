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
#define FORTRANDEF(name, n)  {#name, (DL_FUNC) &F77_NAME(name), n}

/* Declare .Call calls */
extern SEXP adjRatios(SEXP, SEXP, SEXP);
extern SEXP aroon_max(SEXP, SEXP);
extern SEXP ema(SEXP, SEXP, SEXP, SEXP);
extern SEXP evwma(SEXP, SEXP, SEXP);
extern SEXP sar(SEXP, SEXP, SEXP, SEXP);
extern SEXP ttr_rollPercentRank(SEXP, SEXP, SEXP, SEXP);
extern SEXP ttr_zigzag(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP vma(SEXP, SEXP, SEXP);
extern SEXP wilderSum(SEXP, SEXP);
extern SEXP wma(SEXP, SEXP, SEXP);
extern SEXP zlema(SEXP, SEXP, SEXP);

/* Declare .Fortran calls */
extern void F77_NAME(runcov)(double *, double *, double *, double *, int *, int *, int *, double *, int *);
extern void F77_NAME(runmad)(double *, double *, int *, int *, double *, int *, int *, int *);
extern void F77_NAME(runmax)(double *, int *, int *, double *, int *);
extern void F77_NAME(runmedian)(double *, int *, double *, int *, int *, int *);
extern void F77_NAME(runmin)(double *, int *, int *, double *, int *);
extern void F77_NAME(runsum)(double *, int *, int *, double *, int *);

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
  {NULL, NULL, 0}
};

static const R_FortranMethodDef FortranEntries[] = {
  FORTRANDEF(runcov,     9),
  FORTRANDEF(runmad,     8),
  FORTRANDEF(runmax,     5),
  FORTRANDEF(runmedian,  6),
  FORTRANDEF(runmin,     5),
  FORTRANDEF(runsum,     5),
  {NULL, NULL, 0}
};

/* Restrict .Call etc to use only registered symbols */
void R_init_TTR(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, FortranEntries, NULL);
  R_useDynamicSymbols(dll, FALSE);
  //R_forceSymbols(dll, TRUE);  /* only use R symbols (not strings) */

  xts_na_check = (SEXP(*)(SEXP,SEXP)) R_GetCCallable("xts", "naCheck");
}
