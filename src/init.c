#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP distAngle(SEXP, SEXP, SEXP);
extern SEXP distEuclid(SEXP, SEXP, SEXP);
extern SEXP rollMean(SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"distAngle",  (DL_FUNC) &distAngle,  3},
  {"distEuclid", (DL_FUNC) &distEuclid, 3},
  {"rollMean",   (DL_FUNC) &rollMean,   4},
  {NULL, NULL, 0}
};

void R_init_openair(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}