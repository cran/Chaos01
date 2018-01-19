#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
 Check these declarations against the C/Fortran source code.
 */

/* .C calls */
extern void compute_kc(void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void diag_rqa_max(void *, void *, void *, void *, void *, void *, void *, void *);

static const R_CMethodDef CEntries[] = {
  {"compute_kc",   (DL_FUNC) &compute_kc,   9},
  {"diag_rqa_max", (DL_FUNC) &diag_rqa_max, 8},
  {NULL, NULL, 0}
};

void R_init_Chaos01(DllInfo *dll)
{
  R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
