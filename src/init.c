#include <stdlib.h>
#include <R_ext/Rdynload.h>
#include <Rinternals.h>

extern SEXP hazfit_efron(SEXP event, SEXP wt_event, SEXP wt_risk,
                         SEXP wt_eventrisk);

#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}

static const R_CallMethodDef R_CallDef[] = {
  CALLDEF(hazfit_efron, 4),
  {NULL, NULL, 0}
};

void R_init_MachineShop(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, R_CallDef, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}

