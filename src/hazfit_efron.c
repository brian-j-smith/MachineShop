#include <R.h>
#include <Rinternals.h>

SEXP hazfit_efron(SEXP event, SEXP wt_event, SEXP wt_risk, SEXP wt_eventrisk) {
  int i, j, m, n = length(event);
  SEXP res = PROTECT(allocVector(REALSXP, n));

  for (i = 0; i < n; i++) {
    REAL(res)[i] = 0;
    m = INTEGER(event)[i];
    for (j = 0; j < m; j++) {
      REAL(res)[i] +=
        (1 / (REAL(wt_risk)[i] - REAL(wt_eventrisk)[i] * j / m)) / m;
    }
    REAL(res)[i] *= REAL(wt_event)[i];
  }
  UNPROTECT(1);

  return res;
}
