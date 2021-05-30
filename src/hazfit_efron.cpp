#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector hazfit_efron(
    int n, IntegerVector event, NumericVector wt_event, NumericVector wt_risk,
    NumericVector wt_eventrisk
) {
  int i, j, m;
  NumericVector res(n);

  for (i = 0; i < n; i++) {
    m = event[i];
    for (j = 0; j < m; j++) {
      res[i] += (1 / (wt_risk[i] - wt_eventrisk[i] * j / m)) / m;
    }
    res[i] *= wt_event[i];
  }

  return res;
}
