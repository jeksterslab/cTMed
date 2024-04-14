// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-med-s.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.Meds)]]
arma::mat Meds(const arma::mat& phi, const arma::vec& delta_t, const int& from, const int& to, const arma::vec& med) {
  int ts = delta_t.n_rows;
  int ms = med.n_elem;
  int p = phi.n_rows;
  arma::mat output = arma::mat(ts, 4);
  arma::mat total = arma::mat(p, p);
  arma::mat direct = arma::mat(p, p);
  arma::mat d = arma::eye(p, p);
  double total_dbl;
  double direct_dbl;
  double indirect_dbl;
  for (int m = 0; m < ms; m++) {
    d(med[m] - 1, med[m] - 1) = 0;
  }
  for (int t = 0; t < ts; t++) {
    total = arma::expmat(delta_t[t] * phi);
    total_dbl = total(to - 1, from - 1);
    direct = arma::expmat(delta_t[t] * d * phi * d);
    direct_dbl = direct(to - 1, from - 1);
    indirect_dbl = total_dbl - direct_dbl;
    output(t, 0) = total_dbl;
    output(t, 1) = direct_dbl;
    output(t, 2) = indirect_dbl;
    output(t, 3) = delta_t[t];
  }
  return output;
}
