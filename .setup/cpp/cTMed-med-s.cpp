// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-med-d-t-s.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.Meds)]]
const arma::mat Meds(const arma::mat& phi, const arma::vec& delta_t,
                     const int& from, const int& to, const arma::vec& med) {
  int t = delta_t.n_rows;
  arma::mat output = arma::mat(t, 4);
  int p = phi.n_rows;
  arma::mat total = arma::mat(p, p);
  double total_dbl;
  arma::mat d = arma::eye(p, p);
  int m = med.n_elem;
  for (int i = 0; i < m; i++) {
    d(med[i] - 1, med[i] - 1) = 0;
  }
  arma::mat direct = arma::mat(p, p);
  double direct_dbl;
  double indirect_dbl;
  for (int i = 0; i < t; i++) {
    // total effect
    total = arma::expmat(delta_t[i] * phi);
    total_dbl = total(to - 1, from - 1);
    // direct effect
    direct = arma::expmat(delta_t[i] * d * phi * d);
    direct_dbl = direct(to - 1, from - 1);
    // indirect effect
    indirect_dbl = total_dbl - direct_dbl;
    // output
    output(i, 0) = total_dbl;
    output(i, 1) = direct_dbl;
    output(i, 2) = indirect_dbl;
    output(i, 3) = delta_t[i];
  }
  return output;
}
