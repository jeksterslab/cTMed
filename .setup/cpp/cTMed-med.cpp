// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-med-d-t.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.Med)]]
Rcpp::NumericVector Med(const arma::mat& phi, const double& delta_t, const int& from, const int& to, const arma::vec& med) {
  Rcpp::NumericVector output(4);
  int p = phi.n_rows;
  // total effect
  arma::mat total = arma::mat(p, p);
  total = arma::expmat(delta_t * phi);
  double total_dbl = total(to - 1, from - 1);
  // direct effect
  arma::mat d = arma::eye(p, p);
  int m = med.n_elem;
  for (int i = 0; i < m; i++) {
    d(med[i] - 1, med[i] - 1) = 0;
  }
  arma::mat direct = arma::mat(p, p);
  direct = arma::expmat(delta_t * d * phi * d);
  double direct_dbl = direct(to - 1, from - 1);
  // indirect effect
  double indirect_dbl = total_dbl - direct_dbl;
  // output
  output(0) = total_dbl;
  output(1) = direct_dbl;
  output(2) = indirect_dbl;
  output(3) = delta_t;
  return output;
}
