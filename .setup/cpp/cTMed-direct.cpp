// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-direct.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.Direct)]]
double Direct(const arma::mat& phi, const double& delta_t, const int& from, const int& to, const arma::vec& med) {
  int ms = med.n_elem;
  int p = phi.n_rows;
  arma::mat d = arma::eye(p, p);
  for (int m = 0; m < ms; m++) {
    d(med[m] - 1, med[m] - 1) = 0;
  }
  arma::mat direct = arma::expmat(delta_t * d * phi * d);
  return direct(to - 1, from - 1);
}
