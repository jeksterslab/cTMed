// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-direct.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.Direct)]]
double Direct(const arma::mat& phi, const double& delta_t, const arma::uword& from, const arma::uword& to, const arma::vec& med) {
  arma::mat d = arma::eye(phi.n_rows, phi.n_rows);
  for (arma::uword i = 0; i < med.n_elem; ++i) {
    d(med[i] - 1, med[i] - 1) = 0;
  }
  arma::mat direct = arma::expmat(delta_t * d * phi * d);
  return direct(to - 1, from - 1);
}
