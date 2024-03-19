// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-total-d-t.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.Total)]]
arma::mat Total(const arma::mat& phi, const double& delta_t) {
  int p = phi.n_rows;
  arma::mat total = arma::mat(p, p);
  total = arma::expmat(delta_t * phi);
  return total;
}
