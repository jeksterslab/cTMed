// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-total-delta-t.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.TotalDeltaT)]]
Rcpp::NumericVector TotalDeltaT(const arma::mat& phi, const double& delta_t) {
  arma::mat total = arma::expmat(delta_t * phi);
  Rcpp::NumericVector total_vec(total.memptr(), total.memptr() + total.n_elem);
  total_vec.push_back(delta_t);
  return total_vec;
}
