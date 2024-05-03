// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-total-delta-t.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.TotalDeltaT)]]
Rcpp::NumericVector TotalDeltaT(const arma::mat& phi, const double& delta_t) {
  arma::mat total = arma::expmat(delta_t * phi);
  int q = total.n_elem;
  Rcpp::NumericVector total_vec(q);
  for (int i = 0; i < q; ++i) {
    total_vec[i] = total(i);
  }
  total_vec.push_back(delta_t);
  return total_vec;
}
