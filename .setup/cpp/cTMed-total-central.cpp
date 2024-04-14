// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-total-central.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.TotalCentral)]]
Rcpp::NumericVector TotalCentral(const arma::mat& phi, const double& delta_t) {
  arma::mat total = arma::expmat(delta_t * phi);
  arma::vec total_central = arma::vectorise(arma::sum(total, 0) - total.diag().t());
  return Rcpp::NumericVector(total_central.begin(), total_central.end());
}
