// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-total-central-vec.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.TotalCentralVec)]]
Rcpp::NumericVector TotalCentralVec(const arma::vec& phi_vec, const double& delta_t) {
  arma::uword p = std::sqrt(phi_vec.n_elem);
  arma::mat phi = arma::reshape(phi_vec, p, p);
  arma::mat total = arma::expmat(delta_t * phi);
  arma::vec total_central = arma::vectorise(arma::sum(total, 0) - total.diag().t());
  return Rcpp::NumericVector(total_central.begin(), total_central.end());
}
