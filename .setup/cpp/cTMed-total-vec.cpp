// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-total-vec.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.TotalVec)]]
arma::vec TotalVec(const arma::vec& phi_vec, const double& delta_t) {
  arma::uword p = std::sqrt(phi_vec.n_elem);
  arma::mat total = arma::expmat(delta_t * arma::reshape(phi_vec, p, p));
  return arma::vectorise(total);
}
