// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-total-d-t-vec.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.TotalVec)]]
arma::vec TotalVec(const arma::vec& phi_vec, const double& delta_t) {
  int q = phi_vec.n_elem;
  int p = std::sqrt(q);
  arma::mat phi = arma::reshape(phi_vec, p, p);
  arma::mat total = arma::mat(p, p);
  total = arma::expmat(delta_t * phi);
  arma::vec total_vec = arma::vectorise(total);
  return total_vec;
}
