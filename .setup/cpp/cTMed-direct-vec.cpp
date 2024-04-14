// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-direct-vec.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.DirectVec)]]
double DirectVec(const arma::vec& phi_vec, const double& delta_t, const int& from, const int& to, const arma::vec& med) {
  int ms = med.n_elem;
  int p = std::sqrt(phi_vec.n_elem);
  arma::mat phi = arma::reshape(phi_vec, p, p);
  arma::mat d = arma::eye(p, p);
  for (int m = 0; m < ms; m++) {
    d(med[m] - 1, med[m] - 1) = 0;
  }
  arma::mat direct = arma::expmat(delta_t * d * phi * d);
  return direct(to - 1, from - 1);
}
