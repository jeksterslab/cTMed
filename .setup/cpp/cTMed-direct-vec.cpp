// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-direct-d-t-vec.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.DirectVec)]]
double DirectVec(const arma::vec& phi_vec, const double& delta_t,
                 const int& from, const int& to, const arma::vec& med) {
  int p = std::sqrt(phi_vec.n_elem);
  arma::mat phi = arma::reshape(phi_vec, p, p);
  arma::mat d = arma::eye(p, p);
  int m = med.n_elem;
  for (int i = 0; i < m; i++) {
    d(med[i] - 1, med[i] - 1) = 0;
  }
  arma::mat direct = arma::expmat(delta_t * d * phi * d);
  double direct_dbl = direct(to - 1, from - 1);
  return direct_dbl;
}
