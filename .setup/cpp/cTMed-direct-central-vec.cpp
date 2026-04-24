// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-direct-central-vec.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.DirectCentralVec)]]
Rcpp::NumericVector DirectCentralVec(const arma::vec& phi_vec,
                                     const double& delta_t) {
  arma::uword p = std::sqrt(phi_vec.n_elem);
  arma::mat phi = arma::reshape(phi_vec, p, p);
  arma::mat direct(p, p, arma::fill::none);
  arma::mat d = arma::eye(p, p);
  Rcpp::NumericVector output(p);
  for (arma::uword m = 0; m < p; m++) {
    d = arma::eye(p, p);
    d(m, m) = 0;
    direct = arma::expmat(delta_t * d * phi * d);
    for (arma::uword j = 0; j < p; j++) {
      for (arma::uword i = 0; i < p; i++) {
        if (!(m == i || m == j || i == j)) {
          output(m) += direct(i, j);
        }
      }
    }
  }
  return output;
}
