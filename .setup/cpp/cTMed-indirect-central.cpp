// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-indirect-central.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.IndirectCentral)]]
Rcpp::NumericVector IndirectCentral(const arma::mat& phi, const double& delta_t) {
  arma::uword p = phi.n_rows;
  arma::mat total = arma::expmat(delta_t * phi);
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
          output(m) += total(i, j) - direct(i, j);
        }
      }
    }
  }
  return output;
}
