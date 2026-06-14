// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-direct-central-std-s.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.DirectCentralStds)]]
arma::mat DirectCentralStds(const arma::mat& phi, const arma::mat& sigma,
                            const arma::vec& delta_t) {
  arma::uword p = phi.n_rows;
  arma::mat output(p, delta_t.n_elem, arma::fill::zeros);
  arma::mat cov_eta;
  arma::sylvester(cov_eta, phi, phi.t(), sigma);
  arma::vec sqrt_diag = arma::sqrt(cov_eta.diag());
  arma::mat direct(p, p, arma::fill::none);
  arma::mat d = arma::eye(p, p);
  for (arma::uword t = 0; t < delta_t.n_elem; ++t) {
    for (arma::uword m = 0; m < p; ++m) {
      d = arma::eye(p, p);
      d(m, m) = 0;
      direct = arma::expmat(delta_t[t] * d * phi * d);
      for (arma::uword j = 0; j < p; ++j) {
        for (arma::uword i = 0; i < p; ++i) {
          if (!(m == i || m == j || i == j)) {
            output(m, t) += direct(i, j) * sqrt_diag(j) / sqrt_diag(i);
          }
        }
      }
    }
  }
  return output.t();
}
