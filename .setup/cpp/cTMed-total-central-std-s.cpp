// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-total-central-std-s.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.TotalCentralStds)]]
arma::mat TotalCentralStds(const arma::mat& phi, const arma::mat& sigma,
                           const arma::vec& delta_t) {
  arma::uword p = phi.n_rows;
  arma::mat output(p, delta_t.n_elem, arma::fill::zeros);
  arma::mat cov_eta;
  arma::sylvester(cov_eta, phi, phi.t(), sigma);
  arma::vec sqrt_diag = arma::sqrt(cov_eta.diag());
  arma::mat total(p, p, arma::fill::none);
  for (arma::uword t = 0; t < delta_t.n_elem; ++t) {
    total = arma::expmat(delta_t[t] * phi);
    for (arma::uword j = 0; j < p; ++j) {
      for (arma::uword i = 0; i < p; ++i) {
        if (i != j) {
          output(j, t) += total(i, j) * sqrt_diag(j) / sqrt_diag(i);
        }
      }
    }
  }
  return output.t();
}
