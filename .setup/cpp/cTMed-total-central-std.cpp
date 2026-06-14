// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-total-central-std.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.TotalCentralStd)]]
Rcpp::NumericVector TotalCentralStd(const arma::mat& phi,
                                    const arma::mat& sigma,
                                    const double& delta_t) {
  arma::uword p = phi.n_rows;
  arma::mat total = arma::expmat(delta_t * phi);
  arma::mat cov_eta;
  arma::sylvester(cov_eta, phi, phi.t(), sigma);
  arma::vec sqrt_diag = arma::sqrt(cov_eta.diag());
  arma::vec output(p, arma::fill::zeros);
  for (arma::uword j = 0; j < p; ++j) {
    for (arma::uword i = 0; i < p; ++i) {
      if (i != j) {
        output(j) += total(i, j) * sqrt_diag(j) / sqrt_diag(i);
      }
    }
  }
  return Rcpp::NumericVector(output.begin(), output.end());
}
