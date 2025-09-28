// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-direct-std.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.DirectStd)]]
double DirectStd(const arma::mat& phi, const arma::mat& sigma,
                 const double& delta_t, const arma::uword& from,
                 const arma::uword& to, const arma::vec& med) {
  arma::mat d = arma::eye(phi.n_rows, phi.n_rows);
  for (arma::uword i = 0; i < med.n_elem; ++i) {
    d(med[i] - 1, med[i] - 1) = 0;
  }
  arma::mat direct = arma::expmat(delta_t * d * phi * d);
  arma::mat cov_eta;
  arma::sylvester(cov_eta, phi, phi.t(), sigma);
  arma::vec sqrt_diag = arma::sqrt(cov_eta.diag());
  arma::mat direct_std = direct;
  for (size_t i = 0; i < direct.n_rows; i++) {
    for (size_t j = 0; j < direct.n_cols; j++) {
      direct_std(i, j) *= sqrt_diag(j) / sqrt_diag(i);
    }
  }
  return direct_std(to - 1, from - 1);
}
