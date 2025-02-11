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
  arma::mat total = arma::expmat(delta_t * phi);
  arma::mat total_cov;
  arma::syl(total_cov, phi, phi.t(), sigma * sigma.t());
  arma::mat sd_row = arma::diagmat(arma::sqrt(total_cov.diag()));
  arma::mat sd_col_inv = arma::diagmat(1.0 / arma::sqrt(total_cov.diag()));
  arma::mat direct = arma::expmat(delta_t * d * phi * d);
  // arma::mat direct_std = d * (sd_row * direct * sd_col_inv) * d;
  arma::mat direct_std = sd_row * direct * sd_col_inv;
  return direct_std(to - 1, from - 1);
}
