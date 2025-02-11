// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-total-std-delta-t.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.TotalStdDeltaT)]]
Rcpp::NumericVector TotalStdDeltaT(const arma::mat& phi, const arma::mat& sigma,
                                   const double& delta_t) {
  arma::mat total = arma::expmat(delta_t * phi);
  arma::mat total_cov;
  arma::syl(total_cov, phi, phi.t(), sigma * sigma.t());
  arma::mat sd_row = arma::diagmat(arma::sqrt(total_cov.diag()));
  arma::mat sd_col_inv = arma::diagmat(1.0 / arma::sqrt(total_cov.diag()));
  arma::mat total_std = sd_row * total * sd_col_inv;
  Rcpp::NumericVector total_std_vec(total_std.memptr(),
                                    total_std.memptr() + total_std.n_elem);
  total_std_vec.push_back(delta_t);
  return total_std_vec;
}
