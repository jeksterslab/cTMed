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
  arma::mat cov_eta;
  arma::syl(cov_eta, phi, phi.t(), sigma);
  arma::vec sqrt_diag = arma::sqrt(cov_eta.diag());
  arma::mat total_std = total;
  for (size_t i = 0; i < total.n_rows; i++) {
    for (size_t j = 0; j < total.n_cols; j++) {
      total_std(i, j) *= sqrt_diag(j) / sqrt_diag(i);
    }
  }
  Rcpp::NumericVector total_std_vec(total_std.memptr(),
                                    total_std.memptr() + total_std.n_elem);
  total_std_vec.push_back(delta_t);
  return total_std_vec;
}
