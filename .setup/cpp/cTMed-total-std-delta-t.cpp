// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-total-std-delta-t.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.TotalStdDeltaT)]]
Rcpp::NumericVector TotalStdDeltaT(const arma::mat& phi, const arma::mat& sigma,
                                   const double& delta_t) {
  arma::mat I = arma::eye(phi.n_rows, phi.n_cols);
  arma::mat J = arma::eye(phi.n_rows * phi.n_cols, phi.n_rows * phi.n_cols);
  arma::mat total = arma::expmat(delta_t * phi);
  arma::mat phi_hashtag = arma::kron(phi, I) + arma::kron(I, phi);
  arma::vec sigma_vec = arma::vectorise(sigma);
  // arma::vec psi_vec = arma::inv(phi_hashtag) * (arma::expmat(phi_hashtag *
  // delta_t) - J) * sigma_vec;
  arma::vec psi_vec = arma::solve(
      phi_hashtag, (arma::expmat(phi_hashtag * delta_t) - J) * sigma_vec);
  // arma::mat total_cov = arma::reshape(arma::inv(J - arma::kron(total, total))
  // * psi_vec, phi.n_rows, phi.n_cols);
  arma::mat total_cov =
      arma::reshape(arma::solve(J - arma::kron(total, total), psi_vec),
                    phi.n_rows, phi.n_cols);
  arma::mat sd_row = arma::diagmat(arma::sqrt(total_cov.diag()));
  arma::mat sd_col_inv = arma::diagmat(1.0 / arma::sqrt(total_cov.diag()));
  arma::mat total_std = sd_row * total * sd_col_inv;
  Rcpp::NumericVector total_std_vec(total_std.memptr(),
                                    total_std.memptr() + total_std.n_elem);
  total_std_vec.push_back(delta_t);
  return total_std_vec;
}
