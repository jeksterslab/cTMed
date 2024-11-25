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
  arma::mat I = arma::eye(phi.n_rows, phi.n_cols);
  arma::mat J = arma::eye(phi.n_rows * phi.n_cols, phi.n_rows * phi.n_cols);
  arma::mat d = arma::eye(phi.n_rows, phi.n_rows);
  for (arma::uword i = 0; i < med.n_elem; ++i) {
    d(med[i] - 1, med[i] - 1) = 0;
  }
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
  arma::mat direct = arma::expmat(delta_t * d * phi * d);
  // arma::mat direct_std = d * (sd_row * direct * sd_col_inv) * d;
  arma::mat direct_std = sd_row * direct * sd_col_inv;
  return direct_std(to - 1, from - 1);
}
