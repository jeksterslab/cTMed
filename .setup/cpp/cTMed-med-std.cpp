// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-med-std.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.MedStd)]]
Rcpp::NumericVector MedStd(const arma::mat& phi, const arma::mat& sigma,
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
  arma::vec psi_vec = arma::inv(phi_hashtag) *
                      (arma::expmat(phi_hashtag * delta_t) - J) * sigma_vec;
  arma::mat total_cov =
      arma::reshape(arma::inv(J - arma::kron(total, total)) * psi_vec,
                    phi.n_rows, phi.n_cols);
  arma::mat sd_row = arma::diagmat(arma::sqrt(total_cov.diag()));
  arma::mat sd_col_inv = arma::diagmat(1.0 / arma::sqrt(total_cov.diag()));
  arma::mat total_std = sd_row * total * sd_col_inv;
  double total_dbl = total_std(to - 1, from - 1);
  arma::mat direct = arma::expmat(delta_t * d * phi * d);
  arma::mat direct_std = d * (sd_row * direct * sd_col_inv) * d;
  double direct_dbl = direct_std(to - 1, from - 1);
  double indirect_dbl = total_dbl - direct_dbl;
  Rcpp::NumericVector output(4);
  output(0) = total_dbl;
  output(1) = direct_dbl;
  output(2) = indirect_dbl;
  output(3) = delta_t;
  return output;
}
