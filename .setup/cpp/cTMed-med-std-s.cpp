// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-med-std-s.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.MedStds)]]
arma::mat MedStds(const arma::mat& phi, const arma::mat& sigma,
                  const arma::vec& delta_t, const arma::uword& from,
                  const arma::uword& to, const arma::vec& med) {
  arma::mat output(delta_t.n_elem, 4, arma::fill::none);
  arma::mat total(phi.n_rows, phi.n_cols, arma::fill::none);
  arma::mat direct(phi.n_rows, phi.n_cols, arma::fill::none);
  arma::mat d = arma::eye(phi.n_rows, phi.n_cols);
  arma::mat I = arma::eye(phi.n_rows, phi.n_cols);
  arma::mat J = arma::eye(phi.n_rows * phi.n_cols, phi.n_rows * phi.n_cols);
  double total_dbl;
  double direct_dbl;
  double indirect_dbl;
  for (arma::uword i = 0; i < med.n_elem; ++i) {
    d(med[i] - 1, med[i] - 1) = 0;
  }
  arma::mat sd_row(phi.n_rows, phi.n_cols, arma::fill::none);
  arma::mat sd_col_inv(phi.n_rows, phi.n_cols, arma::fill::none);
  arma::mat total_std(phi.n_rows, phi.n_cols, arma::fill::none);
  arma::mat direct_std(phi.n_rows, phi.n_cols, arma::fill::none);
  arma::mat total_cov;
  arma::syl(total_cov, phi, phi.t(), sigma * sigma.t());
  sd_row = arma::diagmat(arma::sqrt(total_cov.diag()));
  sd_col_inv = arma::diagmat(1.0 / arma::sqrt(total_cov.diag()));
  for (arma::uword t = 0; t < delta_t.n_elem; t++) {
    total = arma::expmat(delta_t[t] * phi);
    total_std = sd_row * total * sd_col_inv;
    total_dbl = total_std(to - 1, from - 1);
    direct = arma::expmat(delta_t[t] * d * phi * d);
    // direct_std = d * (sd_row * direct * sd_col_inv) * d;
    direct_std = sd_row * direct * sd_col_inv;
    direct_dbl = direct_std(to - 1, from - 1);
    indirect_dbl = total_dbl - direct_dbl;
    output(t, 0) = total_dbl;
    output(t, 1) = direct_dbl;
    output(t, 2) = indirect_dbl;
    output(t, 3) = delta_t[t];
  }
  return output;
}
