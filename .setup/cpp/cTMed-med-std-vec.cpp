// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-med-std-vec.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.MedStdVec)]]
Rcpp::NumericVector MedStdVec(const arma::vec& v, const double& delta_t, const arma::uword& from, const arma::uword& to, const arma::vec& med) {
  arma::uword q = v.n_elem;
  arma::uword p = (-1 + std::sqrt(1 + 24 * q)) / 6;
  arma::mat phi = arma::mat(v.subvec(0, p * p - 1)).reshape(p, p);
  arma::vec sigma_vech = v.subvec(p * p, q - 1);
  arma::mat sigma(p, p, arma::fill::zeros);
  arma::uword index = 0;
  for (arma::uword j = 0; j < p; ++j) {
    for (arma::uword i = j; i < p; ++i) {
        sigma(i, j) = sigma_vech[index];
        if (i != j) {
            sigma(j, i) = sigma_vech[index];
        }
        index++;
    }
  }
  arma::mat d = arma::eye(p, p);
  for (arma::uword i = 0; i < med.n_elem; ++i) {
    d(med[i] - 1, med[i] - 1) = 0;
  }
  arma::mat total = arma::expmat(delta_t * phi);
  arma::mat cov_eta;
  arma::sylvester(cov_eta, phi, phi.t(), sigma);
  arma::vec sqrt_diag = arma::sqrt(cov_eta.diag());
  arma::mat total_std = total;
  for (size_t i = 0; i < total.n_rows; i++) {
    for (size_t j = 0; j < total.n_cols; j++) {
      total_std(i, j) *= sqrt_diag(j) / sqrt_diag(i);
    }
  }
  double total_dbl = total_std(to - 1, from - 1);
  arma::mat direct = arma::expmat(delta_t * d * phi * d);
  arma::mat direct_std = direct;
  for (size_t i = 0; i < direct.n_rows; i++) {
    for (size_t j = 0; j < direct.n_cols; j++) {
      direct_std(i, j) *= sqrt_diag(j) / sqrt_diag(i);
    }
  }
  double direct_dbl = direct_std(to - 1, from - 1);
  double indirect_dbl = total_dbl - direct_dbl;
  Rcpp::NumericVector output(3);
  output(0) = total_dbl;
  output(1) = direct_dbl;
  output(2) = indirect_dbl;
  return output;
}
