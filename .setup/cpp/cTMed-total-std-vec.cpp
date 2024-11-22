// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-total-std-vec.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.TotalStdVec)]]
arma::vec TotalStdVec(const arma::vec& v, const double& delta_t) {
  arma::uword q = v.size();
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
  arma::mat I = arma::eye(p, p);
  arma::mat J = arma::eye(p * p, p * p);
  arma::mat total = arma::expmat(delta_t * phi);
  arma::mat phi_hashtag = arma::kron(phi, I) + arma::kron(I, phi);
  arma::vec sigma_vec = arma::vectorise(sigma);
  arma::vec psi_vec = arma::inv(phi_hashtag) *
                      (arma::expmat(phi_hashtag * delta_t) - J) * sigma_vec;
  arma::mat total_cov =
      arma::reshape(arma::inv(J - arma::kron(total, total)) * psi_vec, p, p);
  arma::mat sd_row = arma::diagmat(arma::sqrt(total_cov.diag()));
  arma::mat sd_col_inv = arma::diagmat(1.0 / arma::sqrt(total_cov.diag()));
  arma::mat total_std = sd_row * total * sd_col_inv;
  return arma::vectorise(total_std);
}
