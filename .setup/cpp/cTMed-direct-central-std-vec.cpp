// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-direct-central-std-vec.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.DirectCentralStdVec)]]
arma::vec DirectCentralStdVec(const arma::vec& v, const double& delta_t,
                              const bool& sigma_diag = false) {
  arma::uword q = v.n_elem;
  arma::uword p = 0;
  if (sigma_diag) {
    double p_dbl = (-1.0 + std::sqrt(1.0 + 4.0 * q)) / 2.0;
    p = static_cast<arma::uword>(std::round(p_dbl));
    if (p * p + p != q) {
      Rcpp::stop(
          "When sigma_diag = TRUE, v must contain vec(phi) and diag(sigma).");
    }
  } else {
    double p_dbl = (-1.0 + std::sqrt(1.0 + 24.0 * q)) / 6.0;
    p = static_cast<arma::uword>(std::round(p_dbl));
    if (p * p + p * (p + 1) / 2 != q) {
      Rcpp::stop(
          "When sigma_diag = FALSE, v must contain vec(phi) and vech(sigma).");
    }
  }
  arma::mat phi = arma::reshape(v.subvec(0, p * p - 1), p, p);
  arma::mat sigma(p, p, arma::fill::zeros);
  if (sigma_diag) {
    sigma.diag() = v.subvec(p * p, q - 1);
  } else {
    arma::vec sigma_vech = v.subvec(p * p, q - 1);
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
  }
  arma::mat cov_eta;
  arma::sylvester(cov_eta, phi, phi.t(), sigma);
  arma::vec sqrt_diag = arma::sqrt(cov_eta.diag());
  arma::mat direct(p, p, arma::fill::none);
  arma::mat d = arma::eye(p, p);
  arma::vec output(p, arma::fill::zeros);
  for (arma::uword m = 0; m < p; ++m) {
    d = arma::eye(p, p);
    d(m, m) = 0;
    direct = arma::expmat(delta_t * d * phi * d);
    for (arma::uword j = 0; j < p; ++j) {
      for (arma::uword i = 0; i < p; ++i) {
        if (!(m == i || m == j || i == j)) {
          output(m) += direct(i, j) * sqrt_diag(j) / sqrt_diag(i);
        }
      }
    }
  }
  return output;
}
