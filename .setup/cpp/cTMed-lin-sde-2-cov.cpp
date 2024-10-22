// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-r-sq.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

arma::mat.LinSDE2Cov(const double delta_t, const arma::vec& iota,
                     const arma::mat& phi, const arma::mat& sigma_l) {
  int p = iota.n_elem;
  arma::mat I = arma::eye(p, p);
  arma::mat beta = arma::expmat(phi * delta_t);
  arma::vec alpha = arma::vec(p);
  if (iota.is_zero()) {
    alpha = iota;
  } else {
    alpha = arma::inv(phi) * (beta - I) * iota;
  }
  arma::mat psi_l = arma::mat(p, p);
  if (sigma_l.is_zero()) {
    arma::mat cov_eta =
        arma::reshape(arma::inv(arma::kron(I - beta, beta)), p, p);
    psi_l = sigma_l;
  } else {
    arma::mat J = arma::eye(p * p, p * p);
    arma::mat phi_hashtag = arma::kron(phi, I) + arma::kron(I, phi);
    arma::vec sigma_vec = arma::vectorise(sigma_l * sigma_l.t());
    arma::vec psi_vec = arma::inv(phi_hashtag) *
                        (arma::expmat(phi_hashtag * delta_t) - J) * sigma_vec;
    psi_l = arma::chol(arma::reshape(psi_vec, p, p), "lower");
    arma::mat cov_eta =
        arma::reshape(arma::inv(arma::kron(I - beta, beta)) * psi_vec, p, p);
  }
  // output
  return Rcpp::List::create(
      Rcpp::Named("alpha") = alpha, Rcpp::Named("beta") = beta,
      Rcpp::Named("psi_l") = psi_l, Rcpp::Named("cov_eta") = cov_eta);
}
