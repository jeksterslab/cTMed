// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-exp-cov.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.ExpCov)]]
arma::mat ExpCov(const arma::mat& phi, const arma::mat& sigma,
                 const double& delta_t) {
  arma::mat I = arma::eye(phi.n_rows, phi.n_cols);
  arma::mat J = arma::eye(phi.n_rows * phi.n_cols, phi.n_rows * phi.n_cols);
  arma::mat beta = arma::expmat(delta_t * phi);
  arma::mat phi_hashtag = arma::kron(phi, I) + arma::kron(I, phi);
  arma::vec sigma_vec = arma::vectorise(sigma);
  arma::vec psi_vec = arma::inv(phi_hashtag) *
                      (arma::expmat(phi_hashtag * delta_t) - J) * sigma_vec;
  return arma::reshape(arma::inv(J - arma::kron(beta, beta)) * psi_vec,
                       phi.n_rows, phi.n_cols);
}
