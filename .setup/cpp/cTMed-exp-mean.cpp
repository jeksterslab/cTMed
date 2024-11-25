// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-exp-mean.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.ExpMean)]]
Rcpp::NumericVector ExpMean(const arma::mat& phi, const arma::vec& iota,
                            const double& delta_t) {
  if (arma::all(iota == 0)) {
    return Rcpp::NumericVector(phi.n_rows, 0.0);
  }
  arma::mat I = arma::eye(phi.n_rows, phi.n_cols);
  arma::mat beta = arma::expmat(delta_t * phi);
  arma::vec alpha = arma::solve(phi, (beta - I) * iota);
  arma::vec mu = arma::solve((I - beta), alpha);
  Rcpp::NumericVector output(mu.begin(), mu.end());
  return output;
}
